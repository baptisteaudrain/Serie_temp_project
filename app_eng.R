rm(list = ls())

# ==============================================================================
# SHINY APPLICATION: ADVANCED MONITORING (ENGLISH VERSION)
# ==============================================================================
library(shiny)
library(leaflet)
library(dplyr)
library(htmltools)
library(DT)
library(bslib)
library(lubridate)
library(jsonlite)

# ==============================================================================
# 1. CONFIGURATION & SAFETY
# ==============================================================================
options(shiny.port = 4000)

# --- STRICT THRESHOLDS DEFINITION ---
THRESHOLDS <- list(
  "PM2.5" = 25, 
  "NO2"   = 200,
  "O3"    = 120 
)

# --- GEOJSON SAFETY ---
url_remote_geo <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions/occitanie/region-occitanie.geojson"
geo_occitanie <- NULL
try({
  if(file.exists("occitanie.geojson")) {
    geo_occitanie <- fromJSON("occitanie.geojson")
  } else {
    geo_occitanie <- fromJSON(url_remote_geo)
  }
}, silent = TRUE)

# --- DATA LOADING FUNCTION ---
load_data <- function(filename) {
  empty_df <- data.frame(
    Station = character(), Polluant = character(), Typologie = character(),
    Lat = double(), Lon = double(), Heure_Ref = as.POSIXct(character()),
    Echeance_H = integer(), Date_Prevue = as.POSIXct(character()),
    Pred_Mean = double(), Pred_Max_80 = double(), Pred_Max_95 = double(),
    Statut = character(), Heure_Affiche = character(), stringsAsFactors = FALSE
  )
  
  if(!file.exists(filename)) return(empty_df)
  
  data <- tryCatch(read.csv(filename, stringsAsFactors = FALSE), error = function(e) NULL)
  
  if(is.null(data) || nrow(data) == 0) return(empty_df)
  
  return(data %>% 
           mutate(Date_Prevue = ymd_hms(Date_Prevue, quiet = TRUE), 
                  Heure_Ref   = ymd_hms(Heure_Ref, quiet = TRUE),
                  Lat = as.numeric(Lat), Lon = as.numeric(Lon)) %>%
           filter(!is.na(Date_Prevue) & !is.na(Lat) & !is.na(Lon)) %>%
           mutate(Heure_Affiche = format(Date_Prevue, "%H:00")))
}

# --- LOAD DATASETS ---
data_all <- list(
  PM2.5 = list(peri = load_data("data_shiny_pm25_periurbain.csv"), 
               urb  = load_data("data_shiny_pm25_urbain.csv"),
               rur  = load_data("data_shiny_pm25_rural.csv")),
  NO2   = list(peri = load_data("data_shiny_no2_periurbain.csv"), 
               urb  = load_data("data_shiny_no2_urbain.csv"), 
               rur  = load_data("data_shiny_no2_rural.csv")),
  O3    = list(peri = load_data("data_shiny_o3_periurbain.csv"), 
               urb  = load_data("data_shiny_o3_urbain.csv"), 
               rur  = load_data("data_shiny_o3_rural.csv"))
)

# Time Machine Function
get_last_obs_time <- function() {
  all_dfs <- unlist(unname(data_all), recursive = FALSE)
  last_times <- sapply(all_dfs, function(df) if(nrow(df) > 0) max(df$Heure_Ref) else as.POSIXct(NA))
  max_time <- max(as.POSIXct(last_times, origin="1970-01-01"), na.rm = TRUE)
  if(is.infinite(max_time)) return(Sys.time())
  return(max_time)
}

# Formatting date for English display
REF_TIME_STR <- ymd_hms("2026-01-06 12:00:00")

# ==============================================================================
# 2. USER INTERFACE (ENGLISH)
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  
  div(class = "bg-primary text-white p-3 mb-3 rounded",
      h2(style = "margin: 0;", "🌫️ Air Quality Monitor - Occitanie"), 
      p(style = "margin: 0; opacity: 0.8;", 
        paste0("Forecasts generated from observations on: ", REF_TIME_STR))
  ),
  
  sidebarLayout(
    sidebarPanel(
      # BLOCK 1: DATA SELECTION
      div(style="background:#f8f9fa; padding:15px; border-left:5px solid #2c3e50; margin-bottom:15px;",
          h4("1. Data Selection"),
          radioButtons("map_polluant", "Pollutant:", 
                       choices = list("Particles (PM2.5)" = "PM2.5", 
                                      "Nitrogen Dioxide (NO2)" = "NO2", 
                                      "Ozone (O3)" = "O3")),
          checkboxGroupInput("select_typo", "Zones:",
                             choices = list("Urban" = "urb", "Peri-urban" = "peri", "Rural" = "rur"),
                             selected = c("peri"))
      ),
      
      # BLOCK 2: TABLE FILTER
      div(style="background:#eef2f3; padding:15px; border-left:5px solid #16a085; margin-bottom:15px;",
          h4("2. Table Filter"),
          p(style="font-size:0.9em; color:#7f8c8d;", "Select one or more stations to filter the summary table."),
          # Dropdown empty at start, filled by server
          selectizeInput("filter_stations", "Select stations:", 
                         choices = NULL, 
                         multiple = TRUE, 
                         options = list(placeholder = 'All stations...'))
      ),
      
      # BLOCK 3: NAVIGATION
      div(style="background:#f8f9fa; padding:15px; border-left:5px solid #2c3e50; margin-bottom:15px;",
          h4("3. Map Navigation"),
          p("Slide to view forecast evolution."),
          sliderInput("select_h", "Horizon:", min = 1, max = 48, value = 1, post = "h"),
          uiOutput("current_time_display")
      ),
      
      # DYNAMIC LEGEND
      uiOutput("dynamic_legend"),
      width = 3
    ),
    
    mainPanel(
      leafletOutput("maCarte", height = "50vh"),
      br(),
      
      div(class = "card",
          div(class = "card-header bg-secondary text-white", h4(style="margin:0;", "📋 Alert Summary (48h)")),
          div(class = "card-body",
              p("This table groups time slots where an exceedance risk is detected for the selected stations."),
              DTOutput("tab_alertes_grouped")
          )
      ),
      width = 9
    )
  )
)

# ==============================================================================
# 3. SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # --- MERGE AND RECALCULATE STATUS (ENGLISH) ---
  filtered_full_data <- reactive({
    req(input$map_polluant, input$select_typo)
    
    dfs <- data_all[[input$map_polluant]][input$select_typo]
    df_combined <- bind_rows(dfs)
    
    if(nrow(df_combined) == 0) return(df_combined)
    
    limit_val <- THRESHOLDS[[input$map_polluant]]
    
    # Strict recalculation with English Labels
    df_recalc <- df_combined %>%
      mutate(Statut = case_when(
        Pred_Mean > limit_val ~ "HIGH ALERT",
        Pred_Max_80 > limit_val ~ "MEDIUM ALERT",
        Pred_Max_95 > limit_val ~ "POSSIBLE ALERT",
        TRUE ~ "Normal"
      ))
    
    return(df_recalc)
  })
  
  # --- OBSERVER TO UPDATE STATION LIST ---
  observe({
    df <- filtered_full_data()
    
    if(nrow(df) > 0) {
      stations_dispo <- sort(unique(df$Station))
      updateSelectizeInput(session, "filter_stations", 
                           choices = stations_dispo, 
                           selected = NULL, 
                           server = TRUE) 
    } else {
      updateSelectizeInput(session, "filter_stations", choices = NULL, selected = NULL)
    }
  })
  
  # --- DYNAMIC LEGEND (ENGLISH) ---
  output$dynamic_legend <- renderUI({
    curr_pol <- input$map_polluant
    limit_val <- THRESHOLDS[[curr_pol]]
    
    div(style="background:#fff; padding:15px; border:1px solid #ddd; border-radius:4px;",
        h5("Legend & Thresholds"),
        div(style="background:#fee; color:#c0392b; padding:5px; border-radius:3px; margin-bottom:10px; font-weight:bold; text-align:center;",
            paste(curr_pol, "Threshold:", limit_val, "µg/m³")
        ),
        tags$div(tags$span(style="color:red;font-size:1.2em;", "●"), " PM2.5"),
        tags$div(tags$span(style="color:blue;font-size:1.2em;", "●"), " NO2"),
        tags$div(tags$span(style="color:orange;font-size:1.2em;", "●"), " O3"),
        hr(),
        h5("Risk Levels"),
        tags$span(class="badge", style="background-color: #e74c3c;", "HIGH"), " Mean > Threshold", br(),
        tags$span(class="badge", style="background-color: #f39c12;", "MEDIUM"), " Risk 80% > Threshold", br(),
        tags$span(class="badge text-dark", style="background-color: #f1c40f;", "POSSIBLE"), " Risk 95% > Threshold"
    )
  })
  
  # --- MAP AND MARKERS ---
  data_map_h <- reactive({
    df <- filtered_full_data()
    if(nrow(df) == 0) return(NULL)
    df %>% filter(Echeance_H == input$select_h)
  })
  
  output$maCarte <- renderLeaflet({
    leaf <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      setView(lng = 2.5, lat = 43.8, zoom = 7)
    
    if(!is.null(geo_occitanie)) {
      leaf <- leaf %>% addGeoJSON(geojson = geo_occitanie, weight = 1, color = "#95a5a6", fill = FALSE)
    }
    leaf
  })
  
  observe({
    df <- data_map_h()
    proxy <- leafletProxy("maCarte")
    proxy %>% clearMarkers()
    
    if(!is.null(df) && is.data.frame(df) && nrow(df) > 0) {
      col_pal <- switch(input$map_polluant, "PM2.5"="red", "NO2"="blue", "O3"="orange")
      
      popups <- paste0(
        "<div style='font-family:sans-serif;'>",
        "<h5 style='margin:0;color:#2c3e50;'>", df$Station, "</h5>",
        "<span style='font-size:0.8em;color:#7f8c8d;'>", df$Typologie, "</span><hr style='margin:5px 0;'>",
        "<b>Forecast: </b>", format(df$Date_Prevue, "%H:00"), "<br>",
        "<b>Concentration: </b>", round(df$Pred_Mean, 1), " µg/m³<br>",
        "<b>Status: </b>", 
        ifelse(df$Statut == "Normal", 
               "<span style='color:green;font-weight:bold;'>Normal</span>",
               paste0("<span style='color:red;font-weight:bold;'>", df$Statut, "</span>")),
        "</div>"
      )
      
      try({
        proxy %>% addAwesomeMarkers(
          data = df, 
          lng = ~Lon, lat = ~Lat,
          icon = awesomeIcons(icon = 'ios-analytics', library = 'ion', markerColor = col_pal),
          popup = popups,
          label = ~paste0(Station, ": ", round(Pred_Mean, 1), " µg/m³")
        )
      }, silent = TRUE)
    }
  })
  
  output$current_time_display <- renderUI({
    df <- data_map_h()
    # Date format changed to Day/Month at Hour:00
    date_txt <- if(!is.null(df)) format(df$Date_Prevue[1], "%d/%m at %H:00") else "--h--"
    h4(class = "text-center text-primary", style="font-weight:bold;", date_txt)
  })
  
  # --- GROUPED TABLE (ENGLISH) ---
  output$tab_alertes_grouped <- renderDT({
    df <- filtered_full_data()
    
    if(nrow(df) == 0) return(datatable(data.frame(Message = "Data unavailable"), options = list(dom='t'), rownames=F))
    
    # 1. STATION FILTER
    if (!is.null(input$filter_stations) && length(input$filter_stations) > 0) {
      df <- df %>% filter(Station %in% input$filter_stations)
    }
    
    # 2. FILTER ALERTS
    df_alert <- df %>% 
      filter(grepl("ALERT", Statut)) %>% # Matches HIGH, MEDIUM, POSSIBLE ALERT
      arrange(Station, Date_Prevue)
    
    if(nrow(df_alert) == 0) return(datatable(data.frame(Message = "✅ No alerts for current selection."), options = list(dom='t'), rownames=F))
    
    # 3. GROUPING ALGORITHM
    df_grouped <- df_alert %>%
      group_by(Station, Statut, Typologie) %>%
      mutate(
        diff_hours = as.numeric(difftime(Date_Prevue, lag(Date_Prevue, default = first(Date_Prevue)), units = "hours")),
        new_group = cumsum(diff_hours > 1)
      ) %>%
      group_by(Station, Statut, Typologie, new_group) %>%
      summarise(
        Start = min(Date_Prevue),
        End = max(Date_Prevue),
        Max_Peak = max(Pred_Mean),
        .groups = 'drop'
      ) %>%
      mutate(
        Time_Slot = paste0("From ", format(Start, "%d/%m %Hh"), " to ", format(End, "%Hh"))
      ) %>%
      select(Station, Zone = Typologie, `Risk Level` = Statut, `Time Slot` = Time_Slot, `Peak (µg/m³)` = Max_Peak) %>%
      arrange(desc(`Peak (µg/m³)`))
    
    datatable(df_grouped,
              options = list(pageLength = 10, dom = 'tp'), # Removed French language URL
              rownames = FALSE) %>%
      formatStyle('Risk Level', 
                  backgroundColor = styleEqual(c("HIGH ALERT", "MEDIUM ALERT", "POSSIBLE ALERT"), 
                                               c("#e74c3c", "#f39c12", "#f1c40f")),
                  color = 'white', fontWeight = 'bold')
  })
}

shinyApp(ui, server)
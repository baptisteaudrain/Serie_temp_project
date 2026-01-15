rm(list = ls())

# ==============================================================================
# APPLICATION SHINY : SURVEILLANCE AVANCÉE (FILTRE STATION AJOUTÉ)
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
# 1. CONFIGURATION & SÉCURITÉS
# ==============================================================================

# --- DÉFINITION STRICTE DES SEUILS ---
THRESHOLDS <- list(
  "PM2.5" = 25, 
  "NO2"   = 200,
  "O3"    = 120 
)

# --- DOUBLE SÉCURITÉ GEOJSON ---
url_remote_geo <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions/occitanie/region-occitanie.geojson"
geo_occitanie <- NULL
try({
  if(file.exists("occitanie.geojson")) {
    geo_occitanie <- fromJSON("occitanie.geojson")
  } else {
    geo_occitanie <- fromJSON(url_remote_geo)
  }
}, silent = TRUE)

# --- FONCTION DE CHARGEMENT SÉCURISÉE ---
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

# --- CHARGEMENT DES DONNÉES ---
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

# Fonction Time Machine
get_last_obs_time <- function() {
  all_dfs <- unlist(unname(data_all), recursive = FALSE)
  last_times <- sapply(all_dfs, function(df) if(nrow(df) > 0) max(df$Heure_Ref) else as.POSIXct(NA))
  max_time <- max(as.POSIXct(last_times, origin="1970-01-01"), na.rm = TRUE)
  if(is.infinite(max_time)) return(Sys.time())
  return(max_time)
}
REF_TIME_STR <- format(get_last_obs_time(), "%d/%m/%Y à %H:00")

# ==============================================================================
# 2. INTERFACE UTILISATEUR
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  
  div(class = "bg-primary text-white p-3 mb-3 rounded",
      # REMPLACEZ LE SOLEIL ICI :
      h2(style = "margin: 0;", "🌫️ Météo de l'Air - Occitanie"), 
      p(style = "margin: 0; opacity: 0.8;", 
        paste0("Prévisions générées à partir des observations du : ", REF_TIME_STR))
  ),
  
  sidebarLayout(
    sidebarPanel(
      # BLOC 1 : SÉLECTION GLOBALE
      div(style="background:#f8f9fa; padding:15px; border-left:5px solid #2c3e50; margin-bottom:15px;",
          h4("1. Sélection Données"),
          radioButtons("map_polluant", "Polluant :", 
                       choices = list("Particules (PM2.5)" = "PM2.5", 
                                      "Dioxyde d'Azote (NO2)" = "NO2", 
                                      "Ozone (O3)" = "O3")),
          checkboxGroupInput("select_typo", "Zones :",
                             choices = list("Urbaines" = "urb", "Périurbaines" = "peri", "Rurales" = "rur"),
                             selected = c("peri"))
      ),
      
      # BLOC 2 : FILTRE TABLEAU (NOUVEAU)
      div(style="background:#eef2f3; padding:15px; border-left:5px solid #16a085; margin-bottom:15px;",
          h4("2. Filtre Tableau"),
          p(style="font-size:0.9em; color:#7f8c8d;", "Sélectionnez une ou plusieurs stations pour filtrer le tableau récapitulatif."),
          # Liste déroulante vide au départ, remplie par le serveur
          selectizeInput("filter_stations", "Choisir les stations :", 
                         choices = NULL, 
                         multiple = TRUE, 
                         options = list(placeholder = 'Toutes les stations...'))
      ),
      
      # BLOC 3 : NAVIGATION
      div(style="background:#f8f9fa; padding:15px; border-left:5px solid #2c3e50; margin-bottom:15px;",
          h4("3. Navigation Carte"),
          p("Glissez pour voir l'évolution."),
          sliderInput("select_h", "Échéance :", min = 1, max = 48, value = 1, post = "h"),
          uiOutput("current_time_display")
      ),
      
      # LÉGENDE DYNAMIQUE
      uiOutput("dynamic_legend"),
      width = 3
    ),
    
    mainPanel(
      leafletOutput("maCarte", height = "50vh"),
      br(),
      
      div(class = "card",
          div(class = "card-header bg-secondary text-white", h4(style="margin:0;", "📋 Synthèse des Alertes (48h)")),
          div(class = "card-body",
              p("Ce tableau regroupe les plages horaires où un risque de dépassement est détecté pour les stations sélectionnées."),
              DTOutput("tab_alertes_grouped")
          )
      ),
      width = 9
    )
  )
)

# ==============================================================================
# 3. LOGIQUE SERVEUR
# ==============================================================================
server <- function(input, output, session) {
  
  # --- FUSION ET RECALCUL DES STATUTS ---
  filtered_full_data <- reactive({
    req(input$map_polluant, input$select_typo)
    
    dfs <- data_all[[input$map_polluant]][input$select_typo]
    df_combined <- bind_rows(dfs)
    
    if(nrow(df_combined) == 0) return(df_combined)
    
    limit_val <- THRESHOLDS[[input$map_polluant]]
    
    # Recalcul strict
    df_recalc <- df_combined %>%
      mutate(Statut = case_when(
        Pred_Mean > limit_val ~ "ALERTE HAUTE",
        Pred_Max_80 > limit_val ~ "ALERTE MOYENNE",
        Pred_Max_95 > limit_val ~ "ALERTE POSSIBLE",
        TRUE ~ "Normal"
      ))
    
    return(df_recalc)
  })
  
  # --- OBSERVATEUR POUR METTRE À JOUR LA LISTE DES STATIONS ---
  # Se déclenche quand les données changent (changement de polluant ou de zone)
  observe({
    df <- filtered_full_data()
    
    if(nrow(df) > 0) {
      # On récupère la liste unique des stations disponibles dans la sélection actuelle
      stations_dispo <- sort(unique(df$Station))
      
      # On met à jour l'input. "selected = NULL" signifie qu'au début, rien n'est sélectionné (donc tout s'affiche)
      updateSelectizeInput(session, "filter_stations", 
                           choices = stations_dispo, 
                           selected = NULL, 
                           server = TRUE) 
    } else {
      updateSelectizeInput(session, "filter_stations", choices = NULL, selected = NULL)
    }
  })
  
  # --- LÉGENDE DYNAMIQUE ---
  output$dynamic_legend <- renderUI({
    curr_pol <- input$map_polluant
    limit_val <- THRESHOLDS[[curr_pol]]
    
    div(style="background:#fff; padding:15px; border:1px solid #ddd; border-radius:4px;",
        h5("Légende & Seuils"),
        div(style="background:#fee; color:#c0392b; padding:5px; border-radius:3px; margin-bottom:10px; font-weight:bold; text-align:center;",
            paste("Seuil", curr_pol, ":", limit_val, "µg/m³")
        ),
        tags$div(tags$span(style="color:red;font-size:1.2em;", "●"), " PM2.5"),
        tags$div(tags$span(style="color:blue;font-size:1.2em;", "●"), " NO2"),
        tags$div(tags$span(style="color:orange;font-size:1.2em;", "●"), " O3"),
        hr(),
        h5("Niveaux de Risque"),
        tags$span(class="badge", style="background-color: #e74c3c;", "HAUTE"), " Moyenne > Seuil", br(),
        tags$span(class="badge", style="background-color: #f39c12;", "MOYENNE"), " Risque 80% > Seuil", br(),
        tags$span(class="badge text-dark", style="background-color: #f1c40f;", "POSSIBLE"), " Risque 95% > Seuil"
    )
  })
  
  # --- CARTE ET MARQUEURS ---
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
        "<b>Prévision : </b>", format(df$Date_Prevue, "%H:00"), "<br>",
        "<b>Concentration : </b>", round(df$Pred_Mean, 1), " µg/m³<br>",
        "<b>Statut : </b>", 
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
    date_txt <- if(!is.null(df)) format(df$Date_Prevue[1], "%d/%m à %H:00") else "--h--"
    h4(class = "text-center text-primary", style="font-weight:bold;", date_txt)
  })
  
  # --- TABLEAU REGROUPÉ AVEC FILTRE STATION ---
  output$tab_alertes_grouped <- renderDT({
    df <- filtered_full_data()
    
    if(nrow(df) == 0) return(datatable(data.frame(Message = "Données indisponibles"), options = list(dom='t'), rownames=F))
    
    # 1. FILTRAGE PAR STATION (Si l'utilisateur a sélectionné quelque chose)
    # Si input$filter_stations est NULL, on garde tout. Sinon, on filtre.
    if (!is.null(input$filter_stations) && length(input$filter_stations) > 0) {
      df <- df %>% filter(Station %in% input$filter_stations)
    }
    
    # 2. Filtrage des alertes
    df_alert <- df %>% 
      filter(grepl("ALERTE", Statut)) %>%
      arrange(Station, Date_Prevue)
    
    if(nrow(df_alert) == 0) return(datatable(data.frame(Message = "✅ Aucune alerte pour la sélection."), options = list(dom='t'), rownames=F))
    
    # 3. Algorithme de regroupement
    df_grouped <- df_alert %>%
      group_by(Station, Statut, Typologie) %>%
      mutate(
        diff_hours = as.numeric(difftime(Date_Prevue, lag(Date_Prevue, default = first(Date_Prevue)), units = "hours")),
        new_group = cumsum(diff_hours > 1)
      ) %>%
      group_by(Station, Statut, Typologie, new_group) %>%
      summarise(
        Debut = min(Date_Prevue),
        Fin = max(Date_Prevue),
        Pic_Max = max(Pred_Mean),
        .groups = 'drop'
      ) %>%
      mutate(
        Plage_Horaire = paste0("De ", format(Debut, "%d/%m %Hh"), " à ", format(Fin, "%Hh"))
      ) %>%
      select(Station, Zone = Typologie, `Niveau Risque` = Statut, `Plage Horaire` = Plage_Horaire, `Pic (µg/m³)` = Pic_Max) %>%
      arrange(desc(`Pic (µg/m³)`))
    
    datatable(df_grouped,
              options = list(pageLength = 10, dom = 'tp', 
                             language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')),
              rownames = FALSE) %>%
      formatStyle('Niveau Risque', 
                  backgroundColor = styleEqual(c("ALERTE HAUTE", "ALERTE MOYENNE", "ALERTE POSSIBLE"), 
                                               c("#e74c3c", "#f39c12", "#f1c40f")),
                  color = 'white', fontWeight = 'bold')
  })
}

shinyApp(ui, server)
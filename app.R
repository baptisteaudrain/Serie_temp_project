rm(list = ls())

# ==============================================================================
# APPLICATION SHINY : MÉTÉO DE L'AIR OCCITANIE (MULTI-POLLUANTS)
# ==============================================================================
library(shiny)
library(leaflet)
library(dplyr)
library(htmltools)
library(DT)
library(bslib)
library(lubridate)

# ==============================================================================
# 1. CHARGEMENT ET NETTOYAGE DES DONNÉES
# ==============================================================================
load_data <- function(filename) {
  if(file.exists(filename)) {
    data <- read.csv(filename, stringsAsFactors = FALSE)
    data_clean <- data %>% 
      mutate(
        Date_Prevue = ymd_hms(Date_Prevue, quiet = TRUE), 
        Heure_Ref   = ymd_hms(Heure_Ref, quiet = TRUE)
      ) %>%
      filter(!is.na(Date_Prevue)) %>%
      mutate(Heure_Affiche = format(Date_Prevue, "%H:00"))
    return(data_clean)
  } else {
    return(data.frame())
  }
}

df_pm25 <- load_data("data_shiny_pm25_periurbain.csv")
df_no2  <- load_data("data_shiny_no2.csv")
df_o3   <- load_data("data_shiny_o3_periurbain.csv")

url_occitanie <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions/occitanie/region-occitanie.geojson"

# ==============================================================================
# 2. INTERFACE UTILISATEUR (UI)
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(tags$style(HTML("
    .info-box { background-color: #f4f4f4; padding: 15px; border-left: 5px solid #2c3e50; margin-bottom: 15px; }
    .timestamp-box { background-color: #2c3e50; color: white; padding: 12px; border-radius: 5px; text-align: center; }
    .legend-item { margin-bottom: 5px; font-size: 0.9em; }
    .dot { height: 12px; width: 12px; border-radius: 50%; display: inline-block; margin-right: 8px; }
    .leaflet-container { cursor: pointer !important; }
  "))),
  
  titlePanel("Météo de l'Air - Surveillance Prédictive Occitanie"),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "info-box",
          h4("🕒 Contrôle du temps"),
          sliderInput("select_h", "Échéance (Prochaines 48h)", 
                      min = 1, max = 48, value = 1, step = 1,
                      animate = animationOptions(interval = 1000, loop = FALSE)),
          uiOutput("current_time_display")
      ),
      
      div(class = "info-box",
          h4("🌍 Carte : Polluant Actif"),
          radioButtons("map_polluant", label = NULL,
                       choices = list("PM2.5 (Particules)" = "PM2.5", 
                                      "NO2 (Trafic)" = "NO2", 
                                      "O3 (Ozone)" = "O3"), 
                       selected = "PM2.5")
      ),
      
      div(class = "info-box",
          h4("Légende Couleurs"),
          p(tags$b("Par Polluant :")),
          div(class="legend-item", tags$span(class="dot", style="background:#e74c3c;"), "PM2.5 (Rouge)"),
          div(class="legend-item", tags$span(class="dot", style="background:#3498db;"), "NO2 (Bleu)"),
          div(class="legend-item", tags$span(class="dot", style="background:#f39c12;"), "O3 (Orange)"),
          hr(),
          p(tags$b("Opacité :"), "Plus le marqueur est foncé, plus la valeur est élevée.")
      ),
      width = 3
    ),
    
    mainPanel(
      leafletOutput("maCarte", height = "55vh"),
      hr(),
      h4("🚨 Tableau de Bord des Risques"),
      accordion(
        id = "acc_polluants",
        accordion_panel("PM2.5 - Alertes", icon = icon("smog"), value = "PM2.5", DTOutput("tab_pm25")),
        accordion_panel("NO2 - Alertes", icon = icon("car"), value = "NO2", DTOutput("tab_no2")),
        accordion_panel("O3 - Alertes", icon = icon("sun"), value = "O3", DTOutput("tab_o3"))
      ),
      width = 9
    )
  )
)

# ==============================================================================
# 3. LOGIQUE SERVEUR
# ==============================================================================
server <- function(input, output, session) {
  
  # --- 1. Filtrage Réactif ---
  data_pm25_h <- reactive({ req(nrow(df_pm25) > 0); df_pm25 %>% filter(Echeance_H == input$select_h) })
  data_no2_h  <- reactive({ req(nrow(df_no2) > 0);  df_no2  %>% filter(Echeance_H == input$select_h) })
  data_o3_h   <- reactive({ req(nrow(df_o3) > 0);   df_o3   %>% filter(Echeance_H == input$select_h) })
  
  output$current_time_display <- renderUI({
    req(data_pm25_h())
    date_txt <- if(nrow(data_pm25_h()) > 0) format(data_pm25_h()$Date_Prevue[1], "%d/%m à %H:00") else "--:--"
    div(class = "timestamp-box", icon("clock"), tags$b(paste("Prévision :", date_txt)))
  })
  
  output$maCarte <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 12)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addGeoJSON(geojson = jsonlite::fromJSON(url_occitanie), weight = 2, color = "#2c3e50", fillOpacity = 0) %>%
      setView(lng = 2.5, lat = 43.8, zoom = 7.5)
  })
  
  # --- 2. Marqueurs avec couleurs par polluant ---
  observe({
    req(input$map_polluant) 
    
    # Choix du dataset et de la couleur de base
    params <- switch(input$map_polluant, 
                     "PM2.5" = list(data = data_pm25_h(), color = "red"),
                     "NO2"   = list(data = data_no2_h(),  color = "blue"),
                     "O3"    = list(data = data_o3_h(),   color = "orange"))
    
    data_to_plot <- params$data
    
    if (nrow(data_to_plot) == 0) { leafletProxy("maCarte") %>% clearMarkers(); return() }
    
    leafletProxy("maCarte", data = data_to_plot) %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        lng = ~Lon, lat = ~Lat,
        icon = awesomeIcons(
          icon = 'ios-analytics', 
          library = 'ion',
          markerColor = params$color
        ),
        popup = ~paste0(
          "<b>Station:</b> ", Station, "<br>",
          "<b>Type:</b> ", Typologie, "<br><hr>",
          "<b>Prévue (Moy):</b> ", round(Pred_Mean, 1), " µg/m³<br>",
          "<b>Risque (80%):</b> ", round(Pred_Max_80, 1), " µg/m³<br>",
          "<b>Pire cas (95%):</b> ", round(Pred_Max_95, 1), " µg/m³<br>",
          "<b>Statut:</b> ", Statut
        )
      )
  })
  
  # --- 3. Tableaux avec colonnes de risque étendues ---
  render_pollutant_table <- function(data_reactive) {
    renderDT({
      df <- data_reactive()
      if(nrow(df) == 0) return(NULL)
      
      df_alert <- df %>% filter(grepl("ALERTE", Statut))
      
      if(nrow(df_alert) == 0) {
        return(datatable(
          data.frame(Message = "✅ Qualité de l'air conforme : Aucune alerte détectée."),
          options = list(dom = 't', ordering = FALSE), 
          rownames = FALSE, colnames = ""
        ))
      }
      
      # Sélection avec les nouvelles colonnes Pred_Max_80 et 95
      df_show <- df_alert %>% 
        select(Heure = Heure_Affiche, 
               Station, 
               `Moyenne` = Pred_Mean, 
               `Risque (80%)` = Pred_Max_80, 
               `Pire Cas (95%)` = Pred_Max_95, 
               Statut)
      
      datatable(df_show, 
                options = list(dom = 't', pageLength = 5, 
                               language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')), 
                rownames = FALSE) %>%
        formatStyle('Statut', 
                    color = styleEqual(c("ALERTE HAUTE", "ALERTE MOYENNE", "ALERTE POSSIBLE"), 
                                       c("#c0392b", "#d35400", "#f39c12")),
                    fontWeight = 'bold')
    })
  }
  
  output$tab_pm25 <- render_pollutant_table(data_pm25_h)
  output$tab_no2  <- render_pollutant_table(data_no2_h)
  output$tab_o3   <- render_pollutant_table(data_o3_h)
}

shinyApp(ui, server)
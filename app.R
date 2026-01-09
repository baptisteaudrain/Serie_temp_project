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
    # Lecture du fichier
    data <- read.csv(filename, stringsAsFactors = FALSE)
    
    # Traitement robuste des dates
    data_clean <- data %>% 
      mutate(
        # 'quiet = TRUE' évite les warnings rouges dans la console si format incertain
        Date_Prevue = ymd_hms(Date_Prevue, quiet = TRUE), 
        Heure_Ref   = ymd_hms(Heure_Ref, quiet = TRUE)
      ) %>%
      # SUPPRESSION DES LIGNES INVALIDES (Dates NA)
      filter(!is.na(Date_Prevue)) %>%
      # Création de la colonne d'affichage propre (ex: "14:00")
      mutate(Heure_Affiche = format(Date_Prevue, "%H:00"))
    
    return(data_clean)
  } else {
    return(data.frame())
  }
}

# Chargement des 3 fichiers générés par les Backends
df_pm25 <- load_data("data_shiny_pm25_periurbain.csv")
df_no2  <- load_data("data_shiny_no2_periubain.csv")
df_o3   <- load_data("data_shiny_o3_periurbain.csv")

# URL GeoJSON Occitanie (Fond de carte)
url_occitanie <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions/occitanie/region-occitanie.geojson"

# ==============================================================================
# 2. INTERFACE UTILISATEUR (UI)
# ==============================================================================
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(tags$style(HTML("
    .info-box { background-color: #f4f4f4; padding: 15px; border-left: 5px solid #2c3e50; margin-bottom: 15px; }
    .timestamp-box { background-color: #2c3e50; color: white; padding: 12px; border-radius: 5px; text-align: center; }
    .dot { height: 12px; width: 12px; border-radius: 50%; display: inline-block; margin-right: 8px; }
    .leaflet-container { cursor: pointer !important; }
  "))),
  
  titlePanel("Météo de l'Air - Surveillance Prédictive Occitanie"),
  
  sidebarLayout(
    sidebarPanel(
      # --- CONTROLE TEMPOREL ---
      div(class = "info-box",
          h4("🕒 Contrôle du temps"),
          # Slider étendu à 48 heures
          sliderInput("select_h", "Échéance (Prochaines 48h)", 
                      min = 1, max = 48, value = 1, step = 1,
                      animate = animationOptions(interval = 1000, loop = FALSE)),
          uiOutput("current_time_display")
      ),
      
      # --- SÉLECTION POLLUANT (Radio Buttons pour contrôler la carte) ---
      div(class = "info-box",
          h4("🌍 Carte : Polluant Actif"),
          radioButtons("map_polluant", label = NULL,
                       choices = list("PM2.5 (Particules)" = "PM2.5", 
                                      "NO2 (Trafic)" = "NO2", 
                                      "O3 (Ozone)" = "O3"), 
                       selected = "PM2.5")
      ),
      
      # --- LÉGENDE STATIQUE ---
      div(class = "info-box",
          h4("Légende Carte"),
          div(tags$span(class="dot", style="background:purple;"), "Station Urbaine"),
          div(tags$span(class="dot", style="background:pink;"), "Station Périurbaine"),
          div(tags$span(class="dot", style="background:cyan;"), "Station Rurale"),
          hr(),
          p(tags$b("Couleur Marqueur :"), "Typologie"),
          p(tags$b("Popup :"), "Valeur prédite")
      ),
      width = 3
    ),
    
    mainPanel(
      # --- CARTE INTERACTIVE ---
      leafletOutput("maCarte", height = "55vh"),
      hr(),
      
      # --- TABLEAUX D'ALERTE (Accordéon) ---
      h4("🚨 Tableau de Bord des Risques"),
      accordion(
        id = "acc_polluants",
        
        # PANNEAU PM2.5
        accordion_panel(
          "Particules Fines (PM2.5) - Alertes Risque",
          icon = icon("smog"), 
          value = "PM2.5",
          DTOutput("tab_pm25")
        ),
        
        # PANNEAU NO2
        accordion_panel(
          "Dioxyde d'Azote (NO2) - Alertes Trafic",
          icon = icon("car"),
          value = "NO2",
          DTOutput("tab_no2")
        ),
        
        # PANNEAU O3
        accordion_panel(
          "Ozone (O3) - Alertes Photochimiques",
          icon = icon("sun"),
          value = "O3",
          DTOutput("tab_o3")
        )
      ),
      width = 9
    )
  )
)

# ==============================================================================
# 3. LOGIQUE SERVEUR (CORRIGÉE POUR GÉRER L'ABSENCE D'ALERTES)
# ==============================================================================
server <- function(input, output, session) {
  
  # --- 1. Filtrage Réactif ---
  data_pm25_h <- reactive({ req(nrow(df_pm25) > 0); df_pm25 %>% filter(Echeance_H == input$select_h) })
  data_no2_h  <- reactive({ req(nrow(df_no2) > 0);  df_no2  %>% filter(Echeance_H == input$select_h) })
  data_o3_h   <- reactive({ req(nrow(df_o3) > 0);   df_o3   %>% filter(Echeance_H == input$select_h) })
  
  # --- 2. Heure courante ---
  output$current_time_display <- renderUI({
    req(data_pm25_h())
    date_txt <- if(nrow(data_pm25_h()) > 0) format(data_pm25_h()$Date_Prevue[1], "%d/%m à %H:00") else "--:--"
    div(class = "timestamp-box", icon("clock"), tags$b(paste("Prévision :", date_txt)))
  })
  
  # --- 3. Carte ---
  output$maCarte <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 12)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addGeoJSON(geojson = jsonlite::fromJSON(url_occitanie), weight = 2, color = "#2c3e50", fillOpacity = 0) %>%
      setView(lng = 2.5, lat = 43.8, zoom = 7.5)
  })
  
  # --- 4. Marqueurs Dynamiques ---
  observe({
    req(input$map_polluant) 
    data_to_plot <- switch(input$map_polluant, "PM2.5"=data_pm25_h(), "NO2"=data_no2_h(), "O3"=data_o3_h())
    
    if (nrow(data_to_plot) == 0) { leafletProxy("maCarte") %>% clearMarkers(); return() }
    
    leafletProxy("maCarte", data = data_to_plot) %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        lng = ~Lon, lat = ~Lat,
        icon = awesomeIcons(
          icon = 'ios-analytics', library = 'ion',
          markerColor = case_when(
            grepl("Urbain", data_to_plot$Typologie, ignore.case=T) ~ "purple",
            grepl("Périurbain", data_to_plot$Typologie, ignore.case=T) ~ "pink",
            grepl("Rural", data_to_plot$Typologie, ignore.case=T) ~ "cyan",
            TRUE ~ "gray"
          )
        ),
        popup = ~paste0("<b>Station:</b> ", Station, "<br><b>Val:</b> ", round(Pred_Mean, 1), " µg/m³<br><b>Statut:</b> ", Statut)
      )
  })
  
  # --- 5. TABLEAUX INTELLIGENTS (Correction ici) ---
  render_pollutant_table <- function(data_reactive) {
    renderDT({
      df <- data_reactive()
      if(nrow(df) == 0) return(NULL)
      
      # 1. On cherche les alertes
      df_alert <- df %>% filter(grepl("ALERTE", Statut))
      
      # 2. SI AUCUNE ALERTE (C'est le cas fréquent pour NO2/O3)
      if(nrow(df_alert) == 0) {
        # On renvoie un tableau avec un message positif au lieu de rien
        return(datatable(
          data.frame(Message = paste("✅ RAS : Aucune alerte détectée pour l'heure sélectionnée.")),
          options = list(dom = 't', ordering = FALSE), 
          rownames = FALSE,
          colnames = "" # Cache le nom de la colonne
        ))
      }
      
      # 3. SINON ON AFFICHE LES ALERTES
      df_show <- df_alert %>% 
        select(Heure = Heure_Affiche, Station, Type = Typologie,
               `Valeur` = Pred_Mean, `Max 95%` = Pred_Max, Statut)
      
      datatable(df_show, 
                options = list(dom = 't', pageLength = 5, language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')), 
                rownames = FALSE) %>%
        formatStyle('Statut', color = 'red', fontWeight = 'bold')
    })
  }
  
  output$tab_pm25 <- render_pollutant_table(data_pm25_h)
  output$tab_no2  <- render_pollutant_table(data_no2_h)
  output$tab_o3   <- render_pollutant_table(data_o3_h)
}


shinyApp(ui, server)
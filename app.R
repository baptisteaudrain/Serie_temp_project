library(shiny)
library(leaflet)
library(dplyr)
library(htmltools)
library(DT)
library(bslib)
library(lubridate)

# Chargement sécurisé
fichier_data <- "data_shiny_pm25_periurbain.csv"
df_total <- if(file.exists(fichier_data)) {
  read.csv(fichier_data) %>% mutate(Date_Prevue = ymd_hms(Date_Prevue), Heure_Ref = ymd_hms(Heure_Ref))
} else { data.frame() }

url_occitanie <- "https://raw.githubusercontent.com/gregoiredavid/france-geojson/master/regions/occitanie/region-occitanie.geojson"

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(tags$style(HTML("
    .info-box { background-color: #f4f4f4; padding: 15px; border-left: 5px solid #2c3e50; margin-bottom: 15px; }
    .timestamp-box { background-color: #2c3e50; color: white; padding: 12px; border-radius: 5px; text-align: center; }
    .dot { height: 12px; width: 12px; border-radius: 50%; display: inline-block; margin-right: 8px; }
  "))),
  
  titlePanel("Météo de l'Air - Surveillance Prédictive Occitanie"),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "info-box",
          h4("🕒 Contrôle du temps"),
          sliderInput("select_h", "Échéance (Prochaines 24h)", 
                      min = 1, max = 24, value = 1, step = 1,
                      animate = animationOptions(interval = 1200)),
          uiOutput("current_time_display")
      ),
      
      div(class = "info-box",
          h4("Légende Typologies"),
          div(tags$span(class="dot", style="background:purple;"), "Urbain"),
          div(tags$span(class="dot", style="background:pink;"), "Périurbain"),
          div(tags$span(class="dot", style="background:cyan;"), "Rural"),
          hr(),
          p(tags$b("—"), " Limite Région Occitanie")
      ),
      width = 3
    ),
    
    mainPanel(
      leafletOutput("maCarte", height = "50vh"),
      hr(),
      h4("📋 Alertes par Polluant"),
      
      accordion(
        accordion_panel(
          "Particules Fines (PM2.5)",
          icon = icon("mask-ventilator"),
          DTOutput("tab_pm25")
        ),
        accordion_panel(
          "Dioxyde d'Azote (NO2)",
          icon = icon("car-side"),
          p(em("Données de prévision NO2 en cours d'intégration..."))
        ),
        accordion_panel(
          "Ozone (O3)",
          icon = icon("sun"),
          p(em("Données de prévision O3 en cours d'intégration..."))
        )
      ),
      width = 9
    )
  )
)

server <- function(input, output, session) {
  
  # Filtrage réactif
  df_h <- reactive({
    req(df_total)
    df_total %>% filter(Echeance_H == input$select_h)
  })
  
  output$current_time_display <- renderUI({
    req(df_h())
    time_target <- df_h()$Date_Prevue[1]
    div(class = "timestamp-box",
        icon("clock"), tags$b(format(time_target, " %d/%m à %H:00"))
    )
  })
  
  output$maCarte <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 6, maxZoom = 11)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addGeoJSON(geojson = jsonlite::fromJSON(url_occitanie), weight = 3, color = "black", fillOpacity = 0) %>%
      setView(lng = 2.3, lat = 43.7, zoom = 7)
  })
  
  observe({
    data <- df_h()
    req(nrow(data) > 0)
    
    # Couleurs basées sur les 3 typologies prévues dans la légende
    leafletProxy("maCarte", data = data) %>%
      clearMarkers() %>%
      addAwesomeMarkers(
        lng = ~Lon, lat = ~Lat,
        icon = awesomeIcons(
          icon = 'ios-analytics', 
          markerColor = case_when(
            data$Typologie == "Urbain" ~ "purple",
            data$Typologie == "Périurbain" ~ "pink",
            data$Typologie == "Rural" ~ "cyan",
            TRUE ~ "pink"
          )
        ),
        label = ~lapply(paste0(
          "<b>", Station, "</b><br>",
          "Échéance : ", Heure_Prediction, "<br>",
          "Prévision : <b>", Pred_PM25, " µg/m³</b>"
        ), HTML)
      )
  })
  
  # Tableau PM2.5 avec la colonne Heure de prédiction demandée
  output$tab_pm25 <- renderDT({
    req(df_h())
    data <- df_h() %>% 
      filter(Statut == "ALERTE RISQUE") %>%
      select(
        Heure = Heure_Prediction,
        Commune, 
        Station, 
        `Prévu (µg/m³)` = Pred_PM25, 
        `Risque Max` = Risque_Max
      )
    
    datatable(data, 
              options = list(dom = 't', language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')), 
              rownames = FALSE) %>%
      formatStyle('Prévu (µg/m³)', color = 'red', fontWeight = 'bold')
  })
}

shinyApp(ui, server)
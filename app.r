library(dplyr)
library(readxl)
library(shiny)
library(leaflet)
library(geobr)
library(sf)

df <- read_excel("data/dados_wgs.xlsx")

# Baixa o shape (formato geográfico) do município de São Paulo
# O código '3550308' é o código do IBGE para o município de São Paulo
shape_sp <- read_municipality(code_muni = 3550308, year = 2020)

df_l <- df %>%
  filter(!is.na(valor_total) & valor_total != "") %>%
  mutate(across(
    .cols = c(valor_total, unit, area_util, quartos, vagas, condominio,
              suites, banheiros, piscina, academia, quadra, media_bairro,
              qtd_dados_bairro, media_idh, media_gini, expectativa_vida,
              renda_percapita, dist, lat, lon),
    .fns = as.numeric
  )) %>%
  filter(!is.na(lat) & !is.na(lon) & !is.na(valor_total))

min_preco <- min(df_l$valor_total, na.rm = TRUE)
max_preco <- max(df_l$valor_total, na.rm = TRUE)

ui <- fluidPage(
  titlePanel("Apartamentos à Venda em São Paulo"),
  
  sidebarLayout(
    sidebarPanel(
        h4("Filtros"),

        fluidRow(
          column(6,
                 numericInput(inputId = "min_preco",
                              label = "Preço Mínimo (R$):",
                              value = min_preco,
                              min = min_preco,
                              max = max_preco,
                              step = 50000)
          ),
          column(6,
                 numericInput(inputId = "max_preco",
                              label = "Preço Máximo (R$):",
                              value = max_preco,
                              min = min_preco,
                              max = max_preco,
                              step = 50000)
          )
        ),
      
      sliderInput(inputId = "filtro_preco_slider",
                  label = NULL,
                  min = min_preco,
                  max = max_preco,
                  value = c(min_preco, max_preco),
                  step = 50000,
                  pre = "R$ "),
      # ideia de adicionar mais filtros aqui (área, quartos, bairro, etc.)
    ),
    
    mainPanel(
      leafletOutput(outputId = "mapa_sp", height = "85vh")
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$filtro_preco_slider, {
    updateNumericInput(session, "min_preco", value = input$filtro_preco_slider[1])
    updateNumericInput(session, "max_preco", value = input$filtro_preco_slider[2])
  })
  
  observeEvent(input$min_preco, {
    if (input$min_preco <= input$max_preco) {
      updateSliderInput(session, "filtro_preco_slider", value = c(input$min_preco, input$max_preco))
    }
  })
  
  observeEvent(input$max_preco, {
    if (input$max_preco >= input$min_preco) {
      updateSliderInput(session, "filtro_preco_slider", value = c(input$min_preco, input$max_preco))
    }
  })
  
  dados_filtrados <- reactive({
    df_l %>%
      filter(valor_total >= input$filtro_preco_slider[1] & 
             valor_total <= input$filtro_preco_slider[2])
  })
  
  # isso só executa uma vez, quando o mapa é criado
  output$mapa_sp <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      # Adiciona o polígono com o shape de São Paulo
      addPolygons(
        data = shape_sp,
        fillColor = "#66CDAA", 
        weight = 2,           
        color = "white",      
        fillOpacity = 0.3,    
        highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.5,
            bringToFront = TRUE)
      ) %>%
      setView(lng = -46.6333, lat = -23.5505, zoom = 10) # centraliza em São Paulo
  })
  
  # ATUALIZA os pontos do mapa filtrado
  observe({
    leafletProxy("mapa_sp", data = dados_filtrados()) %>%
      clearMarkers() %>%
      clearMarkerClusters() %>%
      addMarkers(
        lng = ~lon,
        lat = ~lat,
        popup = ~paste(
          "<b>Preço:</b> R$", format(valor_total, big.mark=".", decimal.mark=","), "<br>",
          "<b>Área:</b>", area_util, "m²<br>",
          "<b>Bairro:</b>", bairro, "<br>",
          "<a href='", "https://peanutbutterjellytime.net/", "' target='_blank'>Ver anúncio</a>" 
        ),
        clusterOptions = markerClusterOptions() 
      )
  })
}

shinyApp(ui = ui, server = server)
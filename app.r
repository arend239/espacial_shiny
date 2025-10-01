library(dplyr)
library(readxl)
library(shiny)
library(shinythemes)
library(leaflet)
library(geobr)
library(sf)
library(htmlwidgets)
library(DT)
library(ggplot2)
library(plotly)

df <- read_excel("data/dados_wgs.xlsx")

# Baixa os shapes (formatos geográficos)
# shape_sp <- read_municipality(code_muni = 3550308, year = 2020)
# shape_bairros <- read_neighborhood(city = "São Paulo", year = 2020)

# Carrega os shapes (formatos geográficos) a partir de arquivos locais
shape_sp <- readRDS("data/shape_sp.rds")
#shape_bairros <- readRDS("data/shape_bairros.rds")

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

# Define os valores mínimos e máximos para os filtros
min_preco <- min(df_l$valor_total, na.rm = TRUE)
max_preco <- max(df_l$valor_total, na.rm = TRUE)
min_area <- min(df_l$area_util, na.rm = TRUE)
max_area <- max(df_l$area_util, na.rm = TRUE)
min_quartos <- min(df_l$quartos, na.rm = TRUE)
max_quartos <- max(df_l$quartos, na.rm = TRUE)
min_banheiros <- min(df_l$banheiros, na.rm = TRUE)
max_banheiros <- max(df_l$banheiros, na.rm = TRUE)
min_vagas <- min(df_l$vagas, na.rm = TRUE)
max_vagas <- max(df_l$vagas, na.rm = TRUE)

ui <- navbarPage(
    "PrimeiroAndar | São Paulo",
    theme = shinytheme("cerulean"),
     tags$head(
      tags$style(HTML("body { font-family: sans-serif; }"))
    ),
    tabPanel(
      "Mapa",
      div(
        style = "position: relative;",
        leafletOutput(outputId = "mapa_sp", height = "calc(100vh - 80px)"),
        absolutePanel(
          top = 10, left = 10,
          width = 350,
          style = "background-color: rgba(255, 255, 255, 0.6); padding: 20px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.2); overflow-y: auto; max-height: 95vh;",
          h4("Filtros"),
          sliderInput(
            inputId = "filtro_preco_slider",
            label = "Preço (R$):",
            min = min_preco,
            max = max_preco,
            value = c(min_preco, max_preco),
            step = 50000,
            pre = "R$ "
          ),
          hr(),
          sliderInput("filtro_area_slider", "Área Útil (m²):", min = min_area, max = max_area, value = c(min_area, max_area)),
          sliderInput("filtro_quartos_slider", "Quartos:", min = min_quartos, max = max_quartos, value = c(min_quartos, max_quartos), step = 1),
          sliderInput("filtro_banheiros_slider", "Banheiros:", min = min_banheiros, max = max_banheiros, value = c(min_banheiros, max_banheiros), step = 1),
          #sliderInput("filtro_vagas_slider", "Vagas de Garagem:", min = min_vagas, max = max_vagas, value = c(min_vagas, max_vagas), step = 1),
          hr(),
          checkboxGroupInput(
            "filtro_comodidades",
            "Comodidades:",
            choices = c("Piscina", "Academia", "Quadra"),
            inline = TRUE
          )
        )
      )
    ),
    tabPanel(
      "Análise",
      fluidPage(
        h3("Análise Gráfica"),
        p("Gráficos dinâmicos baseados nos dados filtrados. Passe o mouse sobre os gráficos para mais detalhes."),
        fluidRow(
          column(width = 6, plotlyOutput("hist_preco")),
          column(width = 6, plotlyOutput("scatter_preco_area"))
        ),
        hr(),
        fluidRow(
          column(width = 12, plotlyOutput("boxplot_bairro"))
        ),
        hr(),
        h3("Tabela de Dados"),
        p("Tabela com os dados detalhados dos imóveis que correspondem aos filtros selecionados."),
        DT::dataTableOutput("tabela_dados")
      )
    ),
    tabPanel(
      "Sobre",
      fluidPage(
        h3("Sobre o Projeto"),
        p("Aplicativo desenvolvido por Davi Augusto, Eduardo Garcez e João Arend para apresentação de trabalho de estatística espacial na graduação em Estatística na Universidade Federal do Rio Grande do Sul."),
        p("Os dados utilizados estão disponíveis no Kaggle - " ,
        tags$a(href = "https://www.kaggle.com/datasets/jlgrego/apartamentos-venda-na-cidade-de-sao-paulo-sp/data", "Apartamentos à venda na cidade de Sao Paulo, SP"),
        "."),
        hr(),
        tags$footer(
          "Projeto da disciplina MAT02040 - Estatística Espacial, disponível no ",
          tags$a(href = "https://github.com/arend239/espacial_shiny", "Github"),
          ".",
          align = "center",
          style = "padding: 10px;"
        )
      )
    )
  )


server <- function(input, output, session) {

  dados_filtrados <- reactive({

    filtered_data <- df_l %>%
      filter(
        valor_total >= input$filtro_preco_slider[1] & valor_total <= input$filtro_preco_slider[2],
        area_util >= input$filtro_area_slider[1] & area_util <= input$filtro_area_slider[2],
        quartos >= input$filtro_quartos_slider[1] & quartos <= input$filtro_quartos_slider[2],
        banheiros >= input$filtro_banheiros_slider[1] & banheiros <= input$filtro_banheiros_slider[2]
        #vagas >= input$filtro_vagas_slider[1] & vagas <= input$filtro_vagas_slider[2]
      )

    if ("Piscina" %in% input$filtro_comodidades) {
      filtered_data <- filtered_data %>%
        filter(piscina == 1)
    }
    if ("Academia" %in% input$filtro_comodidades) {
      filtered_data <- filtered_data %>%
        filter(academia == 1)
    }
    if ("Quadra" %in% input$filtro_comodidades) {
      filtered_data <- filtered_data %>%
        filter(quadra == 1)
    }

    return(filtered_data)
  })

  # isso só executa uma vez, quando o mapa é criado
  output$mapa_sp <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      addTiles() %>%
      # Adiciona o polígono com o shape de São Paulo
      addPolygons(
        data = shape_sp,
        #fillColor = "#66CDAA",
        fillColor = "#347761",
        weight = 2,
        color = "white",
        fillOpacity = 0.3,
        group = "Município"
      ) %>%
      # addPolygons(
      #  data = shape_bairros,
      #  fillColor = "transparent",
      #  weight = 1,
      #  color = "#444",
      #  group = "Bairros",
      #  label = ~name_neighborhood
      #) %>%
      setView(lng = -46.6333, lat = -23.5505, zoom = 10) %>%
      onRender("
        function(el, x) {
          L.control.zoom({
            position: 'bottomleft'
          }).addTo(this);
        }
      ") # %>%
      #addLayersControl(
      #  overlayGroups = c("Bairros"),
      #  options = layersControlOptions(collapsed = FALSE)
      #)
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
          "<b>Preço:</b> R$", format(valor_total, big.mark = ".", decimal.mark = ","), "<br>",
          "<b>Área:</b>", area_util, "m²<br>",
          "<b>Bairro:</b>", bairro, "<br>",
          "<a href='", "https://peanutbutterjellytime.net/", "' target='_blank'>Ver anúncio</a>"
        ),
        clusterOptions = markerClusterOptions()
      )
  })

  # Renderiza a tabela de dados interativa
  output$tabela_dados <- DT::renderDataTable({
    df <- dados_filtrados() %>%
      select(bairro, valor_total, area_util, quartos, banheiros, vagas, condominio)

    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.25/i18n/Portuguese-Brasil.json')
      ),
      colnames = c("Bairro", "Preço (R$)", "Área (m²)", "Quartos", "Banheiros", "Vagas", "Condomínio (R$)")
    ) %>%
      formatCurrency(c("valor_total", "condominio"), currency = "R$", interval = 3, mark = ".", dec.mark = ",")
  })

  # Gráficos Dinâmicos
  output$hist_preco <- renderPlotly({
    p <- ggplot(dados_filtrados(), aes(x = valor_total)) +
      geom_histogram(aes(text = ..count..), fill = "#1695bb", bins = 30) +
      labs(title = "Distribuição dos Preços", x = "Preço", y = "") +
      theme_minimal() +
      scale_x_continuous(labels = scales::dollar_format(prefix = "R$"))

    ggplotly(p, tooltip = c("x", "text"))
  })

  output$scatter_preco_area <- renderPlotly({
    p <- ggplot(dados_filtrados(), aes(x = area_util, y = valor_total)) +
      geom_point(aes(text = paste("Bairro:", bairro)), color = "#1695bb", alpha = 0.6) +
      labs(title = "Preço vs. Área Útil (m²)", x = "Área Útil (m²)", y = "") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(prefix = "R$"))

    ggplotly(p, tooltip = c("x", "y", "text"))
  })

  output$boxplot_bairro <- renderPlotly({

    top_bairros <- dados_filtrados() %>%
      count(bairro, sort = TRUE) %>%
      top_n(10) %>%
      pull(bairro)

    df_plot <- dados_filtrados() %>%
      filter(bairro %in% top_bairros)

    p <- ggplot(df_plot, aes(x = reorder(bairro, valor_total, FUN = median), y = valor_total)) +
      geom_boxplot(fill = "#1695bb") +
      labs(title = "Distribuição de Preços nos 10 Bairros com Mais Anúncios", x = "", y = "Preço") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(prefix = "R$")) +
      coord_flip()

    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
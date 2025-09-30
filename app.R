# =================================================================
# PROTÓTIPO DE APLICATIVO SHINY PARA ANÁLISE DE DADOS ESPACIAIS
# TEMA: Onde está o Doguinho Caramelo?
# =================================================================

# --- 1. CARREGAR PACOTES NECESSÁRIOS ---

libs <- c(
  "shiny",
  "leaflet",
  "leaflet.extras",
  "dplyr"
)

# Instala e carrega os pacotes
for (lib in libs) {
  if (!requireNamespace(lib, quietly = TRUE)) {
      install.packages(lib)
    }
  library(lib, character.only = TRUE)
}

# =================================================================
# --- 3. INTERFACE DO USUÁRIO (UI) ---
# Aqui definimos a aparência do aplicativo
# =================================================================
ui <- fluidPage(

  # Título do App
  titlePanel("Análise Espacial: Onde estão os imóveis? 🐕"),

  # Layout com uma barra lateral para controles
  sidebarLayout(

    # Painel da barra lateral
    sidebarPanel(
      h4("Controles de Visualização"),

      # Seletor para o tipo de mapa
      selectInput(
        inputId = "tipo_mapa",
        label = "Escolha o tipo de mapa:",
        choices = c("Mapa de Pontos" = "pontos", "Mapa de Calor" = "calor"),
        selected = "pontos"
      ),

      hr(), # Linha horizontal para separar

      # Texto explicativo
      p(
        "Este app visualiza dados de imóveis para demonstrar ",
        "técnicas de visualização de dados espaciais. Use os controles acima para interagir."
      )
    ),

    # Painel principal onde o mapa será exibido
    mainPanel(
      # O mapa será renderizado aqui
      leafletOutput("mapa_espacial", height = "80vh") # 'vh' = altura relativa à janela de visualização
    )
  )
)

# =================================================================
# --- 4. LÓGICA DO SERVIDOR (Server) ---
# Aqui definimos o comportamento do aplicativo: como os dados são
# processados e como os resultados são exibidos.
# =================================================================
server <- function(input, output, session) {

  # --- Carregar os dados ---
  dados_imoveis <- reactive({
    read.csv2("data/dados_wgs.csv") %>%
      rename(latitude = lat, longitude = lon)
  })

  # --- Renderização do mapa ---
  # 'renderLeaflet' cria o mapa interativo.
  output$mapa_espacial <- renderLeaflet({

    # Pega os dados gerados reativamente
    dados <- dados_imoveis()

    # Cria o mapa base com tiles do OpenStreetMap
    mapa_base <- leaflet(data = dados) %>%
      addTiles() %>%
      # Centraliza o mapa na primeira vez que ele é carregado
      fitBounds(
        lng1 = ~min(longitude), lat1 = ~min(latitude),
        lng2 = ~max(longitude), lat2 = ~max(latitude)
      )

    # --- Lógica para alternar entre os tipos de mapa ---
    if (input$tipo_mapa == "pontos") {

      # Adiciona marcadores circulares para cada ponto
      mapa_base %>%
        addCircleMarkers(
          lng = ~longitude,
          lat = ~latitude,
          radius = 5,
          color = "#D95F02", # Uma cor "caramelo"
          stroke = FALSE,
          fillOpacity = 0.7,
          # Cria o texto que aparece ao clicar em um ponto
          popup = ~paste(
            "<b>Endereço:</b>", endereco, "<br>",
            "<b>Bairro:</b>", bairro, "<br>",
            "<b>Valor:</b>", valor_total
          )
        )

    } else if (input$tipo_mapa == "calor") {

      # Usa o pacote leaflet.extras para adicionar a camada de calor
      mapa_base %>%
        addHeatmap(
          lng = ~longitude,
          lat = ~latitude,
          blur = 20,
          max = 0.05,
          radius = 15
        )
    }
  })
}

# =================================================================
# --- 5. RODAR O APLICATIVO ---
# =================================================================
shinyApp(ui = ui, server = server)
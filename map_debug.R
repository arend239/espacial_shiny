# Load necessary libraries
library(leaflet)
library(dplyr)
library(htmlwidgets)

# Load data
dados <- read.csv("data/dados_wgs.csv", sep = ";")

# Rename and convert columns
dados <- dados %>%
  rename(latitude = lat, longitude = lon) %>%
  mutate(latitude = as.numeric(latitude),
         longitude = as.numeric(longitude))

# Create leaflet map
mapa <- leaflet(data = dados) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,
    color = "#D95F02",
    stroke = FALSE,
    fillOpacity = 0.7,
    popup = ~paste(
      "<b>Endereço:</b>", endereco, "<br>",
      "<b>Bairro:</b>", bairro, "<br>",
      "<b>Valor:</b>", valor_total
    )
  )

# Save map to HTML file
saveWidget(mapa, file="mapa_debug.html")

# Print a message to the console
print("Map saved to mapa_debug.html")

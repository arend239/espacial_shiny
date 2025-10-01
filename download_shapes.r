library(geobr)
library(sf)

# Este script baixa os dados geográficos e os salva localmente.
# Execute este script APENAS UMA VEZ ou quando quiser atualizar os dados.

# 1. Baixar o shape do município de São Paulo
cat("Baixando shape do município de São Paulo...\n")
shape_sp <- read_municipality(code_muni = 3550308, year = 2020)
saveRDS(shape_sp, "data/shape_sp.rds")
cat("Salvo em data/shape_sp.rds\n")

# 2. Baixar os shapes dos bairros de São Paulo
cat("Baixando shapes dos bairros de São Paulo...\n")
shape_bairros <- read_neighborhood(city = "São Paulo", year = 2020)
saveRDS(shape_bairros, "data/shape_bairros.rds")
cat("Salvo em data/shape_bairros.rds\n")

cat("Download e salvamento concluídos.\n")


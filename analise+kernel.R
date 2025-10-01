# =============================
# Análise espacial + Kernel
# =============================

# no início do script
library(sf)
library(sp)
library(spatstat.geom)
library(spatstat.explore)
library(spatstat.random)
library(leaflet)
library(scales)   


# --- função auxiliar: converter pontos lat/lon em coordenadas planas (UTM 23S)
project_points_to_planar <- function(df_points, shape_sf) {
  pts_sf <- st_as_sf(df_points, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  
  # força transformação para UTM 23S (adequado para SP) - unidades em metros
  target_crs <- 31983 
  
  pts_tr <- st_transform(pts_sf, target_crs)
  shape_tr <- st_transform(shape_sf, target_crs)
  coords <- st_coordinates(pts_tr)
  
  list(pts_sf = pts_tr, shape_sf = shape_tr, coords = coords, crs = target_crs)
}

# --- construir objeto ppp limitado ao polígono do município (usando polígono real)
make_ppp_from_df <- function(df_filtered, shape_poly) {
  conv <- project_points_to_planar(df_filtered, shape_poly)
  coords <- conv$coords
  
  # cria janela diretamente de um objeto sf
  win <- as.owin(conv$shape_sf)   # agora funciona direto com sf
  
  # pontos dentro do polígono
  inside <- spatstat.geom::inside.owin(x = coords[,1], y = coords[,2], w = win)
  if (sum(inside) == 0) stop("Nenhum ponto dentro do polígono após projeção")
  
  ppp_obj <- ppp(x = coords[inside,1], y = coords[inside,2], window = win, check = FALSE)
  
  list(ppp = ppp_obj, shape_sf = conv$shape_sf)
}


# --- análise espacial (retorna lista com den, ppp, bw, etc)
run_spatial_analysis <- function(dfm, shape_sp, nx = 6, ny = 4, nsim = 99,
                                 dimyx = c(300, 300), cap_quantile = 0.98) {
  # cria objeto ppp
  ppp_obj_list <- make_ppp_from_df(dfm, shape_sp)
  ppp_obj <- ppp_obj_list$ppp
  shape_proj_sf <- ppp_obj_list$shape_sf
  
  # 1) Quadrats e teste qui-quadrado
  Q <- quadratcount(ppp_obj, nx = nx, ny = ny)
  quad_test <- quadrat.test(ppp_obj, nx = nx, ny = ny)
  
  # 2) Função g (pair correlation) e envelope
  g_emp <- pcf(ppp_obj)
  g_env <- envelope(ppp_obj, fun = pcf, nsim = nsim, rank = 1, global = FALSE, verbose = FALSE)
  
  # 3) Função G (nearest neighbor) e envelope
  Gfunc <- Gest(ppp_obj)
  G_env <- envelope(ppp_obj, fun = Gest, nsim = nsim, rank = 1, verbose = FALSE)
  
  # 4) Função K e envelope (Ripley)
  K_emp <- Kest(ppp_obj, correction = "Ripley")
  K_env <- envelope(ppp_obj, fun = Kest, nsim = nsim, correction = "Ripley", rank = 1, verbose = FALSE)
  
  # 5) Intensidade kernel
  bw <- bw.diggle(ppp_obj)                     # Diggle bandwidth (em metros)
  sigma <- 0.5 * bw                             # ajuste prático (meio-bw costuma ficar melhor visualmente)
  den <- density(ppp_obj, sigma = sigma, edge = TRUE, dimyx = dimyx) # retorna 'im' object
  
  # convert 'im' -> raster para manipulação da paleta e capping
  r_den <- raster::raster(den)  # cria raster a partir do objeto 'im'
  
  # capping para lidar com outliers na cor (evita que alguns peaks escondam as variações)
  vals <- raster::values(r_den)
  vmax <- as.numeric(quantile(vals, cap_quantile, na.rm = TRUE))
  vals_capped <- pmin(vals, vmax)
  raster::values(r_den) <- vals_capped
  
  # 6) PPM (máx verossimilhança - tenta usar covariáveis conhecidas)
  covs_available <- c("renda_percapita", "media_idh", "dist")
  covs_present <- covs_available[covs_available %in% names(dfm)]
  if (length(covs_present) > 0) {
    form_str <- paste("~", paste(covs_present, collapse = " + "))
    ppm_model <- try(ppm(ppp_obj, as.formula(form_str)), silent = TRUE)
  } else {
    ppm_model <- try(ppm(ppp_obj, ~ x + y + I(x^2) + I(y^2)), silent = TRUE)
  }
  intensity_fit <- if (inherits(ppm_model, "try-error")) NULL else predict(ppm_model, dimyx = dimyx)
  
  list(
    ppp = ppp_obj,
    shape_proj_sf = shape_proj_sf,
    Q = Q,
    quad_test = quad_test,
    g_emp = g_emp,
    g_env = g_env,
    Gfunc = Gfunc,
    G_env = G_env,
    K_emp = K_emp,
    K_env = K_env,
    bw = bw,
    sigma_used = sigma,
    den_im = den,
    den_raster = r_den,
    ppm_model = ppm_model,
    intensity_fit = intensity_fit
  )
}

# =============================
# EXEMPLO: executar com df_l e shape_sp já carregados
# =============================
# filtrar imóveis entre 500k e 1M (exemplo)
dfm <- df_l %>% 
  filter(!is.na(valor_total)) %>%
  filter(valor_total >= 500000 & valor_total <= 1000000)

# rodar a análise (nsim baixo para teste; aumente nsim = 199 ou 499 para resultados robustos)
res <- run_spatial_analysis(dfm, shape_sp, nx = 6, ny = 4, nsim = 39, dimyx = c(300,300), cap_quantile = 0.98)

# ---------------------------
# PLOT: intensidade kernel (raster) com paleta e pontos
# ---------------------------
# paleta
pal <- colorRampPalette(rev(brewer.pal(9, "YlOrRd")))(100)

# plot do raster com capping já aplicado
plot(res$den_raster, col = pal, main = "Intensidade kernel (capped 98%)", axes = FALSE, box = FALSE)
# sobrepor o polígono projetado e os pontos
plot(as(res$shape_proj_sf, "Spatial"), add = TRUE, border = "black", lwd = 0.6)
points(res$ppp$x, res$ppp$y, pch = 20, cex = 0.5)

# ---------------------------
# PLOT rápido das outras saídas
# ---------------------------
print(res$quad_test)
plot(res$ppp, main = "Pontos no espaço (dentro do polígono de SP)")
plot(res$Q, add = TRUE, col = "red")

plot(res$g_env, main = "Função g (pcf) com envelopes")
plot(res$G_env, main = "Função G com envelopes")
plot(res$K_env, main = "Função K com envelopes")

if (!is.null(res$intensity_fit)) {
  # intensity_fit é um 'im' se ocorrer predict; converter em raster e plotar opcionalmente
  r_ppm <- raster(res$intensity_fit)
  plot(r_ppm, col = pal, main = "Intensidade prevista (PPM)")
  plot(as(res$shape_proj_sf, "Spatial"), add = TRUE, border = "black")
  points(res$ppp$x, res$ppp$y, pch = 20, cex = 0.5)
}

# imprimir banda usada
cat("bw.diggle (m):", res$bw, "\n sigma usada (m):", res$sigma_used, "\n")

# Podemos com a função g que
# a curva observada (preta) sobe muito mais rápido
# que a curva teórica (vermelha tracejada).
# Isso significa que, em distâncias pequenas, 
# há mais vizinhos próximos do que o esperado por CSR, ou seja, clusterização.

# Assim como na Função K a curva observada (preta) está bem acima
# da curva teórica (vermelha), e também fora dos envelopes cinza.
# Isso confirma a presença de clusters em várias escalas de distância.

# --- DENSIDADE KERNEL: todos os imóveis (contagem) -----------------------

# 1) cria objeto ppp com TODOS os imóveis (não apenas dfm filtrado)
ppp_all_list <- make_ppp_from_df(df_l %>% filter(!is.na(lat) & !is.na(lon)), shape_sp)
ppp_all <- ppp_all_list$ppp
shape_proj_sf_all <- ppp_all_list$shape_sf

# 2) banda e densidade
bw_all <- bw.diggle(ppp_all)           # largura inicial em metros
sigma_all <- 0.5 * bw_all              # ajuste prático (testar 0.25, 0.5, 1 * bw_all)
den_all_im <- density(ppp_all, sigma = sigma_all, edge = TRUE, dimyx = c(400,400))

# 3) visualizar rapidamente (base R)
plot(den_all_im, main = paste0("Kernel density (all properties) — sigma = ", round(sigma_all)))
plot(as(shape_proj_sf_all, "Spatial"), add = TRUE, border = "black")
points(ppp_all$x, ppp_all$y, pch = 20, cex = 0.4)

# 4) converter em raster (opcional: cap para visual)
r_den_all <- raster(den_all_im)
vmax <- quantile(values(r_den_all), 0.98, na.rm = TRUE)
values(r_den_all) <- pmin(values(r_den_all), vmax)

# --- DENSIDADE KERNEL PESADA POR PREÇO ----------------------------------

# preparar dados com preço (filtrar NA)
df_price <- df_l %>% filter(!is.na(valor_total) & !is.na(lat) & !is.na(lon))

# montar ppp com marcadores (marks) usando make_ppp_from_df + reordenar
conv_price <- project_points_to_planar(df_price, shape_sp)
win_price <- as.owin(conv_price$shape_sf)
inside_price <- inside.owin(conv_price$coords[,1], conv_price$coords[,2], win_price)
coords_price <- conv_price$coords[inside_price, , drop = FALSE]
vals_price   <- df_price$valor_total[inside_price]

ppp_price <- ppp(x = coords_price[,1], y = coords_price[,2], window = win_price, marks = vals_price, check = FALSE)

# ajustar pesos: para evitar enormes diferenças de escala, normalize ou log-transform
weights_price <- (ppp_price$marks - min(ppp_price$marks, na.rm = TRUE) + 1) # shift positive
weights_price <- weights_price / median(weights_price, na.rm = TRUE)       # scale

# densidade ponderada por preço (usando os weights)
bw_price <- bw.diggle(ppp_price)
sigma_price <- 0.5 * bw_price
den_price_im <- density(ppp_price, sigma = sigma_price, edge = TRUE, weights = weights_price, dimyx = c(400,400))

# visualizar
plot(den_price_im, main = "Kernel density weighted by price")
plot(as(conv_price$shape_sf, "Spatial"), add = TRUE, border = "black")
points(ppp_price$x, ppp_price$y, pch = 20, cex = 0.4)


# converter o CRS do raster para WGS84 (leaflet precisa lon/lat)
r_wgs84 <- projectRaster(r_den_all, crs = CRS("+init=epsg:4326"), method = "bilinear")

# paleta
pal_fun <- colorNumeric(palette = "YlOrRd", domain = values(r_wgs84), na.color = "transparent")

leaflet() %>%
  addTiles() %>%
  addRasterImage(r_wgs84, colors = pal_fun, opacity = 0.7, project = FALSE) %>%
  addLegend(pal = pal_fun, values = values(r_wgs84),
            title = "Densidade (imóveis / m²)",
            labFormat = labelFormat(transform = function(x) signif(x,3))) %>%
  addPolygons(data = shape_sp, fill = FALSE, color = "black", weight = 1)



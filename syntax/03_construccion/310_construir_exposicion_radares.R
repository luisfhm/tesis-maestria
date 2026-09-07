# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 310_construir_exposicion_radares.R
# Objetivo:
#   - Construir medidas de exposición de cada colonia a los equipos de
#     fiscalización (radares / fotocívicas) para cada año del inventario
#   - Variar el buffer espacial: 250, 500, 1000 y 1500 metros
#   - Producir tanto conteos como cobertura de área y distancia al más cercano
#
# Entradas:
#   data/processed/colonias_limpias.gpkg
#   data/raw/08_radares/radares_fotocivicas.gpkg
#
# Salida:
#   data/processed/exposicion_radares_colonia_anual.parquet
#     (formato largo: id_colonia x anio x buffer_m)
#
# Uso previsto:
#   Para un evento en el año E, la exposición "justo antes" del evento se toma
#   del inventario del año E-1 (o E, según se decida en el diseño).
# ==============================================================================


# ------------------------------------------------------------------------------
# 0. Setup
# ------------------------------------------------------------------------------

source(
  here::here("syntax", "00_setup", "000_setup.R"),
  encoding = "UTF-8"
)

sf::sf_use_s2(FALSE)

BUFFERS <- c(250, 500, 1000, 1500)


# ------------------------------------------------------------------------------
# 1. Leer bases
# ------------------------------------------------------------------------------

colonias <- sf::st_read(
  fs::path(rutas$data_processed, "colonias_limpias.gpkg"),
  quiet = TRUE
) |>
  sf::st_transform(32614) |>
  dplyr::select(id_colonia, area_km2)

dispositivos <- sf::st_read(
  fs::path(rutas$data_raw, "08_radares", "radares_fotocivicas.gpkg"),
  quiet = TRUE
) |>
  dplyr::filter(!is.na(x), !is.na(y)) |>
  sf::st_transform(32614)

message("Dispositivos con coordenadas: ", nrow(dispositivos))
print(
  as.data.frame(dispositivos) |>
    dplyr::count(anio) |>
    dplyr::arrange(anio)
)

anios <- sort(unique(dispositivos$anio))
area_colonia_m2 <- as.numeric(sf::st_area(colonias))


# ------------------------------------------------------------------------------
# 2. Función: exposición de todas las colonias en un año y buffer
# ------------------------------------------------------------------------------

exposicion_anio_buffer <- function(anio_i, buffer_m) {

  disp <- dispositivos |>
    dplyr::filter(anio == anio_i)

  if (nrow(disp) == 0) return(NULL)

  # Buffer alrededor de cada dispositivo y unión
  zona <- disp |>
    sf::st_buffer(dist = buffer_m) |>
    sf::st_union()

  # Conteo de dispositivos a <= buffer de cada colonia
  cerca <- sf::st_is_within_distance(
    colonias, disp,
    dist = buffer_m
  )
  n_disp <- lengths(cerca)

  # Distancia al dispositivo más cercano
  dist_min <- apply(
    sf::st_distance(colonias, disp),
    1,
    min
  ) |>
    as.numeric()

  # Cobertura de área de la colonia dentro de la zona de exposición
  inter <- sf::st_intersection(sf::st_geometry(colonias), zona)
  area_exp <- rep(0, nrow(colonias))
  if (length(inter) > 0) {
    ids_inter <- sf::st_intersects(colonias, zona, sparse = FALSE)[, 1]
    area_exp[ids_inter] <- as.numeric(
      sf::st_area(
        sf::st_intersection(sf::st_geometry(colonias)[ids_inter], zona)
      )
    )
  }

  tibble::tibble(
    id_colonia = colonias$id_colonia,
    anio = anio_i,
    buffer_m = buffer_m,
    n_dispositivos = n_disp,
    dist_min_m = dist_min,
    share_area_expuesta = pmin(area_exp / area_colonia_m2, 1),
    tratada = as.integer(n_disp > 0)
  )
}


# ------------------------------------------------------------------------------
# 3. Calcular todas las combinaciones año x buffer
# ------------------------------------------------------------------------------

grid <- tidyr::expand_grid(
  anio_i = anios,
  buffer_m = BUFFERS
)

exposicion <- purrr::pmap_dfr(
  grid,
  function(anio_i, buffer_m) {
    message("  año ", anio_i, " · buffer ", buffer_m, " m")
    exposicion_anio_buffer(anio_i, buffer_m)
  }
)


# ------------------------------------------------------------------------------
# 4. Diagnóstico
# ------------------------------------------------------------------------------

resumen <- exposicion |>
  dplyr::group_by(anio, buffer_m) |>
  dplyr::summarise(
    colonias_tratadas = sum(tratada),
    media_n_disp = round(mean(n_dispositivos), 2),
    media_share_area = round(mean(share_area_expuesta), 3),
    .groups = "drop"
  )

message("")
message("==============================================================")
message("EXPOSICIÓN A EQUIPOS DE FISCALIZACIÓN")
message("==============================================================")
print(as.data.frame(resumen), row.names = FALSE)
message("==============================================================")


# ------------------------------------------------------------------------------
# 5. Guardar
# ------------------------------------------------------------------------------

archivo_salida <- fs::path(
  rutas$data_processed,
  "exposicion_radares_colonia_anual.parquet"
)

arrow::write_parquet(exposicion, archivo_salida)

message("")
message("Archivo guardado en: ", archivo_salida)

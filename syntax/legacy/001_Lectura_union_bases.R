# ============================================================
# Lectura y unión de las tablas de infracciones
# ============================================================

library(progress)

inf <- "data/infracciones_infracciones_transito"
year <- 2020:2023
bim <- 1:6

# Crear únicamente las combinaciones que sí existen
archivos_infracciones <- expand.grid(
  year = year,
  bim = bim
) %>%
  filter(!(year == 2023 & bim > 2)) %>%
  arrange(year, bim)

# Barra de progreso
pb_infracciones <- progress_bar$new(
  format = paste0(
    "Infracciones [:bar] :percent | ",
    ":current/:total | ",
    "Archivo: :archivo | ",
    "ETA: :eta"
  ),
  total = nrow(archivos_infracciones),
  clear = FALSE,
  width = 100
)

infrac <- data.frame()

for (k in seq_len(nrow(archivos_infracciones))) {

  i <- archivos_infracciones$year[k]
  j <- archivos_infracciones$bim[k]

  file <- paste0(inf, "_", i, "_b", j, ".csv")

  # Leer archivo
  df <- read.csv(file)

  # Homologar categoría para archivos antiguos
  if (i == 2020 || (i == 2021 && j < 3)) {
    df <- df %>%
      mutate(categoria = motivacion)
  }

  # Mantener únicamente columnas comunes
  if (j > 1 || i > 2020) {
    columns <- intersect(names(df), names(infrac))

    df <- df %>%
      select(all_of(columns))

    infrac <- infrac %>%
      select(all_of(columns))
  }

  # Unir archivo
  infrac <- rbind(infrac, df)

  # Actualizar progreso al terminar el archivo
  pb_infracciones$tick(
    tokens = list(
      archivo = paste0(i, "_b", j)
    )
  )
}

rm(df)
gc()

message("\nConvirtiendo fechas de infracciones...")

# Convertir a formato de fecha
infrac$fecha_infraccion <- as.Date(infrac$fecha_infraccion)

message("Guardando Infracciones.RDS...")

saveRDS(
  infrac,
  paste0(datadir, "Infracciones.RDS")
)

rm(infrac)
gc()

message("Archivo de infracciones terminado.\n")


# ============================================================
# Lectura y unión de las tablas de incidentes
# ============================================================

anios <- c(
  "2014_2015",
  "2016_2018",
  "2019_2021",
  "2022_2023"
)

file <- "inViales_"

pb_incidentes <- progress_bar$new(
  format = paste0(
    "Incidentes   [:bar] :percent | ",
    ":current/:total | ",
    "Archivo: :archivo | ",
    "ETA: :eta"
  ),
  total = length(anios),
  clear = FALSE,
  width = 100
)

incidentes <- data.frame()

for (i in anios) {

  ruta_archivo <- paste0(
    datadir,
    file,
    i,
    ".csv"
  )

  df <- read.csv(ruta_archivo)

  incidentes <- rbind(
    incidentes,
    df
  )

  pb_incidentes$tick(
    tokens = list(
      archivo = i
    )
  )
}

message("\nConvirtiendo fechas de incidentes...")

# Cambio de formato
incidentes$fecha_cierre <- as.Date(
  incidentes$fecha_cierre
)

incidentes$fecha_creacion <- as.Date(
  incidentes$fecha_creacion
)

message("Eliminando duplicados...")

# Eliminar duplicados
incidentes <- distinct(incidentes)

message("Guardando Incidentes.RDS...")

saveRDS(
  incidentes,
  paste0(datadir, "Incidentes.RDS")
)

rm(incidentes, df)
gc()

message("Proceso completo.")
# Figuras para el capítulo de Contexto (cap. 2 de la tesis):
#  - Serie mensual de víctimas de tránsito (total y fallecidos), 2018-2024.
#  - Mapas de concentración espacial: infracciones (alta concentración) vs
#    víctimas (distribución más homogénea), motivando los EF de colonia.
#
# Obsidian: [[Tesis - Diseño empírico]]

suppressMessages({
  library(arrow)
  library(sf)
  library(dplyr)
  library(ggplot2)
  library(lubridate)
})

rutas <- list(
  victimas = "data/processed/victimas_transito_limpias.parquet",
  panel    = "data/final/panel_analisis_colonia_mes.parquet",
  geom     = "data/processed/colonias_limpias.gpkg",
  figuras  = "output/figures"
)

tema_tesis <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "grey30", size = 10),
    panel.grid.minor = element_blank()
  )

# ------------------------------------------------------------------------------
# 1. Serie mensual de víctimas (Fig. 2.1 equivalente)
# ------------------------------------------------------------------------------

victimas <- read_parquet(rutas$victimas)

serie_mensual <- victimas |>
  mutate(mes = floor_date(fecha, "month")) |>
  # 2018 y hasta dic-2018 tiene cobertura muy escasa en las carpetas de la FGJ
  # (<30 registros/mes vs. ~350+ desde ene-2019): se excluye por ser un
  # artefacto de cobertura, no una caída real de víctimas.
  filter(mes >= as.Date("2019-01-01"), mes <= as.Date("2024-07-01")) |>
  group_by(mes) |>
  summarise(
    victimas = n(),
    fallecidos = sum(gravedad == "fallecido", na.rm = TRUE),
    .groups = "drop"
  )

eventos <- tibble::tibble(
  fecha = as.Date(c("2021-02-04", "2022-04-01", "2023-09-24")),
  etiqueta = c("feb-2021", "abr-2022", "sep-2023")
)

# Dos paneles con eje propio (en vez de reescalar fallecidos) porque las dos
# series difieren en un orden de magnitud (~400 vs. ~60 por mes) y una sola
# escala compartida aplastaría la variación de los fallecidos.
serie_larga <- serie_mensual |>
  tidyr::pivot_longer(cols = c(victimas, fallecidos),
                       names_to = "serie", values_to = "valor") |>
  mutate(serie = factor(serie, levels = c("victimas", "fallecidos"),
                         labels = c("Víctimas totales", "Fallecidos")))

fig_serie <- ggplot(serie_larga, aes(x = mes, y = valor, color = serie)) +
  geom_line(linewidth = 0.7) +
  geom_vline(data = eventos, aes(xintercept = fecha), linetype = "dashed",
             color = "grey50", linewidth = 0.4) +
  geom_text(data = eventos, aes(x = fecha, y = Inf, label = etiqueta),
            angle = 90, vjust = 1.3, hjust = 1.1, size = 3, color = "grey40",
            inherit.aes = FALSE) +
  facet_wrap(~serie, ncol = 1, scales = "free_y") +
  scale_color_manual(values = c("Víctimas totales" = "#1b6ca8",
                                 "Fallecidos" = "#c0392b"), guide = "none") +
  labs(
    title = "Víctimas de hechos de tránsito en la Ciudad de México, 2019-2024",
    subtitle = "Fuente: FGJ CDMX (carpetas de investigación). Cada panel tiene su propia escala.",
    x = NULL, y = "Por mes"
  ) +
  tema_tesis +
  theme(strip.text = element_text(face = "bold", size = 11))

ggsave(fs::path(rutas$figuras, "401_serie_victimas_2019_2024.png"),
       fig_serie, width = 8, height = 5.5, dpi = 150)

message("Guardada: 401_serie_victimas_2019_2024.png")

# ------------------------------------------------------------------------------
# 2. Mapas de concentración espacial: infracciones vs víctimas (Fig. 2.3/2.4 eq.)
# ------------------------------------------------------------------------------

panel <- read_parquet(rutas$panel)
geom  <- st_read(rutas$geom, quiet = TRUE)

totales_colonia <- panel |>
  group_by(id_colonia) |>
  summarise(
    n_infracciones = sum(n_infracciones, na.rm = TRUE),
    n_victimas = sum(n_victimas, na.rm = TRUE),
    .groups = "drop"
  )

geom_totales <- geom |>
  left_join(totales_colonia, by = "id_colonia") |>
  mutate(
    n_infracciones = ifelse(is.na(n_infracciones), 0, n_infracciones),
    n_victimas = ifelse(is.na(n_victimas), 0, n_victimas)
  )

fig_infracciones <- ggplot(geom_totales) +
  geom_sf(aes(fill = n_infracciones), color = "white", linewidth = 0.03) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, trans = "sqrt",
                        name = "Infracciones\n(2020-2023)") +
  labs(title = "Concentración espacial de las infracciones de tránsito") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

fig_victimas <- ggplot(geom_totales) +
  geom_sf(aes(fill = n_victimas), color = "white", linewidth = 0.03) +
  scale_fill_distiller(palette = "YlOrRd", direction = 1, trans = "sqrt",
                        name = "Víctimas\n(2019-jul 2024)") +
  labs(title = "Distribución espacial de las víctimas de tránsito") +
  theme_void(base_size = 12) +
  theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5))

ggsave(fs::path(rutas$figuras, "402_mapa_infracciones_colonia.png"),
       fig_infracciones, width = 6, height = 6, dpi = 150, bg = "white")
ggsave(fs::path(rutas$figuras, "403_mapa_victimas_colonia.png"),
       fig_victimas, width = 6, height = 6, dpi = 150, bg = "white")

message("Guardadas: 402_mapa_infracciones_colonia.png, 403_mapa_victimas_colonia.png")

# Estadístico simple de concentración: % de infracciones/víctimas en el 10% de
# colonias con más eventos (índice de concentración citado en el texto).
top10pct <- ceiling(0.10 * nrow(totales_colonia))

concentracion <- totales_colonia |>
  summarise(
    pct_infracciones_top10 = sum(sort(n_infracciones, decreasing = TRUE)[1:top10pct]) /
      sum(n_infracciones) * 100,
    pct_victimas_top10 = sum(sort(n_victimas, decreasing = TRUE)[1:top10pct]) /
      sum(n_victimas) * 100
  )

message("")
message("Concentración en el 10% de colonias con más eventos:")
message("  Infracciones: ", round(concentracion$pct_infracciones_top10, 1), "%")
message("  Víctimas:     ", round(concentracion$pct_victimas_top10, 1), "%")

readr::write_csv(concentracion, fs::path(rutas$figuras, "../..", "docs/04_diseno_empirico/concentracion_espacial.csv"))

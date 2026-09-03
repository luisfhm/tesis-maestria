#!/usr/bin/env Rscript
# ==============================================================================
# TESIS DE MAESTRÍA
# Script: generar_bitacora_obsidian.R
# Objetivo:
#   - Leer el estado del repositorio (git, cabeceras de scripts, docs, outputs)
#   - Escribir/actualizar una "Bitácora de código" en la bóveda de Obsidian
#   - Insertar un bloque "Scripts relacionados" en las notas de Fase 3 y Fase 4
#
# Entrada:
#   El propio repositorio (raíz detectada con git)
#
# Salidas (dentro de la bóveda de Obsidian):
#   Bitácora de código.md                     (se regenera por completo)
#   Fase 3 — Reconstruir datos.md              (bloque AUTOGEN)
#   Fase 4 — Nuevo análisis econométrico.md    (bloque AUTOGEN)
#
# Uso:
#   "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" syntax/tools/generar_bitacora_obsidian.R
#   (se ejecuta solo en cada commit vía .githooks/post-commit)
#
# Config:
#   Variable de entorno TESIS_OBSIDIAN_DIR para sobrescribir la carpeta destino.
#
# Regla:
#   Este script NO modifica datos ni el código de análisis. Solo escribe notas.
# ==============================================================================

suppressWarnings(suppressMessages({
  library(fs)
  library(stringr)
}))

# ------------------------------------------------------------------------------
# 0. Configuración
# ------------------------------------------------------------------------------

RUTA_OBSIDIAN_DEFAULT <- "D:/Obsidian/Educación/Maestría/Titulación"
ruta_obsidian <- Sys.getenv("TESIS_OBSIDIAN_DIR", unset = RUTA_OBSIDIAN_DEFAULT)

NOMBRE_BITACORA <- "Bitácora de código.md"
NOTA_FASE_3     <- "Fase 3 — Reconstruir datos.md"
NOTA_FASE_4     <- "Fase 4 — Nuevo análisis econométrico.md"

# Raíz del repositorio
raiz <- tryCatch(
  suppressWarnings(system2("git", c("rev-parse", "--show-toplevel"),
                           stdout = TRUE, stderr = FALSE)),
  error = function(e) character(0)
)
if (!length(raiz) || !nzchar(raiz[1])) {
  raiz <- tryCatch(
    if (requireNamespace("here", quietly = TRUE)) here::here() else getwd(),
    error = function(e) getwd()
  )
}
raiz <- normalizePath(raiz[1], winslash = "/", mustWork = FALSE)

if (!dir_exists(ruta_obsidian)) {
  message("[puente] No existe la carpeta de Obsidian: ", ruta_obsidian)
  message("[puente] Define TESIS_OBSIDIAN_DIR o crea la carpeta. Se omite.")
  quit(save = "no", status = 0)
}

AHORA <- format(Sys.time(), "%Y-%m-%d %H:%M")

# ------------------------------------------------------------------------------
# 1. Utilidades
# ------------------------------------------------------------------------------

git <- function(...) {
  out <- tryCatch(
    suppressWarnings(system2("git", c("-C", raiz, ...),
                             stdout = TRUE, stderr = FALSE)),
    error = function(e) character(0)
  )
  if (is.null(out)) character(0) else out
}

leer_utf8 <- function(archivo) {
  if (!file_exists(archivo)) return(character(0))
  con <- file(archivo, open = "r", encoding = "UTF-8")
  on.exit(close(con))
  readLines(con, warn = FALSE)
}

escribir_utf8 <- function(lineas, archivo) {
  dir_create(path_dir(archivo))
  con <- file(archivo, open = "wb")
  on.exit(close(con))
  writeLines(enc2utf8(lineas), con, sep = "\n", useBytes = TRUE)
}

sin_vineta <- function(x) str_trim(str_replace(x, "^\\s*[-*]\\s*", ""))

md_pipe <- function(x) str_replace_all(x, "\\|", "\\\\|")

acortar <- function(x, n = 200) {
  if (str_length(x) > n) str_c(str_sub(x, 1, n - 1), "…") else x
}

# Parsear la cabecera de un script .R -----------------------------------------
parsear_cabecera <- function(archivo) {
  lineas <- leer_utf8(archivo)
  if (!length(lineas)) return(list(objetivo = character(0),
                                   entrada = character(0),
                                   salida = character(0),
                                   obsidian = NA_character_))

  idx_reglas <- which(str_detect(lineas, "^#\\s*={5,}"))
  fin <- if (length(idx_reglas) >= 2) idx_reglas[2] else min(length(lineas), 45L)
  cab <- str_replace(lineas[seq_len(fin)], "^#\\s?", "")

  ETIQUETAS <- str_c(
    "Objetivos?|Entradas?|Salidas?|Resultados?|Reglas?|Notas?|Uso|Config|",
    "Fuentes?|Método|Especificación|Contexto|Supuestos?|Advertencias?|",
    "IMPORTANTE|Obsidian|Requisitos?|Depende de|Parámetros?"
  )
  es_etiqueta <- function(l) {
    str_detect(l, regex(str_c("^\\s*(", ETIQUETAS, ")\\s*:\\s*$"),
                        ignore_case = TRUE))
  }

  campo <- function(nombre_regex) {
    pat_sola   <- str_c("^\\s*(", nombre_regex, ")\\s*:\\s*$")
    pat_inline <- str_c("^\\s*(", nombre_regex, ")\\s*:\\s*(.+)$")
    i <- which(str_detect(cab, regex(pat_sola, ignore_case = TRUE)) |
               str_detect(cab, regex(pat_inline, ignore_case = TRUE)))
    if (!length(i)) return(character(0))
    i <- i[1]
    val <- character(0)
    m <- str_match(cab[i], regex(pat_inline, ignore_case = TRUE))
    if (!is.na(m[1, 3])) val <- c(val, str_trim(m[1, 3]))
    j <- i + 1
    while (j <= length(cab)) {
      l <- cab[j]
      if (str_detect(l, "^\\s*={5,}")) break
      if (!nzchar(str_trim(l))) break
      if (es_etiqueta(l)) break
      if (str_detect(l, "^\\s{2,}\\S")) {
        val <- c(val, str_trim(l))
        j <- j + 1
      } else {
        break
      }
    }
    val[nzchar(val)]
  }

  objetivo <- campo("Objetivos?")

  # Fallback para cabeceras antiguas sin etiqueta "Objetivo:" (solo descripción)
  if (!length(objetivo)) {
    basura <- regex(str_c(
      "^={3,}|^\\s*$|^TESIS DE MAESTR|^Script\\s*:|^\\s*[0-9]{3}_.*\\.R\\s*$|",
      "^\\s*source\\(|^(", ETIQUETAS, ")\\s*:"
    ), ignore_case = TRUE)
    cand <- cab[!str_detect(cab, basura)]
    cand <- str_trim(cand)
    cand <- cand[nzchar(cand)]
    if (length(cand)) objetivo <- head(cand, 3)
  }

  obsi <- campo("Obsidian")
  list(
    objetivo = objetivo,
    entrada  = campo("Entradas?"),
    salida   = campo("Salidas?|Resultados?"),
    obsidian = if (length(obsi)) sin_vineta(obsi[1]) else NA_character_
  )
}

objetivo_fmt <- function(x) {
  if (!length(x)) return("—")
  md_pipe(acortar(str_c(sin_vineta(x), collapse = "; ")))
}

genera_fmt <- function(x) {
  if (!length(x)) return("—")
  b <- path_file(sin_vineta(x))
  b <- b[nzchar(b) & str_detect(b, "\\.")]
  if (!length(b)) return("—")
  str_c("`", md_pipe(b), "`", collapse = "<br>")
}

# ------------------------------------------------------------------------------
# 2. Estado de git
# ------------------------------------------------------------------------------

rama <- git("rev-parse", "--abbrev-ref", "HEAD")
rama <- if (length(rama)) rama[1] else "—"

recientes <- git("log", "-8", "--pretty=format:%h|%ad|%s", "--date=short")
recientes_md <- if (length(recientes)) {
  p <- str_split_fixed(recientes, "\\|", 3)
  str_c("- `", p[, 1], "` · ", p[, 2], " · ", p[, 3])
} else "- (sin historial)"

porcelain <- git("status", "--porcelain")
porcelain <- porcelain[nzchar(porcelain)]
if (!length(porcelain)) {
  estado_wt <- "Árbol de trabajo limpio."
} else {
  rutas_wt <- str_replace_all(str_trim(str_sub(porcelain, 4)), '"', "")
  top <- str_replace(rutas_wt, "^([^/]+/[^/]+).*", "\\1")
  top <- str_replace(top, "^([^/]+)$", "\\1")
  tb <- sort(table(top), decreasing = TRUE)
  estado_wt <- str_c(
    c(
      str_c("**", length(rutas_wt),
            " archivo(s) sin commitear** — recuerda versionar el avance:"),
      str_c("- `", names(tb), "` — ", as.integer(tb))
    ),
    collapse = "\n"
  )
}

# ------------------------------------------------------------------------------
# 3. Inventario del pipeline (syntax/)
# ------------------------------------------------------------------------------

CARPETAS <- list(
  list(dir = "syntax/00_setup",        titulo = "00 · Setup",        fase = 3),
  list(dir = "syntax/01_exploracion",  titulo = "01 · Exploración",  fase = 3),
  list(dir = "syntax/02_limpieza",     titulo = "02 · Limpieza",     fase = 3),
  list(dir = "syntax/03_construccion", titulo = "03 · Construcción", fase = 3),
  list(dir = "syntax/04_descriptivos", titulo = "04 · Descriptivos", fase = 4),
  list(dir = "syntax/05_modelos",      titulo = "05 · Modelos",      fase = 4),
  list(dir = "syntax/06_robustez",     titulo = "06 · Robustez",     fase = 4)
)

tabla_etapa <- function(carpeta) {
  ruta <- path(raiz, carpeta$dir)
  if (!dir_exists(ruta)) return(NULL)
  archivos <- sort(dir_ls(ruta, glob = "*.R", type = "file"))
  if (!length(archivos)) return(NULL)

  filas <- vapply(archivos, function(f) {
    h <- parsear_cabecera(f)
    obs <- if (!is.na(h$obsidian) && nzchar(h$obsidian)) {
      str_c(" · ", h$obsidian)
    } else ""
    str_c("| `", path_file(f), "`", obs, " | ",
          objetivo_fmt(h$objetivo), " | ",
          genera_fmt(h$salida), " |")
  }, character(1))

  c(
    str_c("### ", carpeta$titulo),
    "",
    "| Script | Objetivo | Genera |",
    "|---|---|---|",
    filas,
    ""
  )
}

pipeline_md <- unlist(lapply(CARPETAS, tabla_etapa), use.names = FALSE)

tabla_fase <- function(n_fase) {
  bloques <- lapply(Filter(function(c) c$fase == n_fase, CARPETAS), tabla_etapa)
  unlist(bloques, use.names = FALSE)
}

# ------------------------------------------------------------------------------
# 4. Documentación técnica (docs/)
# ------------------------------------------------------------------------------

titulo_md <- function(f) {
  l <- leer_utf8(f)
  h <- l[str_detect(l, "^#\\s+")]
  if (!length(h)) path_file(f) else str_trim(str_replace(h[1], "^#\\s+", ""))
}
estado_md <- function(f) {
  l <- leer_utf8(f)
  e <- l[str_detect(l, "\\*\\*Estado:?\\*\\*")]
  if (!length(e)) return(NA_character_)
  str_trim(str_replace(e[1], ".*\\*\\*Estado:?\\*\\*\\s*", ""))
}

docs_dir <- path(raiz, "docs")
docs_md <- if (dir_exists(docs_dir)) {
  archivos <- sort(dir_ls(docs_dir, recurse = TRUE, glob = "*.md", type = "file"))
  vapply(archivos, function(f) {
    rel <- path_rel(f, raiz)
    est <- estado_md(f)
    est_txt <- if (!is.na(est)) str_c(" — ", est) else ""
    str_c("- **", titulo_md(f), "**", est_txt, "  \n  `", rel, "`")
  }, character(1))
} else "- (sin carpeta `docs/`)"

# Decisiones metodológicas
dec_file <- path(docs_dir, "05_decisiones_metodologicas.md")
decisiones_md <- if (file_exists(dec_file)) {
  dl <- leer_utf8(dec_file)
  idx <- which(str_detect(dl, "^##\\s+D\\d"))
  if (length(idx)) {
    vapply(seq_along(idx), function(k) {
      ini <- idx[k]
      fin <- if (k < length(idx)) idx[k + 1] - 1 else length(dl)
      titulo <- str_trim(str_replace(dl[ini], "^##\\s+", ""))
      est_l <- dl[ini:fin][str_detect(dl[ini:fin], "\\*\\*Estado:?\\*\\*")]
      est <- if (length(est_l)) {
        str_trim(str_replace(est_l[1], ".*\\*\\*Estado:?\\*\\*\\s*", ""))
      } else "—"
      str_c("- **", titulo, "** — ", est)
    }, character(1))
  } else "- (sin decisiones registradas)"
} else "- (sin `docs/05_decisiones_metodologicas.md`)"

# ------------------------------------------------------------------------------
# 5. Outputs recientes
# ------------------------------------------------------------------------------

listar_outputs <- function(sub, tipo) {
  ruta <- path(raiz, "output", sub)
  if (!dir_exists(ruta)) return(NULL)
  info <- dir_info(ruta, type = "file")
  if (!nrow(info)) return(NULL)
  data.frame(
    archivo = path_file(info$path),
    tipo = tipo,
    fecha = as.Date(info$modification_time),
    ts = info$modification_time,
    stringsAsFactors = FALSE
  )
}

out_df <- do.call(rbind, list(
  listar_outputs("figures", "figura"),
  listar_outputs("tables", "tabla"),
  listar_outputs("maps", "mapa")
))

outputs_md <- if (is.null(out_df) || !nrow(out_df)) {
  "- (sin outputs)"
} else {
  out_df <- out_df[order(out_df$ts, decreasing = TRUE), ]
  n <- min(nrow(out_df), 20L)
  c(
    str_c("Últimos ", n, " de ", nrow(out_df), " archivos en `output/`:"),
    "",
    "| Archivo | Tipo | Fecha |",
    "|---|---|---|",
    str_c("| `", out_df$archivo[seq_len(n)], "` | ",
          out_df$tipo[seq_len(n)], " | ",
          out_df$fecha[seq_len(n)], " |")
  )
}

# ------------------------------------------------------------------------------
# 6. Pendientes (README.md → Próximos pasos)
# ------------------------------------------------------------------------------

readme <- leer_utf8(path(raiz, "README.md"))
pendientes_md <- {
  i0 <- which(str_detect(readme, "^#+\\s+Próximos pasos"))
  if (!length(i0)) {
    "- (no se encontró la sección «Próximos pasos» en el README)"
  } else {
    resto <- readme[(i0[1] + 1):length(readme)]
    fin <- which(str_detect(resto, "^#+\\s+"))
    if (length(fin)) resto <- resto[seq_len(fin[1] - 1)]
    items <- resto[str_detect(resto, "^\\s*-\\s*\\[[ xX]\\]")]
    if (!length(items)) "- (sin items)" else str_trim(items)
  }
}

# ------------------------------------------------------------------------------
# 7. Escribir la bitácora
# ------------------------------------------------------------------------------

bitacora <- c(
  "---",
  "type: reference",
  "tags:",
  "  - tesis",
  "  - codigo",
  "  - bitacora",
  "  - autogenerado",
  str_c("generado: ", AHORA),
  str_c("repo: ", raiz),
  "---",
  "",
  "> [!warning] Nota autogenerada — no editar a mano",
  str_c("> La escribe `syntax/tools/generar_bitacora_obsidian.R` en cada commit del repo `",
        path_file(raiz), "`."),
  "> Cualquier cambio manual se sobrescribe. Enlázala desde otras notas con `[[Bitácora de código]]`.",
  "",
  "# Bitácora de código — Tesis",
  "",
  str_c("Fases: [[Titulación — Maestría en Economía Aplicada]] · ",
        "[[Fase 3 — Reconstruir datos]] · ",
        "[[Fase 4 — Nuevo análisis econométrico]]"),
  "",
  "---",
  "",
  "## Estado del repositorio",
  "",
  str_c("- **Rama:** `", rama, "`"),
  str_c("- **Actualizado:** ", AHORA),
  "",
  "### Commits recientes",
  "",
  recientes_md,
  "",
  "### Cambios pendientes de commit",
  "",
  estado_wt,
  "",
  "---",
  "",
  "## Pipeline de análisis (`syntax/`)",
  "",
  "Cada fila sale de la cabecera del script. Añade `# Obsidian: [[nota]]` a la",
  "cabecera de un script para enlazarlo con una nota concreta.",
  "",
  pipeline_md,
  "---",
  "",
  "## Documentación técnica (`docs/`)",
  "",
  docs_md,
  "",
  "### Decisiones metodológicas",
  "",
  decisiones_md,
  "",
  "---",
  "",
  "## Outputs recientes",
  "",
  outputs_md,
  "",
  "---",
  "",
  "## Pendientes (según `README.md`)",
  "",
  pendientes_md,
  "",
  "---",
  "",
  str_c("_Regenerar manualmente:_ `\"C:/Program Files/R/R-4.5.1/bin/Rscript.exe\" ",
        "syntax/tools/generar_bitacora_obsidian.R`"),
  ""
)

escribir_utf8(bitacora, path(ruta_obsidian, NOMBRE_BITACORA))
message("[puente] Bitácora escrita: ", path(ruta_obsidian, NOMBRE_BITACORA))

# ------------------------------------------------------------------------------
# 8. Inyectar bloque en las notas de Fase 3 y Fase 4
# ------------------------------------------------------------------------------

inyectar_bloque <- function(archivo, marcador, contenido) {
  if (!file_exists(archivo)) {
    message("[puente] No existe (se omite): ", archivo)
    return(invisible(FALSE))
  }
  ini <- str_c("<!-- AUTOGEN:", marcador, " START -->")
  fin <- str_c("<!-- AUTOGEN:", marcador, " END -->")
  bloque <- str_c(ini, "\n", contenido, "\n", fin)

  txt <- str_c(leer_utf8(archivo), collapse = "\n")

  if (str_detect(txt, fixed(ini)) && str_detect(txt, fixed(fin))) {
    antes   <- sub(str_c("(?s)\\Q", ini, "\\E.*"), "", txt, perl = TRUE)
    despues <- sub(str_c("(?s).*\\Q", fin, "\\E"), "", txt, perl = TRUE)
    txt <- str_c(antes, bloque, despues)
  } else {
    txt <- str_c(str_trim(txt), "\n\n", bloque, "\n")
  }
  escribir_utf8(str_split(txt, "\n")[[1]], archivo)
  message("[puente] Bloque actualizado en: ", path_file(archivo))
  invisible(TRUE)
}

encabezado_bloque <- function(n_fase) {
  str_c(
    "## Scripts relacionados (autogenerado)\n\n",
    "_Actualizado ", AHORA, " por `generar_bitacora_obsidian.R`. ",
    "Detalle completo en [[Bitácora de código]]._\n"
  )
}

inyectar_bloque(
  path(ruta_obsidian, NOTA_FASE_3),
  "scripts-fase3",
  str_c(encabezado_bloque(3), "\n",
        str_c(tabla_fase(3), collapse = "\n"))
)

inyectar_bloque(
  path(ruta_obsidian, NOTA_FASE_4),
  "scripts-fase4",
  str_c(encabezado_bloque(4), "\n",
        str_c(tabla_fase(4), collapse = "\n"))
)

message("[puente] Listo.")

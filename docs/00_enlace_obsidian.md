# 00 — Enlace con Obsidian

Este repositorio y la bóveda de Obsidian de la titulación están conectados para
que el trabajo de código quede documentado del lado de la gestión de la tesis.

- **Repositorio:** `D:/maestría/tesis-maestria`
- **Carpeta en Obsidian:** `D:/Obsidian/Educación/Maestría/Titulación`

---

## Dirección código → Obsidian (automática)

El script `syntax/tools/generar_bitacora_obsidian.R` lee el estado del repo y
escribe/actualiza tres notas en la bóveda:

| Nota en Obsidian | Contenido |
|---|---|
| `Bitácora de código.md` | Estado de git, inventario del pipeline, docs, decisiones, outputs recientes y pendientes. Se regenera completa. |
| `Fase 3 — Reconstruir datos.md` | Bloque `AUTOGEN:scripts-fase3` con las etapas 00–03. |
| `Fase 4 — Nuevo análisis econométrico.md` | Bloque `AUTOGEN:scripts-fase4` con las etapas 04–06. |

El bloque en las notas de fase va delimitado por comentarios HTML
(`<!-- AUTOGEN:... START/END -->`); todo lo que escribas fuera de esos
marcadores se conserva.

### Cuándo se ejecuta

- **En cada commit**, mediante el hook `.githooks/post-commit`.
- **A mano**, cuando quieras, con:

  ```
  "C:/Program Files/R/R-4.5.1/bin/Rscript.exe" syntax/tools/generar_bitacora_obsidian.R
  ```

### Activar el hook (una sola vez, tras clonar)

```
git config core.hooksPath .githooks
```

El hook nunca bloquea el commit: si R no está disponible o la carpeta de
Obsidian no existe, solo imprime un aviso.

### Cambiar la ruta de la bóveda

Por variable de entorno:

```
setx TESIS_OBSIDIAN_DIR "D:/otra/ruta/Titulación"
```

o editando `RUTA_OBSIDIAN_DEFAULT` en el script.

---

## Dirección Obsidian → código (convención manual)

Para que un script apunte a la nota de Obsidian que lo motiva o lo discute,
se añade una línea `# Obsidian:` en la cabecera:

```r
# ==============================================================================
# TESIS DE MAESTRÍA
# Script: 506_event_study_2023.R
# Objetivo:
#   - Estimar el estudio de evento de septiembre de 2023
# Obsidian: [[Fase 2 — Rediseñar investigación]]
# ==============================================================================
```

El generador recoge ese enlace y lo muestra junto al nombre del script en la
bitácora y en las notas de fase. Si no hay línea `# Obsidian:`, el script se
clasifica por carpeta (00–03 → Fase 3, 04–06 → Fase 4).

---

## Reglas

1. La `Bitácora de código.md` es de solo lectura: no editarla en Obsidian.
2. En las notas de fase, escribir siempre **fuera** de los marcadores `AUTOGEN`.
3. Si se añade una etapa nueva a `syntax/`, registrarla en la lista `CARPETAS`
   del generador.
4. El puente solo escribe notas; nunca toca `data/`, `output/` ni el análisis.

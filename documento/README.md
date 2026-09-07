# Documento de la tesis

Proyecto LaTeX de la tesis de maestría. Reemplaza la versión de 2024
(`../Paper_Inv_Aplicada_II.pdf`, "El efecto de las multas sobre los accidentes
de tráfico"). Andamio ITAM reutilizado de la tesina de licenciatura del autor.

## Compilar

```
cd documento
pdflatex main
bibtex main
pdflatex main
pdflatex main
```

(o `latexmk -pdf main.tex`)

## Estructura

| Archivo | Estado | Base 2024 |
|---|---|---|
| `capitulos/00_introduccion.tex` | borrador | pp. 1–3 |
| `capitulos/01_literatura.tex` | borrador (falta ampliar) | pp. 4–5 |
| `capitulos/02_contexto.tex` | esqueleto con TODOs | pp. 6–11 |
| `capitulos/03_datos.tex` | esqueleto con TODOs | pp. 12–16 |
| `capitulos/04_estrategia_empirica.tex` | **redactado** | pp. 17–19 (reescrito) |
| `capitulos/05_resultados.tex` | **redactado** | pp. 20–24 (reescrito) |
| `capitulos/06_conclusiones.tex` | esqueleto con TODOs | pp. 25–26 |

## Qué cambia respecto a la versión 2024

| | 2024 | Ahora |
|---|---|---|
| Unidad | código postal-mes | colonia-mes (unidad territorial IECM) |
| Evento | Reglamento de Tránsito 2022 (shock homogéneo) | renovación de la Policía de Tránsito, abr-2022 (aplicación diferencial en el espacio) |
| Tratamiento | $T_i$ = infracciones sobre la mediana un año antes | exposición a infraestructura de fiscalización, buffer 250/500/1000 m |
| Identificación | DiD + IV (instrumento = $T_i$) | estudio de evento mes a mes con EF de colonia y mes; **sin IV** |
| Multas | primera etapa del IV | **mecanismo** (evidencia de más enforcement), no primera etapa |
| Outcome | incidentes C5 | **víctimas FGJ** (principal); C5 secundario con advertencia de pre-tendencias |
| Ventana | −11 / +12 | −10 / +10 |
| Resultado | IV +0.716 (más multas → más accidentes), sig. 1% | **nulo robusto** sobre víctimas; el efecto lineal desaparece con Poisson |

## Figuras

Se copian de `../output/figures/`. Actualizar tras cada corrida de
`syntax/05_modelos/` y `syntax/06_robustez/`:

- `509_pretendencias_c5_vs_victimas.png`
- `510_victimas.png`, `510_mecanismo_multas.png`, `510_incidentes_c5.png`
- `603_robustez_att.png`

## Pendientes mayores

- [ ] Cap. 1: ampliar literatura (reformas de policía, efectos de reporte, diseño DiD/event study).
- [ ] Cap. 2: línea del tiempo de política vial; mapas de calor actualizados.
- [ ] Cap. 3: descriptivos a nivel colonia; tabla de exposición por buffer; auditoría de muestra.
- [ ] Cap. 5: tabla completa del DiD (todas las variantes) desde `output/tables/510_did_agregado.csv` y `603_robustez_att.csv`.
- [ ] Cap. 6: desarrollar la lectura del nulo y la relación con la versión 2024.
- [ ] Revisar el título a la luz del rediseño.
- [ ] Resumen / abstract.

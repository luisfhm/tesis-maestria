# Inventario de fuentes de datos

## Objetivo

Documentar las fuentes disponibles para reconstruir la estrategia empírica de
la tesis, incluyendo cobertura temporal, granularidad geográfica y estado de
auditoría.

## Fuentes

| ID | Fuente | Cobertura conocida | Geografía | Estado |
|---|---|---|---|---|
| 01 | Incidentes viales C5 | 2014–2024* | Coordenadas | 🟢 Auditado |
| 02 | Hechos de tránsito SSC | 2018–2023 | Coordenadas | 🟢 Auditado |
| 05a | Infracciones generales | 2020–2023 | Pendiente | 🟡 Pendiente |
| 05b | Infracciones de parquímetros | 2025 | Pendiente | 🟡 Pendiente |
| 06 | Ubicación de fotomultas pre-2019 | Pre-2019 | Puntos | 🟡 Parcial |
| 07a | Cumplimiento de Fotocívicas | Pendiente | Pendiente | 🟡 Pendiente |
| 07b | Ubicación de Fotocívicas | Pendiente | Puntos/líneas | 🟡 Pendiente |
| 08 | Radares | Pendiente | Puntos | 🟡 Pendiente |
| 09 | INFOVIAL | 2016–2019 | Vialidades | 🟡 Pendiente |
| 10 | Accidentes peatones/ciclistas | Pendiente | Puntos | 🟡 Pendiente |
| 11 | Depósitos vehiculares | 2022 | Pendiente | 🟡 Pendiente |
| 12 | Accidentes por alcaldía | 2018–2023 | Alcaldía | 🟡 Pendiente |
| 13 | Vehículos registrados | 1990–2019 | Pendiente | 🟡 Pendiente |

\* La auditoría de C5 muestra información principal hasta febrero de 2024.

## Fuentes auditadas

### 01 — Incidentes viales C5

**Estado:** 🟢 Utilizable

Cobertura extensa y georreferenciada de incidentes viales.

Ventaja principal:

- Serie temporal larga.
- Coordenadas.
- Diferentes categorías de incidentes.

Consultar:

`docs/03_exploracion_datos/01_incidentes_c5.md`

### 02 — Hechos de tránsito SSC

**Estado:** 🟢 Utilizable

Cobertura 2018–2023 con información detallada sobre hechos de tránsito,
lesionados, fallecidos, tipos de persona y vehículos involucrados.

Ventaja principal:

- Severidad del accidente.
- Características de víctimas.
- Tipo de vehículo.
- Coordenadas.

Consultar:

`docs/03_exploracion_datos/02_hechos_transito_ssc.md`

## Estado de la auditoría

Actualmente se está realizando la exploración inicial de todas las fuentes
antes de seleccionar el evento de política y definir la estrategia empírica.

No se construirán todavía bases analíticas definitivas hasta terminar esta
etapa.
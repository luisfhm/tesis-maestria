# 01 — Incidentes viales C5

**Estado:** 🟢 Utilizable

## Fuente

Incidentes viales reportados a través de los sistemas de atención de
emergencias de la Ciudad de México.

## Cobertura temporal

Los archivos disponibles contienen información desde 2014 hasta febrero de
2024.

Se detectaron algunos registros correspondientes al año anterior dentro de
los bloques publicados.

### Archivos

- `inViales_2014_2015.csv`
- `inViales_2016_2018.csv`
- `inViales_2019_2021.csv`
- `inViales_2022_2023.csv`
- `inViales_2022_2024.csv`

## Solapamiento

El archivo `inViales_2022_2024.csv` contiene nuevamente los registros
correspondientes a 2022 y 2023.

La comprobación mediante folios mostró:

| Año | Folios archivo anterior | Presentes en archivo nuevo |
|---|---:|---:|
| 2022 | 234,938 | 100% |
| 2023 | 128,361 | 100% |

Por tanto, `inViales_2022_2023.csv` no debe concatenarse con
`inViales_2022_2024.csv`.

El archivo más reciente debe utilizarse para 2022–2024.

## Cobertura de 2024

El archivo más reciente contiene:

| Mes | Observaciones |
|---|---:|
| Enero 2024 | 17,265 |
| Febrero 2024 | 18,660 |

Por tanto, la cobertura disponible para 2024 termina en febrero.

## Geografía

La base contiene coordenadas que permiten construir unidades espaciales para
el análisis.

## Uso potencial

Esta fuente es especialmente atractiva como outcome debido a:

- cobertura temporal extensa;
- frecuencia elevada;
- georreferenciación;
- clasificación de incidentes.

Puede utilizarse para construir outcomes agregados espacial y temporalmente.

## Consideraciones

La pandemia produce un cambio importante en movilidad durante 2020 y debe
considerarse explícitamente al definir las ventanas del análisis.

Los archivos originales deben permanecer sin modificaciones en `data/raw`.

## Script de auditoría

`syntax/01_exploracion/102_explorar_incidentes_c5.R`
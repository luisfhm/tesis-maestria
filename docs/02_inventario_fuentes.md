# Inventario de fuentes de datos

## Objetivo

Documentar las fuentes disponibles para reconstruir la estrategia empírica de
la tesis, incluyendo cobertura temporal, granularidad espacial, utilidad
potencial y estado de auditoría.

La auditoría busca determinar qué contiene realmente cada fuente antes de
construir las bases analíticas o seleccionar de manera definitiva el evento de
política y la estrategia de identificación.

---

## Resumen de fuentes

| ID | Fuente | Cobertura observada | Granularidad espacial | Papel potencial | Estado |
|---|---|---|---|---|---|
| 01 | Incidentes viales C5 | 2014–feb. 2024 | Coordenadas | Outcome principal | 🟢 Auditado |
| 02 | Hechos de tránsito SSC | 2018–2023 | Coordenadas | Outcome / severidad / heterogeneidad | 🟢 Auditado |
| 05a | Infracciones generales | ene. 2020–abr. 2023 | Alcaldía; CP parcial; coordenadas hasta abr. 2021 | Enforcement | 🟢 Auditado |
| 05b | Infracciones de parquímetros | 2025 | Pendiente | Enforcement específico | 🟡 Pendiente |
| 06 | Fotomultas pre-2019 | Pre-2019 | Puntos | Infraestructura histórica | 🟡 Parcial |
| 07a | Cumplimiento de Fotocívicas | jul. 2019–2022 | Sin geografía útil para evento | Operación del sistema | 🟢 Auditado |
| 07b | Ubicación de Fotocívicas | Fotografía disponible | Puntos y líneas | Exposición espacial | 🟢 Auditado |
| 08 | Radares / Fotocívicas | 2019–2024 | Puntos | Tratamiento / exposición espacial | 🟢 Auditado |
| 09a | INFOVIAL — velocidad | ene. 2016 | Vialidad/sentido/tramo | Caracterización basal | 🟢 Auditado |
| 09b | INFOVIAL — clasificación vehicular | ene. 2017 | Vialidad/sentido | Tráfico / composición vehicular basal | 🟢 Auditado |
| 10 | Accidentes de peatones y ciclistas | 2019 | Coordenadas | Outcome complementario | 🟢 Auditado |
| 11 | Depósitos vehiculares | abr.–dic. 2022* | Sin geografía | Fuente complementaria incierta | 🟡 Auditado — interpretación pendiente |
| 12 | Accidentes por alcaldía | 2018–2023** | Alcaldía | Validación agregada | 🟢 Auditado |
| 13 | Vehículos registrados | 1980–2020 | CDMX / tipo de vehículo | Exposición / denominador | 🟢 Auditado |

\* La fecha observada corresponde a `fecha_busqueda`; no se ha demostrado que
sea fecha de ingreso a depósito.

\** El archivo contiene únicamente un conteo acumulado por alcaldía y no una
serie temporal interna.

---

# Fuentes centrales potenciales

## 01 — Incidentes viales C5

**Estado:** 🟢 Auditado — alta utilidad

Cobertura extensa y georreferenciada de incidentes viales.

### Fortalezas

- Serie larga: 2014–febrero de 2024.
- Coordenadas.
- Alta frecuencia.
- Diferentes categorías de incidentes.
- Permite construir outcomes a distintas escalas espaciales y temporales.

### Limitaciones

- La cobertura de 2024 termina en febrero.
- La pandemia altera fuertemente los patrones de movilidad.

### Papel potencial

Principal candidato a **outcome longitudinal y espacial**.

Consultar:

`docs/03_exploracion_datos/01_incidentes_c5.md`

---

## 02 — Hechos de tránsito SSC

**Estado:** 🟢 Auditado — alta utilidad

Cobertura 2018–2023 con información detallada sobre hechos, víctimas y
vehículos.

### Fortalezas

- Coordenadas.
- Lesionados y fallecidos.
- Tipo de persona.
- Tipo de vehículo.
- Permite identificar motociclistas, peatones y ciclistas.
- Mayor detalle de severidad que C5.

### Limitaciones

- Cobertura temporal menor que C5.
- `folio` no es una llave globalmente única.

### Papel potencial

Outcome complementario principal para:

- severidad;
- accidentes de motociclistas;
- peatones;
- ciclistas;
- heterogeneidad por tipo de usuario.

Consultar:

`docs/03_exploracion_datos/02_hechos_transito_ssc.md`

---

## 05a — Infracciones generales

**Estado:** 🟢 Auditado — alta utilidad para enforcement

Cobertura:

**enero de 2020 – abril de 2023**

Aproximadamente **12.1 millones de infracciones**.

### Fortalezas

- Alta frecuencia.
- Variables normativas estables como `articulo` y `fraccion`.
- Alcaldía prácticamente completa.
- Permite separar tipos de infracción.
- El exceso de velocidad domina gran parte de la base.

### Hallazgo principal

El artículo 9, asociado con velocidad, representa aproximadamente tres cuartas
partes de las infracciones.

### Limitaciones

- Abril de 2021 está ausente.
- Cambio de estructura alrededor de mayo de 2021.
- Coordenadas prácticamente desaparecen después de abril de 2021.
- Código postal pierde mucha cobertura.
- 2020 está fuertemente afectado por la pandemia.
- La base termina en abril de 2023.

### Papel potencial

Medida de:

- intensidad de enforcement;
- composición del enforcement;
- infracciones de velocidad.

Consultar:

`docs/03_exploracion_datos/05_infracciones.md`

---

## 07 — Fotocívicas

**Estado:** 🟢 Auditado

Se identificaron dos componentes.

### Cumplimiento

Contiene registros de cumplimiento de sanciones desde 2019 hasta 2022.

Incluye información como:

- usuario;
- placa;
- fecha;
- tipo de actividad;
- estatus.

### Ubicación

Se identificaron **140 ubicaciones**:

- 113 puntos;
- 27 líneas.

### Limitaciones

- Las capas espaciales no contienen una fecha explícita de instalación.
- No debe interpretarse la fecha del archivo como fecha de entrada en operación.

### Papel potencial

- Descripción de operación de Fotocívicas.
- Medidas espaciales de exposición.
- Comparación con inventarios de radares.

Consultar:

`docs/03_exploracion_datos/07_fotocivicas.md`

---

## 08 — Radares / Fotocívicas

**Estado:** 🟢 Auditado — alta utilidad potencial

La fuente contiene inventarios anuales para:

**2019–2024**

### Estructura

- 639 registros.
- 634 geometrías utilizables.
- Aproximadamente 197 ubicaciones físicas.
- Radares fijos y móviles.
- Variación anual en la composición del inventario.

### Hallazgos

Una misma ubicación puede tener varios IDs administrativos.

El matching 2023–2024 muestra:

- 53.16% de ubicaciones 2024 a ≤10 m de una ubicación 2023.
- 63.29% a ≤25 m.
- 72.15% a ≤50 m.
- 77.22% a ≤100 m.
- 18 ubicaciones 2024 a >100 m de cualquier punto de 2023.

### Limitaciones

- `anio` representa aparición en el inventario, no necesariamente instalación.
- Los IDs pueden cambiar entre años.
- La continuidad debe evaluarse por ubicación física, no solo por ID.

### Papel potencial

Es la fuente más prometedora para construir:

- exposición espacial;
- entradas/salidas de infraestructura;
- posibles tratamientos alrededor de radares.

Consultar:

`docs/03_exploracion_datos/08_radares.md`

---

# Fuentes complementarias

## 09 — INFOVIAL

**Estado:** 🟢 Auditado — uso complementario

### Velocidad

Cobertura:

**enero de 2016**

- 255,192 observaciones.
- Información horaria.
- Aproximadamente 343 combinaciones vialidad × sentido × ubicación.

### Clasificación vehicular

Cobertura:

**enero de 2017**

- 1,531,152 filas.
- 255,192 observaciones base.
- 6 categorías vehiculares.

### Fortalezas

Permite medir:

- velocidad;
- flujo;
- composición vehicular.

### Limitaciones

- No existe serie continua 2016–2019.
- Velocidad contiene outliers severos.
- No hay coordenadas explícitas.

### Papel potencial

Caracterización basal de vialidades y posible heterogeneidad.

Consultar:

`docs/03_exploracion_datos/09_infovial.md`

---

## 10 — Accidentes de peatones y ciclistas

**Estado:** 🟢 Auditado — complementario

Cobertura exclusiva:

**2019**

### Ciclistas

- 686 eventos.
- 730 lesionados.
- 11 occisos.

### Peatones

- 3,971 eventos.
- 4,069 lesionados.
- 173 occisos.

### Fortalezas

- Fecha y hora.
- Coordenadas exactas.
- Severidad.
- Vehículos involucrados.
- Calidad espacial alta.

### Limitación principal

Solo cubre un año.

### Papel potencial

- Outcome específico para usuarios vulnerables.
- Validación espacial.
- Análisis de proximidad a Fotocívicas/radares.
- Descriptivos de atropellamientos y ciclistas.

Consultar:

`docs/03_exploracion_datos/10_accidentes_peatones_ciclistas.md`

---

## 13 — Vehículos registrados

**Estado:** 🟢 Auditado — útil como exposición

Cobertura:

**1980–2020**

### Categorías

- Automóviles.
- Camiones para pasajeros.
- Camiones y camionetas para carga.
- Motocicletas.

### Hallazgo principal

El parque de motocicletas crece fuertemente durante la década de 2010.

Por ejemplo:

| Año | Motocicletas |
| ---: | ---: |
| 2012 | 59,130 |
| 2014 | 210,020 |
| 2016 | 300,587 |
| 2018 | 406,671 |
| 2020 | 497,481 |

Su participación en el parque vehicular aumenta aproximadamente de 1.3% a más
de 8% entre 2012 y 2020.

### Limitaciones

- Faltan datos de motocicletas en 2010 y 2011.
- La serie termina en 2020.
- Solo existe agregación CDMX.

### Papel potencial

- Denominador para tasas de accidentes.
- Control aproximado de exposición.
- Contexto sobre crecimiento del parque de motocicletas.

Consultar:

`docs/03_exploracion_datos/13_vehiculos_registrados.md`

---

# Fuentes de referencia o baja utilidad para el diseño principal

## 06 — Fotomultas pre-2019

**Estado:** 🟡 Parcial

La capa contiene:

- 111 registros originales;
- 13 geometrías vacías;
- 98 geometrías utilizables.

Las ubicaciones válidas caen dentro de la Ciudad de México.

### Papel potencial

Infraestructura histórica previa a Fotocívicas.

### Pendiente

- documentar mejor el periodo;
- comparar espacialmente con Fotocívicas y radares posteriores.

---

## 11 — Depósitos vehiculares

**Estado:** 🟡 Auditado — interpretación pendiente

Contiene:

- 12,581 placas únicas;
- consultas realizadas entre abril y diciembre de 2022;
- información administrativa de vehículo, padrón, tarjeta, seguro,
  infracciones y sanciones.

### Problema principal

No existe información explícita sobre:

- depósito;
- fecha de remisión;
- motivo;
- ubicación.

El mecanismo de selección de las placas es desconocido.

### Papel potencial

Bajo hasta recuperar documentación de origen.

---

## 12 — Accidentes por alcaldía

**Estado:** 🟢 Auditado — referencia agregada

La base contiene:

- 16 alcaldías;
- una fila total CDMX;
- 10,673 accidentes acumulados.

No contiene dimensión temporal ni coordenadas.

### Papel potencial

- Validación de agregados.
- Referencia descriptiva.

Su utilidad para identificación es muy baja porque C5 y SSC contienen
información mucho más granular.

---

# Fuentes pendientes

## 05b — Infracciones de parquímetros

**Cobertura conocida:** 2025.

Pendiente evaluar:

- unidad de observación;
- fechas;
- ubicación;
- tipo de infracción;
- relación con enforcement vial general.

---

# Clasificación preliminar de fuentes

## Núcleo potencial del diseño

Las fuentes que actualmente parecen más prometedoras son:

1. **C5** — outcome longitudinal principal.
2. **SSC** — severidad y heterogeneidad.
3. **Radares** — exposición/tratamiento espacial.
4. **Fotocívicas** — infraestructura y contexto institucional.
5. **Infracciones generales** — intensidad y composición del enforcement.

## Complementarias importantes

- Vehículos registrados.
- INFOVIAL.
- Accidentes de peatones y ciclistas.
- Fotomultas pre-2019.

## Referencia o baja prioridad

- Accidentes agregados por alcaldía.
- Depósitos vehiculares, mientras no se aclare su procedencia.

---

# Estado actual de la auditoría

La primera revisión sistemática de las fuentes principales se encuentra
prácticamente terminada.

El siguiente paso ya no consiste en explorar archivos individualmente, sino en
comparar los posibles diseños empíricos utilizando criterios comunes:

1. fecha del evento;
2. disponibilidad de periodo pre;
3. disponibilidad de periodo post;
4. variación espacial;
5. tratamiento observable;
6. outcome disponible;
7. posibilidad de construir tendencias pretratamiento;
8. principales amenazas de identificación;
9. controles o medidas de exposición disponibles.

A partir de esta comparación se seleccionará el evento y se construirá la base
analítica final.
# 10 — Accidentes de peatones y ciclistas

## Descripción

La fuente contiene registros georreferenciados de hechos de tránsito que
involucran peatones y ciclistas en la Ciudad de México durante 2019.

Los datos se distribuyen en dos capas espaciales independientes:

- accidentes de ciclistas;
- accidentes de peatones.

Cada registro representa un evento de tránsito y contiene información temporal,
espacial, vehicular y de severidad.

---

## Cobertura temporal

Ambas capas corresponden exclusivamente a 2019.

| Fuente | Inicio | Fin | Eventos |
| --- | --- | --- | ---: |
| Ciclistas | 2019-01-02 | 2019-12-31 | 686 |
| Peatones | 2019-01-01 | 2019-12-31 | 3,971 |

Los doce meses del año se encuentran representados en ambas fuentes.

La cobertura está limitada a un solo año, por lo que la fuente no permite por
sí misma construir un panel longitudinal amplio o realizar comparaciones
pre/post de largo plazo.

---

## Unidad de observación

La unidad de observación corresponde a un evento de tránsito georreferenciado.

Las variables incluyen:

- folio;
- fecha;
- año;
- mes;
- hora;
- condición;
- tipo de evento;
- coordenadas;
- calles o puntos de referencia;
- colonia;
- alcaldía;
- vehículos involucrados;
- marcas de vehículos;
- lesiones;
- edad de lesionados u occisos;
- número de lesionados;
- número de occisos;
- identidad de la persona;
- unidad médica;
- traslado;
- hospital;
- prioridad.

La fuente permite, por tanto, distinguir no solamente la ocurrencia del evento,
sino también parte de su severidad y de los vehículos involucrados.

---

## Geografía

Las dos capas utilizan geometrías de tipo `POINT`.

### Ciclistas

- 686 observaciones;
- 686 geometrías válidas;
- 0 geometrías vacías.

Bounding box en WGS84:

- longitud: -99.30 a -98.97;
- latitud: 19.17 a 19.56.

### Peatones

- 3,971 observaciones;
- 3,971 geometrías válidas;
- 0 geometrías vacías.

Bounding box en WGS84:

- longitud: -99.33 a -98.96;
- latitud: 19.15 a 19.57.

Los rangos espaciales son compatibles con la Ciudad de México.

Además, las coordenadas almacenadas como atributos coinciden exactamente con
las coordenadas de las geometrías para todas las observaciones.

No se encontraron coordenadas faltantes.

---

## Cobertura territorial

Las 16 alcaldías de la Ciudad de México aparecen en ambas capas.

Las mayores concentraciones de eventos se encuentran en:

### Ciclistas

- Cuauhtémoc: 156;
- Miguel Hidalgo: 81;
- Benito Juárez: 74;
- Iztapalapa: 69;
- Coyoacán: 54.

### Peatones

- Cuauhtémoc: 764;
- Iztapalapa: 592;
- Gustavo A. Madero: 363;
- Miguel Hidalgo: 329;
- Coyoacán: 314.

Estas diferencias corresponden a conteos brutos y no deben interpretarse como
medidas de riesgo sin controlar por población, movilidad o exposición vial.

---

## Severidad

### Ciclistas

Se registran:

- 686 eventos;
- 675 eventos clasificados como `LESIONADO`;
- 11 eventos clasificados como `OCCISO`;
- 730 lesionados;
- 11 occisos.

Los eventos con condición `OCCISO` contienen exactamente 11 fallecidos y
ningún lesionado registrado.

### Peatones

Se registran:

- 3,971 eventos;
- 3,798 eventos clasificados como `LESIONADO`;
- 173 eventos clasificados como `OCCISO`;
- 4,069 lesionados;
- 173 occisos.

Los 173 eventos clasificados como `OCCISO` contienen exactamente 173
fallecidos.

Diez de estos eventos también registran personas lesionadas, con un total de
21 lesionados, lo que indica que un mismo evento puede involucrar múltiples
víctimas con distintos niveles de severidad.

Por esta razón, una fila debe interpretarse como un evento y no necesariamente
como una víctima individual.

---

## Tipos de evento

### Ciclistas

La distribución observada es:

| Condición | Tipo de evento | Eventos |
| --- | --- | ---: |
| Lesionado | Choque | 579 |
| Lesionado | Caída de ciclista | 96 |
| Occiso | Choque | 11 |

### Peatones

La gran mayoría corresponde a atropellamientos:

| Condición | Tipo de evento | Eventos |
| --- | --- | ---: |
| Lesionado | Atropellado | 3,783 |
| Occiso | Atropellado | 169 |
| Lesionado | Choque | 15 |
| Occiso | Choque | 2 |
| Occiso | Caída de pasajero | 1 |
| Occiso | Volcadura | 1 |

Esto hace especialmente atractiva la capa de peatones para estudiar eventos
directamente relacionados con la interacción entre vehículos y usuarios
vulnerables.

---

## Identificadores y duplicados

### Ciclistas

- 686 filas;
- 686 folios distintos;
- 0 folios faltantes;
- 0 filas completamente duplicadas.

### Peatones

- 3,971 filas;
- 3,970 folios distintos;
- 0 folios faltantes;
- 0 filas completamente duplicadas.

Existe un folio repetido:

`991661`

Sin embargo, las dos observaciones corresponden a eventos distintos:

- diferentes fechas;
- diferentes horas;
- diferentes ubicaciones;
- diferentes alcaldías;
- diferentes características del evento.

Por tanto, no constituye una duplicación de registros.

El campo `no_folio` no debe utilizarse como llave única estricta sin incorporar
información adicional como fecha, hora o ubicación.

---

## Calidad de los datos

La calidad espacial de la fuente es alta:

- no existen geometrías vacías;
- todas las geometrías son válidas;
- todas son puntos;
- no existen coordenadas faltantes;
- coordenadas de atributos y geometría coinciden exactamente.

También existe una alta consistencia entre la variable `condicion` y los
conteos de lesionados y occisos.

Se observan problemas menores de codificación de caracteres en algunos campos
de texto, por ejemplo nombres de días y hospitales.

Estos problemas deberán corregirse durante la etapa de limpieza, pero no
afectan la información espacial ni las principales variables cuantitativas.

---

## Limitaciones

La principal limitación es temporal.

La fuente contiene exclusivamente eventos de 2019, por lo que no permite
construir por sí misma una serie pre/post extensa alrededor de Fotocívicas.

También deben considerarse:

1. posible reutilización o error en algunos folios;
2. problemas menores de codificación de caracteres;
3. una fila representa un evento y puede contener múltiples víctimas;
4. los conteos espaciales brutos no representan directamente tasas de riesgo.

---

## Papel potencial en la tesis

La fuente puede funcionar como outcome complementario especializado en usuarios
vulnerables.

Su principal ventaja frente a fuentes agregadas es la combinación de:

- georreferenciación exacta;
- fecha y hora;
- tipo de usuario vulnerable;
- severidad;
- vehículos involucrados.

Esto permitiría cruzar los eventos espacialmente con las ubicaciones de radares
y Fotocívicas.

Sin embargo, al disponer únicamente de 2019, su utilidad para identificación
causal longitudinal es limitada.

Sus usos más prometedores son:

- caracterización espacial de accidentes de usuarios vulnerables;
- validación de patrones encontrados en C5 o SSC;
- análisis de proximidad a radares/Fotocívicas;
- análisis específico de atropellamientos;
- análisis descriptivo de severidad;
- caracterización del primer año de Fotocívicas.

---

## Evaluación

**Estado:** 🟢 Auditado — utilizable como fuente complementaria.

**Cobertura temporal:** 2019.

**Granularidad temporal:** Evento, con fecha y hora.

**Granularidad espacial:** Punto georreferenciado.

**Unidad de observación:** Evento de tránsito.

**Principal fortaleza:** Identificación específica de peatones y ciclistas con
información espacial y de severidad.

**Principal limitación:** Cobertura restringida a un solo año.

**Utilidad para el análisis principal:** Media-baja.

**Utilidad para análisis complementarios:** Alta.

---

## Conclusión

La fuente presenta una calidad espacial alta y una estructura rica a nivel de
evento.

Los registros de peatones son especialmente relevantes debido a que casi todos
corresponden a atropellamientos y contienen información sobre severidad,
ubicación y momento del evento.

No obstante, la cobertura exclusiva de 2019 impide utilizar esta fuente como
base longitudinal principal de la estrategia empírica.

Se conservará como fuente complementaria para estudiar usuarios vulnerables,
realizar validaciones espaciales y explorar la relación entre accidentes y la
infraestructura de control de velocidad.
# 02 — Hechos de tránsito SSC

**Estado:** 🟢 Utilizable

## Cobertura

- Periodo: 2018-01-01 a 2023-12-31.
- Unidad original: hecho de tránsito.
- Tabla principal: 134,079 registros.

## Estructura

El conjunto contiene una tabla principal y varias tablas auxiliares:

- Hechos de tránsito.
- Personas lesionadas o fallecidas por edad.
- Personas lesionadas o fallecidas por sexo.
- Personas lesionadas o fallecidas por tipo de persona.
- Vehículos involucrados.

Las tablas de hechos, edad, sexo y vehículos cubren 2018–2023.

La tabla de tipo de persona cubre 2020–2023.

## Geografía

La tabla principal contiene:

- latitud;
- longitud;
- alcaldía;
- colonia;
- características de vialidad e intersección.

## Información disponible

### Hecho

Permite identificar:

- fecha;
- hora;
- tipo de evento;
- lesionados;
- fallecidos;
- ubicación;
- características de la vialidad.

### Personas

Las tablas auxiliares permiten estudiar:

- edad;
- sexo;
- condición de la persona;
- tipo de persona.

Esto permite identificar grupos como peatones, ciclistas y motociclistas.

### Vehículos

Existe información sobre los vehículos involucrados en cada hecho de tránsito.

## Identificación

El campo `folio` no constituye un identificador globalmente único.

La auditoría produjo:

| Llave | Llaves únicas | Llaves repetidas | Filas excedentes |
|---|---:|---:|---:|
| folio | 126,473 | 219 | 7,606 |
| folio + fecha | 127,141 | 381 | 6,938 |
| folio + fecha + hora | 133,963 | 113 | 116 |
| folio + fecha + hora + latitud + longitud | 134,078 | 1 | 1 |

La combinación de folio, fecha, hora y coordenadas identifica de manera
prácticamente única los 134,079 registros.

### Conflicto restante

Dos observaciones comparten:

- folio;
- fecha;
- hora;
- latitud;
- longitud.

Sin embargo, difieren en:

| Variable | Observación 1 | Observación 2 |
|---|---|---|
| `tipo_evento` | CAIDA DE PASAJERO | DERRAPADO |
| `unidad_medica_de_apoyo` | SOPORTE VITAL | CRUZ ROJA |

No son filas completamente duplicadas.

No existe evidencia suficiente para eliminar alguna de las observaciones, por
lo que ambas deberán conservarse.

Durante el procesamiento se generará un identificador interno `id_hecho`.

## Tablas auxiliares

Todos los folios presentes en las tablas auxiliares aparecen también en la
tabla principal.

Sin embargo, debido a que `folio` no es globalmente único, no deben realizarse
joins utilizando únicamente esta variable sin estudiar previamente la llave
de unión adecuada.

Además, las tablas auxiliares pueden contener múltiples observaciones por
hecho. Deben agregarse al nivel correspondiente antes de realizar joins para
evitar multiplicación accidental de registros.

## Calidad

- No se detectaron problemas de parsing en la lectura completa.
- No existen filas completamente duplicadas en la tabla principal.
- Existen valores genéricos de folio como `SD`, `SIN FOLIO` y `PM`.

## Outcomes potenciales

La fuente permite construir:

- total de hechos;
- hechos con lesionados;
- número de lesionados;
- hechos con fallecidos;
- número de fallecidos;
- hechos por tipo;
- hechos relacionados con motocicletas;
- hechos relacionados con ciclistas;
- hechos relacionados con peatones;
- hechos por tipo de vehículo.

## Utilidad para la tesis

**Alta.**

SSC complementa a C5 proporcionando información considerablemente más rica
sobre severidad, personas afectadas y vehículos involucrados.

Su cobertura temporal es menor que C5, pero permite estudiar heterogeneidad
de los efectos sobre distintos tipos de usuarios de la vía.

## Script de auditoría

`syntax/01_exploracion/103_explorar_hechos_ssc.R`
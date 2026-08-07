# 09 — INFOVIAL

## Descripción

INFOVIAL contiene información de monitoreo del tránsito vehicular en vialidades
de la Ciudad de México.

Los archivos disponibles proporcionan dos productos distintos:

1. mediciones horarias de velocidad;
2. clasificación y conteo vehicular por tipo de vehículo.

Aunque inicialmente la fuente se consideró como una posible serie para el
periodo 2016–2019, los archivos disponibles corresponden únicamente a cortes
mensuales específicos.

---

## Archivos disponibles

### Velocidad

Archivo:

`201601_velocidad_limpio.csv`

Cobertura:

**1–31 de enero de 2016**

Variables principales:

- `id`
- `fecha`
- `vialidad`
- `sentido`
- `ubicacion`
- `velocidad`

La base contiene:

- 255,192 observaciones;
- información horaria;
- 26 vialidades;
- aproximadamente 343 combinaciones de vialidad, sentido y ubicación.

Las ubicaciones con cobertura completa presentan:

**744 observaciones = 31 días × 24 horas**

Esto confirma que la granularidad temporal de la fuente es horaria.

La variable `id` de esta base es prácticamente única por observación y no debe
interpretarse como identificador de sensor.

---

### Clasificación vehicular

Archivo:

`201701_clasificacion_limpio.csv`

Cobertura:

**1–31 de enero de 2017**

Variables principales:

- `fecha`
- `hora`
- `id`
- `vialidad`
- `sentido`
- `tipo_vehiculo`
- `cantidad`

La base contiene:

- 1,531,152 filas;
- 255,192 observaciones base;
- 26 identificadores de corredor;
- seis categorías de vehículo.

Cada observación base aparece exactamente seis veces, una por cada categoría
vehicular.

Por tanto:

`255,192 × 6 = 1,531,152`

La estructura puede representarse como:

`corredor → vialidad/tramo → sentido → fecha/hora → tipo de vehículo`

Los valores de `id` parecen identificar grandes corredores o conjuntos de
vialidades y no observaciones individuales.

---

## Granularidad temporal

Ambas fuentes tienen granularidad horaria.

Sin embargo, la cobertura disponible es limitada:

| Producto | Periodo disponible |
| --- | --- |
| Velocidad | Enero de 2016 |
| Clasificación vehicular | Enero de 2017 |

Por lo tanto, los archivos disponibles no permiten construir una serie
longitudinal continua entre 2016 y 2019.

---

## Granularidad espacial

La base de velocidad contiene información de:

- vialidad;
- sentido;
- ubicación o tramo.

Se identificaron aproximadamente 343 combinaciones de:

`vialidad × sentido × ubicación`

Las ubicaciones están descritas mediante nombres de calles e intersecciones,
pero los archivos explorados no contienen coordenadas geográficas explícitas.

La base de clasificación utiliza una estructura relacionada basada en
corredores, vialidades y sentidos.

La georreferenciación de estos tramos requeriría un procesamiento adicional si
se quisiera cruzar INFOVIAL con radares, Fotocívicas o incidentes C5.

---

## Calidad de la variable velocidad

La distribución general de velocidad presenta valores plausibles en la mayor
parte de la base, pero también contiene observaciones problemáticas.

Se encontraron:

- 255,192 observaciones;
- 25,171 observaciones con velocidad igual a cero;
- 9.86% de las observaciones con velocidad cero;
- 4,712 observaciones superiores a 100 km/h;
- 1,371 superiores a 150 km/h;
- 848 superiores a 200 km/h;
- aproximadamente 0.54% superiores a 150 km/h.

También se observaron valores extremos claramente incompatibles con velocidades
vehiculares reales, incluyendo un máximo superior a 12,000 km/h.

Por tanto, la variable `velocidad` requiere un procedimiento explícito de
limpieza antes de cualquier análisis sustantivo.

Los valores iguales a cero tampoco deben eliminarse automáticamente, ya que
todavía no se ha determinado si representan:

- ausencia de circulación;
- congestión;
- ausencia de medición;
- alguna codificación particular del sistema.

---

## Clasificación vehicular

La base de enero de 2017 contiene seis categorías vehiculares:

- C1;
- C2;
- C3;
- C4;
- C5;
- C6.

Cada observación base contiene las seis categorías.

La variable `cantidad` presenta un patrón horario consistente con el flujo
vehicular esperado:

- niveles bajos durante la madrugada;
- crecimiento durante las primeras horas de la mañana;
- altos niveles durante las horas de actividad diurna;
- reducción durante la noche.

Esto proporciona evidencia preliminar de consistencia interna para interpretar
`cantidad` como una medida de conteo o flujo vehicular.

La interpretación sustantiva exacta de C1–C6 deberá documentarse antes de
utilizar la composición vehicular.

---

## Limitaciones

1. Los archivos disponibles no proporcionan una serie continua 2016–2019.

2. Velocidad y clasificación vehicular corresponden a meses y años distintos.

3. La base no contiene coordenadas geográficas explícitas.

4. La variable de velocidad contiene valores extremos que requieren limpieza.

5. No se ha determinado todavía el significado de los valores de velocidad
   iguales a cero.

6. Las categorías C1–C6 requieren documentación adicional para conocer qué tipo
   de vehículo representa cada una.

7. Los identificadores tienen significados distintos entre los dos productos y
   no deben interpretarse automáticamente como identificadores de sensores.

---

## Papel potencial en la tesis

INFOVIAL no parece adecuada como fuente longitudinal principal para evaluar el
efecto temporal de Fotocívicas u otras políticas de seguridad vial.

Su principal utilidad potencial es como fuente complementaria para caracterizar
las condiciones iniciales de las vialidades.

En particular, podría utilizarse para construir medidas históricas de:

- velocidad basal;
- intensidad o flujo vehicular;
- composición vehicular;
- características de corredores.

Si posteriormente se logra georreferenciar las ubicaciones, estas medidas
podrían cruzarse con:

- radares;
- Fotocívicas;
- incidentes C5;
- hechos de tránsito SSC.

Esto permitiría explorar heterogeneidad de los efectos según las condiciones
previas de tránsito.

---

## Evaluación

**Estado:** 🟢 Auditado — uso complementario

**Utilidad para el análisis principal:** Media-baja.

**Utilidad como control o caracterización basal:** Media-alta.

**Granularidad temporal:** Horaria.

**Granularidad espacial:** Vialidad, sentido y ubicación/tramo.

**Principal fortaleza:** Medición directa de velocidad y flujo vehicular antes
de la implementación de Fotocívicas.

**Principal limitación:** Solo se dispone de enero de 2016 para velocidad y
enero de 2017 para clasificación vehicular.

---

## Conclusión

INFOVIAL proporciona información de alta granularidad sobre las condiciones de
circulación en determinados corredores de la Ciudad de México.

Sin embargo, la cobertura temporal disponible es demasiado corta para utilizar
la fuente como serie longitudinal principal.

Se conservará como fuente complementaria para una posible caracterización basal
de las vialidades y para futuros análisis de heterogeneidad, especialmente si
es posible georreferenciar sus ubicaciones y vincularlas con la infraestructura
de control de velocidad y las bases de siniestros viales.
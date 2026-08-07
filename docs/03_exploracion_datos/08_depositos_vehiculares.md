# 11 — Depósitos vehiculares

## Descripción

La fuente contiene información de 12,581 placas vehiculares consultadas durante
2022.

A pesar del nombre del archivo (`depositos_vehiculares_preliminar.csv`), la
estructura disponible no contiene información explícita sobre depósitos
vehiculares, fechas de ingreso, motivos de remisión o ubicación del depósito.

La base parece corresponder a un conjunto de placas posteriormente enriquecido
mediante consultas a registros administrativos vehiculares.

---

## Cobertura

La variable `fecha_busqueda` cubre el periodo:

- inicio: 2022-04-05;
- fin: 2022-12-01;
- fechas distintas de consulta: 241.

Las consultas presentan observaciones prácticamente diarias durante el periodo.

No debe interpretarse esta ventana como cobertura temporal de eventos de
tránsito o ingresos a depósitos, ya que `fecha_busqueda` parece corresponder
al momento en que se consultó la información de la placa.

---

## Unidad de observación

La base contiene:

- 12,581 filas;
- 12,581 placas únicas;
- 12,581 combinaciones únicas de placa y fecha.

Por tanto:

**1 fila = 1 placa/vehículo consultado.**

No existe seguimiento repetido de una misma placa en el tiempo.

---

## Variables disponibles

La base contiene 22 variables relacionadas principalmente con información
administrativa del vehículo:

- fecha de búsqueda;
- placa;
- identificación de placa CDMX;
- estatus en padrón;
- servicio;
- estatus del registro;
- marca;
- línea;
- clase de vehículo;
- tipo de vehículo;
- modelo;
- fecha de alta;
- combustible;
- tipo de servicio;
- estatus de tarjeta de circulación;
- expedición y expiración de tarjeta;
- estatus de seguro;
- adeudos de tenencia;
- estatus de infracciones;
- número de infracciones;
- número de sanciones.

No contiene variables explícitas de:

- depósito vehicular;
- fecha de remisión;
- motivo de remisión;
- ubicación;
- alcaldía;
- coordenadas.

---

## Padrón vehicular

De las 12,581 placas:

| Tipo | Vehículos |
| --- | ---: |
| Placa no CDMX | 6,247 |
| Placa CDMX | 6,334 |

Entre las placas CDMX:

| Estatus | Vehículos |
| --- | ---: |
| Encontrado en padrón | 3,677 |
| No encontrado en padrón | 1,789 |
| No aplica | 864 |
| Error de búsqueda | 4 |

La disponibilidad de características vehiculares depende casi completamente
de que la placa haya sido encontrada en el padrón.

Los 3,677 registros encontrados en padrón tienen información vehicular
disponible.

---

## Características administrativas

### Tarjeta de circulación

| Estatus | Vehículos | % |
| --- | ---: | ---: |
| Vigente | 2,913 | 23.15 |
| No vigente | 764 | 6.07 |
| No aplica | 8,900 | 70.74 |
| Error de búsqueda | 4 | 0.03 |

### Seguro

| Estatus | Vehículos | % |
| --- | ---: | ---: |
| Tiene seguro | 1,744 | 13.86 |
| No tiene seguro | 1,767 | 14.04 |
| No aplica | 8,904 | 70.77 |
| Error de búsqueda | 166 | 1.32 |

### Tenencia

- 7,844 vehículos (62.35%) presentan adeudos;
- 4,733 (37.62%) no presentan adeudos;
- 4 registros no tienen información.

---

## Infracciones

De las 12,581 placas:

- 10,348 aparecen sin adeudos de infracciones;
- 2,229 presentan infracciones;
- 4 no tienen información.

Esto equivale a aproximadamente 17.72% de la muestra con infracciones.

En total se registran 8,837 infracciones.

Distribución:

- media: 0.70 infracciones por placa;
- mediana: 0;
- percentil 90: 1;
- percentil 95: 3;
- máximo: 329.

La distribución presenta una cola derecha pronunciada, con un pequeño número
de placas acumulando muchas infracciones.

---

## Sanciones

Se registran:

- 381 sanciones;
- 134 vehículos con al menos una sanción;
- 12,442 sin sanciones.

La media es 0.03 sanciones por vehículo y el máximo observado es 25.

Las sanciones son considerablemente menos frecuentes que las infracciones.

---

## Motocicletas

Entre los vehículos para los que existe clasificación vehicular utilizable:

- 2,541 corresponden a otros vehículos;
- 1,139 corresponden a motocicletas o vehículos equivalentes.

Las motocicletas representan aproximadamente **30.95%** de este subconjunto.

La categoría incluye:

- motocicletas;
- motonetas;
- cuatrimotos;
- trimotos.

### Infracciones

| Grupo | Vehículos | % con infracciones | Media de infracciones |
| --- | ---: | ---: | ---: |
| Otros vehículos | 2,541 | 10.35% | 0.244 |
| Motocicletas | 1,139 | 28.76% | 0.558 |

En esta muestra, las motocicletas presentan infracciones con mayor frecuencia.

Sin embargo, esta diferencia **no debe interpretarse como una comparación
poblacional ni causal**, ya que se desconoce el mecanismo mediante el cual las
placas fueron incorporadas a la base.

### Sanciones

No se registran sanciones entre las 1,139 motocicletas clasificadas.

Entre los otros vehículos se registran 141 sanciones.

Esta diferencia requiere conocer la definición institucional de `sanciones`
antes de interpretarse.

---

## Principal problema de la fuente

La principal limitación no es técnica sino conceptual.

El nombre del archivo sugiere una relación con depósitos vehiculares, pero las
variables disponibles no permiten verificar directamente que las 12,581 placas
correspondan a vehículos remitidos a depósitos.

Tampoco es posible determinar a partir del archivo:

- por qué una placa entra en la muestra;
- cuándo ocurrió una posible remisión;
- a qué depósito fue enviada;
- cuál fue la causa;
- dónde ocurrió el evento.

Por tanto, el mecanismo de selección de la muestra permanece desconocido.

---

## Papel potencial en la tesis

La fuente no parece adecuada como outcome principal.

No proporciona:

- una serie longitudinal de eventos;
- localización espacial;
- fecha del posible evento de remisión;
- exposición directa a radares o Fotocívicas.

Podría resultar útil como fuente complementaria si posteriormente se confirma
que las placas corresponden a vehículos remitidos a depósitos.

En particular, la elevada presencia de motocicletas y sus características
administrativas podrían resultar relevantes para estudiar la población de
vehículos afectada por acciones de enforcement.

Sin embargo, cualquier análisis de este tipo requiere primero documentar el
origen y mecanismo de selección de las placas.

---

## Evaluación

**Estado:** 🟡 Auditado — interpretación pendiente.

**Cobertura observada:** abril–diciembre de 2022.

**Granularidad temporal:** fecha de consulta, no necesariamente fecha de evento.

**Granularidad espacial:** ninguna.

**Unidad de observación:** placa/vehículo consultado.

**Principal fortaleza:** información administrativa detallada del vehículo.

**Principal limitación:** mecanismo de selección desconocido y ausencia de
información explícita sobre depósitos.

**Utilidad para el análisis principal:** Baja.

**Utilidad potencial como fuente complementaria:** Pendiente de documentar su
procedencia.

---

## Conclusión

La fuente contiene información administrativa detallada para una muestra de
12,581 placas vehiculares, pero no permite identificar directamente eventos de
ingreso a depósitos vehiculares.

La variable temporal disponible corresponde aparentemente a la fecha de
consulta de cada placa y no existe información espacial ni de remisión.

Aunque se observan patrones potencialmente interesantes —particularmente una
alta presencia de motocicletas y una mayor frecuencia de infracciones entre
ellas—, estos resultados no deben interpretarse sin conocer el mecanismo de
selección de la muestra.

La fuente se conserva como complementaria y queda pendiente recuperar o
verificar su documentación de origen.
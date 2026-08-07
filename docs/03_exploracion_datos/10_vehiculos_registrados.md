# 13 — Vehículos registrados en circulación

## Descripción

La fuente contiene información anual sobre vehículos registrados en circulación
en la Ciudad de México, desagregada por tipo de vehículo.

La base permite observar la evolución de cuatro categorías:

- automóviles;
- camiones para pasajeros;
- camiones y camionetas para carga;
- motocicletas.

A diferencia de otras fuentes auxiliares, esta base puede utilizarse como una
medida aproximada de exposición vehicular a través del tiempo.

---

## Cobertura temporal

La cobertura observada es:

**1980–2020**

La base contiene 41 años distintos.

El nombre original del archivo hacía referencia a 1990–2019, pero la inspección
directa de los datos muestra una cobertura efectiva de 1980 a 2020.

---

## Estructura

La unidad de observación es:

**año × tipo de vehículo**

La base contiene:

- 41 años;
- 4 tipos de vehículo;
- 164 combinaciones año-tipo.

Existe exactamente una fila por combinación de año y tipo de vehículo.

Por tanto, la estructura corresponde a un panel anual balanceado en términos
de presencia de filas.

---

## Variables

La base contiene tres variables originales:

- `Año`;
- `Tipo de vehiculo`;
- `Numero de registro`.

`Numero de registro` fue importada originalmente como texto y posteriormente
convertida a formato numérico para el análisis.

---

## Categorías vehiculares

Las cuatro categorías disponibles son:

1. Automóviles.
2. Camiones para pasajeros.
3. Camiones y camionetas para carga.
4. Motocicletas.

Esta clasificación permite separar explícitamente el parque de motocicletas
del resto de los vehículos.

---

## Calidad de los datos

La estructura año-tipo es completa y no presenta combinaciones duplicadas.

Sin embargo, se identificó una anomalía en la serie de motocicletas:

- 2010: sin dato;
- 2011: sin dato.

Por tanto, aunque existen filas para estos años, no existe un conteo utilizable
de motocicletas.

Estos valores no fueron interpolados ni imputados durante la etapa de
exploración.

---

## Evolución de las motocicletas

La serie muestra un crecimiento considerable del parque de motocicletas,
particularmente durante la década de 2010.

Algunos valores relevantes son:

| Año | Motocicletas registradas |
| ---: | ---: |
| 2012 | 59,130 |
| 2013 | 91,324 |
| 2014 | 210,020 |
| 2015 | 260,099 |
| 2016 | 300,587 |
| 2017 | 347,851 |
| 2018 | 406,671 |
| 2019 | 473,576 |
| 2020 | 497,481 |

Entre 2012 y 2020, el número de motocicletas registradas pasó de
aproximadamente 59 mil a casi 500 mil.

Esto representa un incremento superior a ocho veces el nivel observado al
inicio del periodo.

---

## Participación en el parque vehicular

El crecimiento de las motocicletas no se explica únicamente por el crecimiento
general del parque vehicular.

Su participación en el total de vehículos registrados aumenta de
aproximadamente:

- 1.3% en 2012;
- 4.4% en 2014;
- 5.2% en 2015;
- 6.3% en 2017;
- 7.8% en 2019;
- 8.1% en 2020.

Por tanto, durante este periodo las motocicletas adquieren un peso
considerablemente mayor dentro del parque vehicular de la Ciudad de México.

---

## Relevancia para la tesis

Esta fuente puede desempeñar un papel importante como medida aproximada de
exposición.

Los conteos absolutos de accidentes de motociclistas no son directamente
comparables a través del tiempo si el número de motocicletas circulando cambia
sustancialmente.

Por ejemplo, un aumento en accidentes de motociclistas podría reflejar:

1. un aumento en el riesgo de accidente por motocicleta;
2. un aumento en el número de motocicletas circulando;
3. una combinación de ambos mecanismos.

La fuente permite aproximar esta distinción mediante tasas como:

**accidentes de motociclistas por 100,000 motocicletas registradas.**

Conceptualmente:

tasa de accidentes =
(accidentes de motociclistas / motocicletas registradas) × 100,000

Esto puede complementar las series de accidentes provenientes de C5 y SSC.

---

## Limitaciones

### Cobertura hasta 2020

La principal limitación para la estrategia empírica es que la serie termina en
2020.

Esto permite estudiar adecuadamente la evolución previa y los primeros años de
Fotocívicas, pero no permite construir directamente tasas de exposición para
eventos posteriores, particularmente la reforma relacionada con motocicletas
de 2023.

Sería conveniente buscar posteriormente una actualización de esta fuente que
extienda la serie después de 2020.

### Vehículos registrados ≠ vehículos circulando

El número de vehículos registrados debe interpretarse como una aproximación de
exposición.

No mide directamente:

- kilómetros recorridos;
- viajes realizados;
- intensidad de uso;
- vehículos efectivamente circulando diariamente;
- exposición espacial a radares.

Dos años con el mismo número de motocicletas registradas pueden presentar
niveles diferentes de circulación efectiva.

### Agregación espacial

La información corresponde al conjunto de la Ciudad de México.

No existe desagregación por:

- alcaldía;
- colonia;
- vialidad;
- coordenadas.

Por tanto, la fuente es adecuada para normalizar tendencias temporales
agregadas, pero no para análisis espaciales alrededor de radares.

---

## Papel potencial en la estrategia empírica

La fuente puede utilizarse principalmente de tres maneras.

### 1. Contexto descriptivo

Documentar el fuerte crecimiento del parque de motocicletas durante el periodo
analizado.

### 2. Tasas de accidentalidad

Construir indicadores como accidentes de motociclistas por cada 100,000
motocicletas registradas.

### 3. Control de exposición

Ayudar a distinguir cambios en accidentes asociados con mayor exposición de
cambios potencialmente relacionados con regulación o seguridad vial.

Su uso exacto dependerá del diseño empírico finalmente seleccionado.

---

## Evaluación

**Estado:** 🟢 Auditado — utilizable.

**Cobertura:** 1980–2020.

**Granularidad temporal:** Anual.

**Granularidad espacial:** Ciudad de México.

**Unidad de observación:** Año × tipo de vehículo.

**Tipos de vehículo:** 4.

**Principal fortaleza:** Permite medir la evolución del parque de motocicletas.

**Principal limitación:** La serie termina en 2020 y presenta valores faltantes
de motocicletas en 2010–2011.

**Utilidad para el análisis principal:** Media.

**Utilidad como variable de exposición/contexto:** Alta.

---

## Conclusión

La fuente de vehículos registrados constituye una fuente auxiliar relevante
para la tesis debido al fuerte crecimiento del parque de motocicletas en la
Ciudad de México.

Entre 2012 y 2020, las motocicletas registradas aumentaron de aproximadamente
59 mil a casi 500 mil y su participación en el parque vehicular pasó de cerca
de 1.3% a más de 8%.

Este cambio implica que las tendencias de accidentes de motociclistas no deben
analizarse exclusivamente mediante conteos absolutos.

La fuente puede utilizarse para construir tasas de accidentalidad y como
medida aproximada de exposición, especialmente en análisis agregados previos a
2021.

Se recomienda buscar posteriormente una actualización de la serie que permita
extender esta medida de exposición hacia 2023 y años posteriores.
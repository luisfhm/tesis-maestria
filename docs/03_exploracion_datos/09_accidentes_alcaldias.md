# 12 — Accidentes por alcaldía

## Descripción

La fuente contiene un conteo agregado de accidentes de tránsito por alcaldía
en la Ciudad de México.

La base es extremadamente compacta y contiene únicamente dos variables:

- `Alcaldía`;
- `Número de accidente`.

No contiene registros individuales de accidentes ni variables temporales.

---

## Estructura

La base contiene 17 filas:

- las 16 alcaldías de la Ciudad de México;
- una fila adicional denominada `CDMX`.

La fila `CDMX` corresponde al total de las 16 alcaldías y no representa una
categoría adicional.

La suma de los accidentes de las 16 alcaldías coincide exactamente con el
total reportado para CDMX:

**10,673 accidentes.**

---

## Granularidad espacial

La unidad geográfica disponible es la alcaldía.

No existen:

- coordenadas;
- colonias;
- vialidades;
- puntos de accidente.

Por tanto, la fuente solo permite comparaciones agregadas entre las 16
alcaldías.

---

## Granularidad temporal

La base no contiene ninguna variable temporal.

No existen variables de:

- fecha;
- año;
- mes;
- trimestre;
- hora.

Por tanto, los conteos representan acumulados para el periodo de referencia
del conjunto de datos.

El periodo 2018–2023 asociado previamente a esta fuente debe considerarse
únicamente como periodo de referencia reportado, no como una serie temporal
observable dentro del archivo.

---

## Distribución espacial

Se registran 10,673 accidentes en total.

Las alcaldías con mayor número de accidentes son:

| Alcaldía | Accidentes |
| --- | ---: |
| Cuauhtémoc | 1,524 |
| Gustavo A. Madero | 1,407 |
| Iztapalapa | 1,207 |
| Miguel Hidalgo | 979 |
| Benito Juárez | 837 |

Las alcaldías con menor número son:

| Alcaldía | Accidentes |
| --- | ---: |
| Milpa Alta | 95 |
| La Magdalena Contreras | 125 |
| Tláhuac | 152 |
| Xochimilco | 220 |
| Cuajimalpa de Morelos | 286 |

Estos valores son conteos absolutos y no deben interpretarse directamente como
medidas de riesgo, ya que no controlan por población, parque vehicular,
movilidad, longitud de la red vial u otras medidas de exposición.

---

## Calidad de los datos

La estructura es completa:

- 0 valores faltantes en alcaldía;
- 0 valores faltantes en número de accidentes;
- las 16 alcaldías están representadas;
- el total reportado para CDMX coincide exactamente con la suma de las
  alcaldías.

No se identificaron problemas estructurales relevantes.

---

## Limitaciones

La principal limitación es el alto nivel de agregación.

La fuente no permite identificar:

- cuándo ocurrió cada accidente;
- ubicación exacta;
- severidad;
- tipo de accidente;
- vehículos involucrados;
- peatones o ciclistas involucrados;
- lesionados u occisos.

Además, no existe una serie temporal dentro del archivo.

---

## Papel potencial en la tesis

La utilidad de esta fuente para el análisis principal es muy limitada.

Las fuentes C5 y SSC contienen registros individuales con coordenadas y
variables temporales, por lo que permiten construir directamente conteos por:

- alcaldía;
- año;
- mes;
- semana;
- zonas alrededor de radares.

Por tanto, esta fuente no aporta una granularidad adicional respecto a las
fuentes principales.

Su posible utilidad se limita a:

- validación externa de órdenes de magnitud;
- comparación descriptiva de distribución espacial;
- comprobación de agregados construidos con otras fuentes.

---

## Evaluación

**Estado:** 🟢 Auditado — utilizable únicamente como referencia agregada.

**Periodo de referencia:** 2018–2023, pendiente de confirmar con metadatos de
origen.

**Granularidad temporal:** Ninguna dentro del archivo.

**Granularidad espacial:** Alcaldía.

**Unidad de observación:** Alcaldía.

**Total reportado:** 10,673 accidentes.

**Principal fortaleza:** Cobertura completa de las 16 alcaldías y consistencia
del total.

**Principal limitación:** Ausencia completa de dimensión temporal y
georreferenciación detallada.

**Utilidad para el análisis principal:** Muy baja.

**Utilidad complementaria:** Baja.

---

## Conclusión

La fuente contiene únicamente un conteo agregado de accidentes para cada una de
las 16 alcaldías de la Ciudad de México.

Aunque los datos son internamente consistentes, su elevada agregación espacial
y ausencia de dimensión temporal limitan considerablemente su utilidad para la
estrategia empírica de la tesis.

Se conservará como fuente de referencia y posible validación externa, pero no
como insumo para la construcción del panel analítico principal.
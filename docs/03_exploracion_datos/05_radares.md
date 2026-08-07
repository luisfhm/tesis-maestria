# Exploración de radares y Fotocívicas

## Fuente

Base geográfica de ubicaciones de radares y dispositivos asociados al sistema
de control de velocidad de la Ciudad de México.

La fuente contiene información anual para el periodo:

**2019–2024**

---

## Estructura

La base contiene:

- 639 registros.
- 634 registros con geometría utilizable.
- 5 registros con geometría vacía.
- geometrías tipo `POINT`.
- información de año, tipo de dispositivo, identificadores y ubicación.

Se identificaron dos categorías principales:

- radar fijo;
- radar móvil.

La base debe interpretarse como una sucesión de inventarios anuales y no como
639 dispositivos físicos distintos.

---

## Cobertura temporal

El número de registros por año es:

| Año | Radar fijo | Radar móvil | Total |
|---:|---:|---:|---:|
| 2019 | 89 | 27 | 116 |
| 2020 | 91 | 18 | 109 |
| 2021 | 91 | 18 | 109 |
| 2022 | 87 | 18 | 105 |
| 2023 | 103 | 18 | 121 |
| 2024 | 79 | 0 | 79 |

La composición del inventario cambia a lo largo del tiempo.

En 2024 no aparecen observaciones clasificadas como radar móvil y cambia
también el formato de algunos identificadores, por lo que este año debe
compararse con los anteriores utilizando principalmente la ubicación física
y no únicamente el ID administrativo.

---

## Estructura espacial

Después de excluir las cinco geometrías vacías se identificaron
aproximadamente **197 ubicaciones espaciales** durante 2019–2024.

El número de ubicaciones por año es:

| Año | Ubicaciones |
|---:|---:|
| 2019 | 97 |
| 2020 | 88 |
| 2021 | 88 |
| 2022 | 82 |
| 2023 | 105 |
| 2024 | 79 |

Una misma ubicación puede estar asociada con múltiples identificadores.

Se encontraron **27 ubicaciones asociadas con más de un ID**, por lo que el
identificador administrativo no equivale necesariamente a una ubicación
física independiente.

---

## Persistencia espacial

La distribución de ubicaciones según el número de años en que aparecen es:

| Años observada | Ubicaciones |
|---:|---:|
| 1 | 69 |
| 2 | 40 |
| 3 | 6 |
| 4 | 38 |
| 5 | 44 |

Existe por tanto una combinación de:

- ubicaciones altamente persistentes;
- ubicaciones temporales;
- entradas y salidas del inventario.

Esta variación puede ser relevante para construir posteriormente medidas de
exposición espacial al enforcement.

---

## Comparación 2023–2024

Se realizó un matching espacial entre cada ubicación registrada en 2024 y su
ubicación más cercana en el inventario de 2023.

Los resultados fueron:

| Distancia | Radares 2024 | Porcentaje |
|---|---:|---:|
| ≤10 m | 42 | 53.16% |
| 10–25 m | 8 | 10.13% |
| 25–50 m | 7 | 8.86% |
| 50–100 m | 4 | 5.06% |
| >100 m | 18 | 22.78% |

Por lo tanto:

- 63.29% se encuentra a ≤25 m de una ubicación de 2023;
- 72.15% se encuentra a ≤50 m;
- 77.22% se encuentra a ≤100 m;
- 22.78% se encuentra a más de 100 m.

La elevada coincidencia espacial indica que una parte importante del inventario
de 2024 corresponde a ubicaciones ya presentes en 2023, aun cuando pueda haber
cambios en identificadores o características administrativas.

---

## Posibles nuevas apariciones o reubicaciones en 2024

Se identificaron **18 ubicaciones de 2024 situadas a más de 100 metros de
cualquier registro de 2023**.

Algunas presentan distancias considerablemente mayores, superiores incluso a
1 o 2 kilómetros respecto del dispositivo de 2023 más cercano.

Estas observaciones son candidatas a representar:

- nuevas ubicaciones;
- reubicaciones;
- cambios importantes en el inventario.

No deben interpretarse automáticamente como nuevas instalaciones, ya que la
variable `anio` representa la aparición dentro del inventario disponible y no
se ha demostrado que corresponda a la fecha efectiva de instalación.

---

## Limitaciones

1. La variable `anio` no puede interpretarse directamente como año de
   instalación.

2. Los identificadores administrativos pueden cambiar entre inventarios.

3. Varios identificadores pueden corresponder a una misma ubicación física.

4. Las coordenadas pueden presentar pequeñas diferencias entre años, por lo
   que la continuidad espacial debe determinarse mediante tolerancias de
   distancia y no mediante igualdad exacta.

5. La ausencia de radares móviles en 2024 puede representar un cambio en la
   clasificación o construcción de la fuente y no necesariamente la
   desaparición física de estos dispositivos.

6. Cinco registros no contienen geometría utilizable.

---

## Papel potencial en la tesis

Esta fuente tiene un potencial importante para construir medidas espaciales de
exposición al enforcement.

Puede combinarse con los incidentes viales C5 para estudiar la evolución de
los siniestros alrededor de ubicaciones con distinta exposición a radares.

La unidad relevante parece ser principalmente la **ubicación física del
dispositivo**, no el identificador administrativo.

Las entradas y salidas observadas entre inventarios podrían utilizarse
posteriormente para definir cambios de exposición, siempre que puedan
validarse temporalmente y distinguirse de simples modificaciones
administrativas.

---

## Conclusión preliminar

La base proporciona información espacial y temporal sobre la distribución de
radares en la CDMX entre 2019 y 2024.

Existe una infraestructura persistente, pero también una cantidad relevante de
cambios en las ubicaciones observadas entre años.

En particular, la comparación 2023–2024 muestra que aproximadamente tres
cuartas partes de los puntos de 2024 se encuentran a menos de 50 metros de una
ubicación de 2023, mientras que 18 puntos presentan diferencias superiores a
100 metros.

La fuente es, por tanto, una candidata importante para construir el componente
espacial del diseño empírico, pero las apariciones en el inventario no deben
interpretarse todavía como fechas de instalación.

Será necesario combinarla posteriormente con la cronología de política vial y
con las bases de incidentes para determinar qué cambios pueden utilizarse como
tratamientos plausibles.
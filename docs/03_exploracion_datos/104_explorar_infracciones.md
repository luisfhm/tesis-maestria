# Exploración de infracciones de tránsito

## Fuente

Base histórica de **Infracciones al Reglamento de Tránsito de la Ciudad de México**.

Los archivos disponibles se encuentran divididos por año y bimestre:

- 2020: 6 archivos.
- 2021: 6 archivos.
- 2022: 6 archivos.
- 2023: 2 archivos.

En total se analizaron **20 archivos CSV**.

---

## Cobertura temporal

La base cubre aproximadamente:

**enero de 2020 – abril de 2023**

Se identificó un hueco completo en:

- **abril de 2021**

La cobertura restante es continua.

### Observación sobre los archivos

El periodo indicado en el nombre del archivo no siempre coincide exactamente
con la fecha de las observaciones.

Por ejemplo, el archivo correspondiente a `2022_b1` contiene **56 registros
fechados el 1 de enero de 2023**.

Estos 56 registros:

- tienen identificadores propios;
- no aparecen duplicados en `2023_b1`;
- por lo tanto, no deben eliminarse únicamente por encontrarse en un archivo
  cuyo nombre corresponde a otro periodo.

### Regla para procesamiento

El periodo de cada observación deberá definirse utilizando:

`fecha_infraccion`

El año y bimestre indicados en el nombre del archivo se conservarán únicamente
como metadatos de procedencia.

---

## Tamaño de la fuente

La unión conceptual de los archivos contiene aproximadamente:

**12.1 millones de infracciones**

La cantidad de observaciones presenta una fuerte variación temporal.

En particular, durante 2020 se observa una caída muy pronunciada coincidente
con el periodo de restricciones de movilidad asociado a la pandemia de COVID-19.

Por esta razón, 2020 deberá tratarse con precaución como periodo de referencia
en cualquier diseño empírico posterior.

---

## Cambio de estructura de la base

Se identificaron dos esquemas principales.

### Esquema 1

Aproximadamente:

**2020 – abril de 2021**

Contiene variables como:

- `id_folio`
- `año_infraccion`
- `mes_infraccion`
- `motivacion`
- `modelo`
- `calle`
- `entre_calle_uno`
- `entre_calle_dos`

### Esquema 2

Aproximadamente desde:

**mayo de 2021**

Contiene variables como:

- `id_infraccion`
- `ao_infraccion`
- `mes`
- `categoria`
- `marca_general`
- `submarca`
- `en_la_calle`
- `entre_calle`
- `y_calle`

El cambio parece corresponder principalmente a una modificación del esquema
de publicación y no necesariamente a una ruptura en el universo de
infracciones.

---

## Variables longitudinalmente estables

A pesar del cambio de estructura, se identificaron variables presentes de
forma consistente en prácticamente todos los archivos:

- `fecha_infraccion`
- `articulo`
- `fraccion`
- `inciso`
- `parrafo`
- `placa`
- `marca`
- `colonia`
- `alcaldia`
- `codigo_postal`
- `latitud`
- `longitud`
- `color`

Para clasificar infracciones longitudinalmente, la combinación:

**artículo + fracción**

parece más adecuada que intentar homologar directamente `motivacion` y
`categoria`.

---

## Cobertura geográfica

La calidad de las variables espaciales cambia considerablemente durante el
periodo.

| Año | Coordenadas completas | Código postal | Alcaldía |
|---|---:|---:|---:|
| 2020 | 97.13% | 97.22% | 97.11% |
| 2021 | 27.56% | 36.61% | 97.99% |
| 2022 | 0% | 25.68% | 100% |
| 2023* | 0% | 22.88% | 100% |

\* 2023 únicamente contiene enero–abril.

### Ruptura espacial

Las coordenadas dejan de estar disponibles a partir de aproximadamente
**mayo de 2021**, coincidiendo con el cambio de estructura de la fuente.

Esto implica que:

- no puede construirse una georreferenciación puntual homogénea para
  2020–2023 directamente con latitud y longitud;
- `alcaldia` permanece prácticamente completa durante toda la serie;
- `colonia` también conserva una cobertura elevada;
- `codigo_postal` pierde considerable cobertura después de 2020.

La reconstrucción espacial mediante colonia, calles u otras fuentes deberá
evaluarse posteriormente durante el procesamiento.

---

## Clasificación de las infracciones

La variable `articulo` presenta una cobertura prácticamente completa y permite
identificar una fuerte concentración de las infracciones.

### Exceso de velocidad

El **artículo 9** representa aproximadamente:

**76.57% de todas las infracciones**

Las fracciones I y II del artículo 9 concentran prácticamente la totalidad de
este grupo.

En el esquema nuevo, la categoría:

**Exceder límites de velocidad**

representa aproximadamente **72.16%** de las observaciones clasificadas.

Existe, por tanto, evidencia consistente de que la fuente está fuertemente
dominada por infracciones relacionadas con **exceso de velocidad**.

---

## Evolución temporal

Se comparó mensualmente:

- infracciones asociadas al artículo 9;
- resto de las infracciones;
- participación de las infracciones de velocidad sobre el total.

Las infracciones de velocidad representan normalmente una proporción muy
elevada del total, aunque esta participación varía considerablemente a lo largo
del tiempo.

Al marcar **abril de 2022** como posible evento de política pública, la
exploración visual no muestra una discontinuidad suficientemente clara como
para asumir, únicamente a partir de esta fuente, que dicha fecha constituye
un quiebre estructural en el enforcement.

Por lo tanto, la reforma de 2022 debe mantenerse como **evento candidato**,
pero no seleccionarse todavía como evento principal de la investigación.

---

## Limitaciones

1. La fuente termina en **abril de 2023**, por lo que no permite evaluar
   directamente reformas posteriores, como cambios de septiembre de 2023
   o intervenciones de 2025.

2. Abril de 2021 está ausente.

3. Existe un cambio de estructura de variables alrededor de mayo de 2021.

4. Las coordenadas geográficas desaparecen a partir de mayo de 2021.

5. La cobertura de código postal disminuye considerablemente después de 2020.

6. El periodo de pandemia genera una alteración extraordinaria en los niveles
   de infracciones durante 2020.

7. El nombre del archivo no debe utilizarse como fuente definitiva del periodo
   temporal de una observación.

---

## Papel potencial en la tesis

La fuente parece especialmente útil para medir:

### Enforcement

- volumen de infracciones;
- intensidad temporal del enforcement;
- composición por tipo de infracción;
- enforcement asociado específicamente con velocidad.

### Dimensión espacial

La alcaldía puede utilizarse durante todo el periodo.

Un análisis espacial más granular mediante código postal o coordenadas presenta
problemas de comparabilidad después de 2020–2021 y requerirá reconstrucción o
fuentes espaciales adicionales.

### Clasificación normativa

`articulo + fraccion` constituye, preliminarmente, la clasificación longitudinal
más estable de las infracciones.

---

## Conclusión preliminar

La base de infracciones constituye una fuente importante para caracterizar el
**enforcement vial en la CDMX entre 2020 y abril de 2023**.

Su principal fortaleza es el tamaño de la base y la estabilidad de las variables
normativas, especialmente `articulo` y `fraccion`.

Su principal limitación es la pérdida de información espacial granular a partir
de 2021.

La exploración tampoco proporciona, por sí sola, evidencia visual suficiente
para elegir abril de 2022 como el evento causal principal.

Por esta razón, la selección del evento y del diseño empírico deberá realizarse
después de comparar esta fuente con:

- incidentes viales C5;
- hechos de tránsito SSC;
- Fotocívicas;
- ubicación de radares;
- otras intervenciones de política vial disponibles.
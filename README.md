# Tesis de Maestría — Economía Aplicada

Repositorio de trabajo para la tesis de la **Maestría en Economía Aplicada**.

## Estado del proyecto

**Estado:** En desarrollo  
**Fase actual:** Rediseño de la investigación y actualización de fuentes de datos.

Este repositorio corresponde a una nueva versión del proyecto de investigación desarrollado originalmente en 2024.

La versión anterior se conserva en las carpetas `legacy/` con el objetivo de mantener reproducibles los análisis y resultados utilizados en el documento preliminar.

> **Importante:** Los archivos dentro de `legacy/` y `data/raw/` deben considerarse inmutables.

---

# Pregunta de investigación

La pregunta de investigación se encuentra actualmente en proceso de redefinición.

De manera general, el proyecto estudia la relación entre **políticas de enforcement vial y seguridad vial en la Ciudad de México**, aprovechando cambios regulatorios, institucionales y tecnológicos en la aplicación del Reglamento de Tránsito.

Actualmente se están evaluando como posibles eventos de interés:

1. Implementación de **Fotocívicas** en 2019.
2. Renovación de la **Policía de Tránsito** en 2022.
3. Cambios regulatorios y operativos dirigidos a **motociclistas** en 2023.
4. Cambios en la utilización de **radares y sistemas tecnológicos de enforcement**.

La estrategia empírica definitiva se determinará después de evaluar la disponibilidad de datos, la variación espacial y temporal de cada política y los supuestos de identificación requeridos.

---

# Estructura del proyecto

```text
TESIS-MAESTRIA/
│
├── data/
│   ├── legacy/
│   ├── raw/
│   ├── processed/
│   └── final/
│
├── syntax/
│   ├── legacy/
│   ├── 00_setup/
│   ├── 01_exploracion/
│   ├── 02_limpieza/
│   ├── 03_construccion/
│   ├── 04_descriptivos/
│   ├── 05_modelos/
│   └── 06_robustez/
│
├── output/
│   ├── figures/
│   ├── tables/
│   ├── maps/
│   └── logs/
│
├── .here
└── README.md
```

---

# Datos

## `data/legacy/`

Contiene los datos utilizados en la versión de la investigación desarrollada originalmente en 2024.

Esta carpeta funciona como respaldo histórico.

**No modificar archivos dentro de esta carpeta.**

Si un archivo de `legacy/` vuelve a utilizarse en la nueva investigación, deberá copiarse a `data/raw/`.

---

## `data/raw/`

Contiene las fuentes originales utilizadas en la nueva versión de la tesis.

Los archivos de esta carpeta:

- no deben modificarse;
- no deben sobrescribirse;
- deben conservar el formato original de descarga;
- deben acompañarse, cuando sea posible, de sus diccionarios y documentación.

Las transformaciones deben escribirse en `data/processed/`.

### Fuentes identificadas hasta el momento

#### Seguridad vial

- Incidentes viales reportados al C5.
- Hechos de tránsito registrados por la SSC.
- Personas lesionadas y fallecidas.
- Vehículos involucrados en hechos de tránsito.
- Puntos de accidentes de peatones.
- Puntos de accidentes de ciclistas.
- Accidentes terrestres por alcaldía.

#### Enforcement

- Infracciones al Reglamento de Tránsito (2020–2023).
- Ubicación histórica de Fotomultas.
- Ubicación de Fotocívicas.
- Ubicación de radares de velocidad.
- Infracciones generadas mediante equipos electrónicos.
- Cumplimiento de Fotocívicas.
- Depósitos vehiculares.

#### Movilidad y controles

- INFOVIAL.
- Vehículos registrados en circulación por tipo de vehículo.
- Otras fuentes de movilidad y características territoriales por incorporar.

> La inclusión de una fuente en `raw/` no implica necesariamente que será utilizada en la especificación final.

---

## `data/processed/`

Contiene bases intermedias generadas mediante los scripts de limpieza y transformación.

Ejemplos:

```text
accidentes_c5_limpio.parquet
hechos_transito_ssc_limpio.parquet
infracciones_limpio.parquet
radares_limpio.gpkg
```

Todos los archivos contenidos aquí deben poder reconstruirse a partir de:

```text
data/raw/ + syntax/
```

Por tanto, ningún archivo de `processed/` deberá contener información que no pueda reproducirse mediante código.

---

## `data/final/`

Contendrá exclusivamente las bases analíticas utilizadas directamente en:

- estadísticas descriptivas;
- modelos econométricos;
- estudios de eventos;
- ejercicios de robustez.

La unidad de observación definitiva todavía está por determinar.

Posibles estructuras incluyen:

```text
código postal × mes
vialidad × mes
zona de exposición × mes
```

---

# Código

Todo el análisis se realiza principalmente en **R**.

## `syntax/legacy/`

Código correspondiente a la versión de la tesis desarrollada en 2024.

**No modificar.**

---

## `syntax/00_setup/`

Configuración general del proyecto.

```text
000_setup.R
```

Incluye:

- rutas;
- paquetes;
- opciones generales;
- creación de directorios;
- funciones auxiliares.

---

## `syntax/01_exploracion/`

Exploración inicial de cada fuente.

Convención:

```text
101_explorar_c5.R
102_explorar_hechos_ssc.R
103_explorar_fotomultas.R
104_explorar_fotocivicas.R
105_explorar_infovial.R
```

Los scripts de esta carpeta **no deben realizar transformaciones definitivas**.

Su objetivo es conocer:

- estructura;
- cobertura temporal;
- cobertura espacial;
- missing values;
- categorías;
- calidad de coordenadas;
- posibles inconsistencias.

---

## `syntax/02_limpieza/`

Limpieza y homologación individual de las fuentes.

Ejemplo:

```text
201_limpiar_c5.R
202_limpiar_hechos_ssc.R
203_limpiar_infracciones.R
```

Los resultados deben guardarse en:

```text
data/processed/
```

---

## `syntax/03_construccion/`

Construcción de variables y bases analíticas.

Aquí se realizarán tareas como:

- cruces espaciales;
- asignación de códigos postales;
- cálculo de distancias a radares;
- construcción de medidas de exposición;
- agregación temporal;
- unión de fuentes;
- construcción del panel.

---

## `syntax/04_descriptivos/`

Estadísticas descriptivas, tendencias y análisis espacial.

Los resultados deben escribirse en:

```text
output/figures/
output/tables/
output/maps/
```

---

## `syntax/05_modelos/`

Modelos econométricos principales.

La estrategia definitiva está pendiente de la fase actual de rediseño.

Posibles metodologías:

- diferencias en diferencias;
- estudio de eventos;
- diseños de tratamiento continuo;
- diseños espaciales.

---

## `syntax/06_robustez/`

Ejercicios adicionales de identificación y sensibilidad.

Potencialmente:

- diferentes ventanas temporales;
- diferentes definiciones de tratamiento;
- tendencias diferenciales;
- placebos;
- exclusión de outliers;
- diferentes niveles de clustering;
- outcomes alternativos;
- heterogeneidad por usuario de la vía.

---

# Convenciones

## Datos

Se sigue el flujo:

```text
RAW
 │
 ▼
PROCESSED
 │
 ▼
FINAL
 │
 ▼
MODELOS / OUTPUT
```

Nunca modificar directamente `raw/`.

---

## Scripts

La numeración indica la etapa del pipeline:

| Serie | Etapa |
|---|---|
| `000` | Setup |
| `100` | Exploración |
| `200` | Limpieza |
| `300` | Construcción |
| `400` | Descriptivos |
| `500` | Modelos |
| `600` | Robustez |

---

## Archivos generados

Evitar nombres como:

```text
final.csv
final2.csv
final_bueno.csv
final_ahora_si.csv
```

Preferir nombres descriptivos:

```text
hechos_transito_ssc_limpio.parquet
panel_cp_mes.parquet
exposicion_radares_cp.parquet
```

Cuando sea necesario versionar manualmente un archivo, utilizar fechas en formato:

```text
YYYYMMDD
```

---

# Reproducibilidad

El objetivo es que los resultados de la tesis puedan reconstruirse a partir de:

```text
data/raw/
        +
syntax/
        ↓
data/processed/
        ↓
data/final/
        ↓
output/
```

La información del entorno de R se registra en:

```text
output/logs/
```

El proyecto utiliza `.here` para identificar la raíz del repositorio y evitar rutas absolutas dependientes del equipo.

---

# Reglas del proyecto

1. No modificar `data/legacy/`.
2. No modificar `syntax/legacy/`.
3. No modificar archivos dentro de `data/raw/`.
4. Toda transformación debe realizarse mediante código.
5. Todo dataset final debe poder reconstruirse desde `raw/`.
6. No sobrescribir resultados históricos necesarios para reproducibilidad.
7. Documentar fuentes y decisiones metodológicas relevantes.
8. Mantener separadas las fuentes administrativas que midan fenómenos similares hasta justificar su homologación.
9. Evitar incorporar variables únicamente por disponibilidad; cada variable debe tener una función dentro del diseño empírico.
10. Priorizar reproducibilidad sobre trabajo manual.

---

# Próximos pasos

- [ ] Completar inventario de fuentes.
- [ ] Documentar cobertura temporal de cada dataset.
- [ ] Documentar cobertura espacial.
- [ ] Explorar ubicación histórica de Fotomultas y Fotocívicas.
- [ ] Evaluar cambios espaciales en dispositivos de enforcement.
- [ ] Explorar INFOVIAL.
- [ ] Explorar Hechos de Tránsito SSC.
- [ ] Determinar candidatos finales de tratamiento.
- [ ] Definir pregunta de investigación.
- [ ] Definir unidad de análisis.
- [ ] Definir estrategia de identificación.
- [ ] Construir nueva base analítica.

---

# Versión anterior

La versión preliminar de la investigación desarrollada en 2024 se conserva como referencia histórica.

El nuevo análisis no deberá sobrescribir los datos, código o resultados necesarios para reproducir dicha versión.
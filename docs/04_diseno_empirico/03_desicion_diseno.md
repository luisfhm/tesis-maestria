# Decisión de diseño empírico

Estado: **diseño acordado** (correo Horacio 2026-09-07) · Actualizado: 2026-09-07

---

## 0. Diseño acordado (2026-09-07)

Tras la corrida del framework y el intercambio con Horacio:

| Elemento | Decisión |
|---|---|
| **Evento** | Abril de 2022 — renovación de la Policía de Tránsito |
| **Ventana** | −10 / +10 meses; referencia mes −1 (marzo 2022) |
| **Unidad** | colonia-mes |
| **Tratamiento** | exposición a infraestructura de fiscalización (inventario 2021), buffers 250/500/1000 m |
| **Outcome principal** | víctimas de tránsito FGJ (lesionados + fallecidos) |
| **Outcome secundario** | incidentes C5 (con cautela: pre-tendencias no planas) |
| **Multas** | **mecanismo**, no primera etapa. Solo para mostrar que el enforcement aumentó diferencialmente. No se hace IV. |
| **Interpretación esperada** | posible *deterrence* sobre víctimas que no pasa por multas; resultado plausible: enforcement ↑ pero víctimas ≈ sin cambio |

Razón de no usar multas como primera etapa (Horacio): el efecto disuasivo sobre
víctimas puede operar sin pasar por multas (la gente cumple sin ser multada), así
que las multas no son "el" canal — son evidencia de más enforcement.

Implementación: `syntax/05_modelos/510_evento_abril2022.R`,
robustez `syntax/06_robustez/603_robustez_abril2022.R`.

### Resultados (2026-09-07, ventana −10/+10, inventario 2021)

| Outcome | Pre-tendencias | ATT agrupado | Robusto a Poisson | Lectura |
|---|---|---|---|---|
| **Fallecidos** | ✅ p 0.17–0.31 | ≈ 0 (p > 0.4) en toda especificación | ✅ | **Nulo limpio** |
| **Víctimas (les.+fall.)** | ✅ p 0.17–0.39 | +12–21% en MCO (marginalmente sig.) | ❌ **desaparece** (Poisson: −0.5%, p 0.93) | El efecto MCO es artefacto de modelo lineal sobre conteo disperso |
| **Multas** (mecanismo) | ✅ p 0.27–0.39 | +10–35%, **nunca significativo** (p 0.16–0.57); bump transitorio ~4 meses en el event study | — | Enforcement diferencial débil / de corta duración |
| **Incidentes C5** | ❌ p ≈ 1e-15 | +10% | — | No se usa |

**Conclusión:** no hay evidencia robusta de que la renovación de la Policía de
Tránsito (abril 2022) redujera las víctimas de tránsito en las zonas con
infraestructura de fiscalización. Los fallecidos no se mueven en ninguna
especificación; el aumento aparente en víctimas totales bajo MCO no sobrevive a
un modelo de conteo (Poisson EF). El "primer paso" de más enforcement es débil y
transitorio, por lo que el nulo debe leerse como *ausencia de evidencia de
efecto en un contexto donde el tratamiento (enforcement diferencial) es en sí
mismo débil*, más que como un cero preciso.

Problemas de datos corregidos: se excluyen abr/may/jun-2021 de la cobertura de
infracciones (fuente incompleta o con colonia = DESCONOCIDO en ~67%).

---

## Contexto (corrida previa, 2026-09-06)

Este documento resume el diseño acordado con Horacio (hilo de correo ago-2026),
lo que se construyó para implementarlo y los resultados de la primera corrida.

---

## 1. Diseño acordado con el asesor

Del intercambio de correos (10, 17, 18, 23 y 24 de agosto):

1. **Unidad = colonia-mes**, definida administrativamente. El tratamiento y los
   outcomes se definen y agregan a ese nivel, nunca en función de si la unidad
   recibió tratamiento.
2. **Tratamiento = exposición espacial a infraestructura de fiscalización
   preexistente**, interactuada con el tiempo relativo a cada evento
   (`i(tiempo_evento, tratada)`), con **efectos fijos de colonia y mes** y
   errores agrupados por colonia.
3. **Varios eventos candidatos**, no solo septiembre de 2023:
   feb-2021 (facultades de infracción), abr-2022 (Policía de Tránsito),
   sep-2023 (reforma motociclistas).
4. **Jugar con el buffer** que define "tratada" y **combinarlo con multas**.
5. **Extender la ventana posterior** más allá de +5.

---

## 2. Restricciones de datos (auditadas)

| Fuente | Cobertura | Georreferencia | Uso |
|---|---|---|---|
| Incidentes C5 | 2014 – **feb 2024** | Coordenadas (≈100%) | Outcome principal actual. Limita el post de sep-2023 a **+5**. |
| Víctimas de tránsito FGJ | 2019 – **jul 2024** | Coordenadas (88%) | Outcome de gravedad (lesionados/fallecidos). Extiende sep-2023 a **+10**. |
| Infracciones | ene-2020 – **abr 2023** | Coord. solo 2020 (97%); nombre después | Solo cubre pre-evento de sep-2023. Asignación a colonia **65%** (97% en 2020, ~55% en 2022-23), **sesgada**: falta justo en las colonias de cámaras. |
| Radares / equipos | Inventario anual **2019 – 2024** | Coordenadas | Base para el tratamiento por buffer. |
| Fotocívicas | Un corte (dic-2022) | Coordenadas | Solo "tenía cámara en 2022". |

**Implicaciones:**
- Multas **no** sirven como outcome post para abr-2022 ni sep-2023. Sí como
  medida de intensidad de enforcement 2020-2021 y como mecanismo.
- La ventana post de sep-2023 solo se extiende con **víctimas FGJ**.

---

## 3. Lo que se construyó (scripts nuevos)

```
02_limpieza/202_limpiar_infracciones.R      12.1M infracciones, 2 esquemas homologados
02_limpieza/203_limpiar_victimas.R          26.8k víctimas de tránsito, 2019-jul2024
03_construccion/308_asignar_infracciones_colonia.R
03_construccion/309_construir_panel_infracciones.R
03_construccion/310_construir_exposicion_radares.R   buffers 250/500/1000/1500 m x año
03_construccion/311_asignar_victimas_colonia.R       88% asignadas
03_construccion/312_construir_panel_analisis.R        panel colonia-mes con 3 outcomes
05_modelos/507_event_study_framework.R                event study multi-evento x buffer x outcome
```

Tratamiento por buffer (número de colonias tratadas, de 1,814):

| Buffer | 250 m | 500 m | 1000 m | 1500 m |
|---|---|---|---|---|
| Colonias tratadas | ~200-236 | ~310-370 | ~550-640 | ~780-900 |

---

## 4. Resultados de la primera corrida (507)

Prueba conjunta de pre-tendencias (p-value), ventana −12:

| Evento | Outcome | buffer 250 | buffer 500 | buffer 1000 |
|---|---|---|---|---|
| feb-2021 | incidentes C5 | 2e-15 | 5e-21 | 7e-26 |
| feb-2021 | víctimas | 0.08 | 0.04 | 0.05 |
| abr-2022 | incidentes C5 | 3e-11 | 1e-16 | 2e-19 |
| abr-2022 | víctimas | **0.20** | **0.11** | **0.15** |
| sep-2023 | incidentes C5 | 1e-9 | 5e-13 | 3e-14 |
| sep-2023 | víctimas | **0.46** | **0.29** | **0.62** |

**Hallazgo central:** con el outcome de **incidentes C5**, las colonias cercanas a
radares están en una trayectoria sistemáticamente distinta (creciente) que las
lejanas, en **los tres eventos y todos los buffers**. No es un problema de un
evento concreto: es que la comparación "cerca de infraestructura" vs. "lejos" no
cumple tendencias paralelas para C5, porque la infraestructura está en vías
primarias con dinámica de tráfico propia.

Con **víctimas FGJ** las pre-tendencias sí pasan (sobre todo abr-2022 y sep-2023),
pero el outcome es muy disperso a nivel colonia-mes (media 0.16, 89% ceros) y el
event study queda **sin potencia** para detectar un efecto.

El diseño original de radares (scripts 300-303, 8 ubicaciones nuevas + matching)
**sí** tenía pre-tendencias planas (303: p=0.39), pero con solo 8 unidades
tratadas.

---

## 5. Tensión de fondo

| | Pre-tendencias | Potencia |
|---|---|---|
| Pocas unidades (8 radares nuevos) + matching | ✅ planas | ❌ 8 tratadas |
| Muchas unidades (200-600 cerca de radares) + FE | ❌ no paralelas (C5) | ✅ |

---

## 6. Caminos para la siguiente iteración

**A. Tratamiento = radar nuevo + controles comparables.**
Definir tratadas las colonias que reciben un **radar nuevo** (expansión 2023:
+23 ubicaciones fijas) y construir el control por matching sobre características
pretratamiento (nivel de incidentes, tendencia, tipo de vía), escalando el
enfoque de 302/303 a colonia con buffers. Evento = la propia instalación.

**B. Cambiar el outcome principal a víctimas + agregación.**
Las víctimas pasan pre-tendencias. Ganar potencia agregando a una unidad
espacial más gruesa (colonia base sin subdivisiones I/II/III, o corredores) o
modelo de conteo (Poisson) en vez de MCO.

**C. Doble diferencia sobre la intensidad de enforcement.**
Usar las multas 2020-2021 como primera etapa: ¿las colonias tratadas tuvieron
más multas tras feb-2021? Y víctimas/C5 como forma reducida.

**D. Segmentos viales** como unidad, como Luis ya mencionaba explorar.

---

## 7. Decisiones pendientes

- [ ] D001 — Evento(s) principal(es): ¿la expansión de radares 2023, o un evento regulatorio?
- [ ] D002 — Identificación: matching de controles vs. FE simples
- [ ] D003 — Outcome principal: C5 (potencia, falla pre-trends) vs. víctimas (pre-trends OK, sin potencia)
- [ ] D004 — Unidad: colonia IECM vs. colonia base vs. segmento vial

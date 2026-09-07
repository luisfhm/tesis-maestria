# Borrador de correo para Horacio — 2026-09-06

**Asunto:** Re: Retomar proyecto de tesis y solicitud de retroalimentación

**Adjuntos sugeridos:**
`output/figures/ecuacion_event_study.png` (insertar como imagen donde dice ESPECIFICACIÓN),
`output/figures/509_pretendencias_c5_vs_victimas.png` (soporta el primer y segundo punto),
`output/figures/508_primera_etapa.png` (soporta el tercer punto)

---

Hola profe,

Ya construí el outcome de víctimas y las definiciones de tratamiento por buffer,
y corrí el estudio de evento mes a mes para los tres cambios de política.

**Especificación.** Para cada evento estimo:

[IMAGEN: ecuacion_event_study.png]

con `tratada_i` = exposición a la infraestructura de fiscalización (radares y
equipos) del año previo al evento, y errores agrupados por colonia. La primera
etapa es la misma ecuación con multas del lado izquierdo.

**Los eventos**

- **Febrero de 2021** — cambio regulatorio en las facultades de infracción.
- **Abril de 2022** — renovación de la Policía de Tránsito (cuerpo especializado,
  digitalización, más agentes facultados para infraccionar).
- **Septiembre de 2023** — entrada en vigor de la reforma para motociclistas.

**Lo que se observa a primera vista** (figura `509`, buffer 500 m)

- Con **incidentes C5** como outcome (fila de arriba), las colonias cercanas a la
  infraestructura vienen en una trayectoria distinta (creciente) que las lejanas,
  en los tres eventos y en todos los buffers. La prueba conjunta de
  pre-tendencias se rechaza siempre (p ≈ 1e-16). No parece un efecto de política,
  sino que la infraestructura está en vías primarias con dinámica de tráfico
  propia.
- Con **víctimas de tránsito** (fila de abajo; fuente FGJ, que además llega hasta
  julio de 2024 y me permite +10 meses en septiembre de 2023), las pre-tendencias
  sí pasan en abril de 2022 y septiembre de 2023 (p = 0.11 y 0.29), pero el
  outcome es muy disperso a nivel colonia-mes (89% de ceros) y el estudio de
  evento no tiene potencia para detectar nada.
- Como primera etapa probé si las **multas** suben cerca de la infraestructura
  tras cada evento (figura `508`). En febrero de 2021 no se puede leer
  (recuperación post-COVID). En **abril de 2022** las pre-tendencias pasan y las
  multas cerca de la infraestructura suben del orden de +30% después del evento,
  consistente entre buffers.

**Mi pregunta**

Ninguna de las tres corridas da todavía un diseño limpio. Antes de invertir más,
¿qué recomienda como siguiente paso?

- ¿Anclar en abril de 2022 y trabajar la doble diferencia sobre enforcement
  (multas como primera etapa, víctimas y C5 como forma reducida)?
- ¿Construir primero un grupo de control comparable por matching, escalando lo
  de los 8 radares a nivel colonia?
- ¿Pasar a segmentos viales como unidad?

Saludos.

Luis

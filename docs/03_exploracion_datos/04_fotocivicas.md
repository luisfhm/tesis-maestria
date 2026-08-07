## Fotocívicas

### Cobertura

Se identificaron dos componentes principales:

1. **Cumplimiento de sanciones**
   - Cursos en línea: julio de 2019 – julio de 2022.
   - Actividades presenciales: julio de 2019 – septiembre de 2022.
   - Las bases contienen identificadores de usuario y placa, fecha,
     tipo de actividad y estatus de cumplimiento.

2. **Ubicaciones de Fotocívicas**
   - 140 ubicaciones.
   - 113 representadas mediante geometrías POINT.
   - 27 representadas mediante geometrías LINESTRING.
   - CRS WGS 84.
   - Ambas capas contienen información de vialidad, ubicación y sentido.

### Limitaciones

- Las capas de ubicación no contienen una fecha explícita de instalación
  o inicio de operación.
- Por lo tanto, no puede inferirse todavía cuándo comenzó a operar cada
  ubicación.
- Las fechas internas de los archivos espaciales no deben interpretarse
  como fechas de instalación.
- La información de cumplimiento presenta una fuerte disrupción durante
  la pandemia y termina en 2022.

### Papel potencial en la tesis

La información de cumplimiento puede utilizarse para describir la
operación del sistema Fotocívicas.

Las ubicaciones pueden ser relevantes para construir medidas de
exposición espacial al enforcement, pero primero deben compararse con
la base independiente de radares/Fotocívicas.
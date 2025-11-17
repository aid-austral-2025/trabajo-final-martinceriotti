# Tarea 2

## Descripción

El objetivo de este trabajo es generar un producto en el cual se evidencie la presencia de las cuatro etapas del **Análisis Inteligente de Datos**, discutidas durante el curso: **selección**, **limpieza**, **exploración** y **comunicación** de datos.

La problemática a abordar y el conjunto de datos a emplear es de **elección libre** por parte de cada estudiante. Mencionamos algunas ideas:

-   Utilizar datos relacionados a su actividad laboral o académica. Para no exponer datos privados, pueden generar bases con registros falsos o inventados, pero que posean una estructura y temática similar a la analizada regularmente por ustedes.

-   Si les interesa el análisis de datos textuales pueden buscar y descargar datos de este tipo y realizar con ellos un análisis exploratorio.

-   Si les interesa la georreferencia, pueden descargar datos relacionados a indicadores socio-económicos, de salud, ambientales, etc. desde la web [**Gapminder**](https://www.gapminder.org/data/) y generar visualizaciones al respecto con alguna de las herramientas desarrolladas en la clase de Mapas.

-   Si les interesa la música, pueden descargar bases de canciones y/o álbumes de su artista favorito desde la API de [**Spotify**](https://open.spotify.com/), tal como se hizo para la Tarea 1 (no se pueden usar nuevamente los datos de Charly García).

-   Se puede implementar algún procedimiento de *web scraping* para recolectar datos de algún portal de interés, con su posterior limpieza y exploración.

-   Se puede generar la automatización del algún proceso no trivial que deba ejecutarse periódicamente (limpieza de datos, producción de reportes, etc.) y que ponga en evidencia la práctica de un flujo de trabajo reproducible.

El producto debe tomar una de las siguientes formas, a elección de cada estudiante:

1.  **Un reporte o presentación producida con Quarto en formato de salida HTML**

2.  **Una aplicación web Shiny**

Cualquiera sea la opción elegida, el producto resultante debe evidenciar la presencia de los cuatro componentes del análisis, teniendo en cuenta lo siguiente:

a.  **Selección**: describir brevemente el problema en estudio, la fuente de los datos y qué tipo de información se dispone.

b.  **Limpieza**: comentar de manera general y en forma breve los pasos tomados para limpiar y ordenar los datos. Si no hubiese pasos necesarios para limpiar los datos, comentar cómo se llegó a esa conclusión.

c.  **Exploración**: incluir medidas descriptivas numéricas (si corresponde) y visualizaciones que resulten de interés para el problema planteado. Quienes opten por realizar una ShinyApp, deberán incorporar al menos un componente dinámico que le permita al usuario de la aplicación ser partícipe de la exploración de los datos (elegir variables, categorías, tipo de gráfico u otras opciones que resulten relevantes).

d.  **Comunicación**: este aspecto se verá reflejado implícitamente en la utilización de las herramientas Quarto o Shiny para generar el producto final. Ambas aportan a la comunicación de los resultados al proveer un marco de automatización, reproducibilidad y cuidado estético.

En el caso de entregar un reporte o presentación, no es necesario que los puntos anteriores conformen las distintas secciones del texto, sólo deben estar reflejados en el mismo. Se sugiere estructurar la redacción mediante secciones tales como *Introducción*, *Materiales y métodos*, *Resultados* y *Conclusiones*. Si se realiza una ShinyApp, los comentarios referidos a la limpieza y selección pueden estar detallados en alguna pestaña o sector como información general. Si el proyecto consiste en otro tipo de trabajo (como la automatización de algún proceso de forma reproducible) igualmente se debe preparar un reporte producido con Quarto con salida en HTML donde se detalle y ejemplifique (si corresponde) la tarea realizada (la fase de *exploración* podría no corresponder en este tipo de entrega).

Ante la duda sobre la pertinencia del proyecto elegido como trabajo final de la asignatura, se puede consultar a los docentes describiendo la idea concebida.

## Forma de entrega

La entrega se realiza mediante este repositorio, en el cual se deben subir todos los archivos relacionados al trabajo realizado (datos, imágenes, scripts, etc.). Por default, **este repositorio es público**. En caso de utilizar para el trabajo datos u otros materiales que no puedan ser compartidos abiertamente, solicitar a los docentes que lo transformen en privado. A continuación se detallan algunas particularidades según el tipo de proyecto, que asumen que el repositorio será público.

### Reporte o presentación producida con Quarto en formato de salida HTML

Utilizaremos el servicio **GitHub Pages** para alojar la página *html* con el reporte o presentación. Para esto:

-   Llamar al archivo Quarto fuente `index.qmd`, para que la salida sea nombrada automáticamente `index.html`.
-   Colocar ambos archivos en el directorio principal del repo (no en subcarpetas). Pueden organizar el resto de los materiales como deseen.
-   En el encabezado *YAML* incluir:

```         
format: 
  html:
    embed-resources: true
```

-   Pocos minutos después de haber subido el archivo `index.html`, su contenido se verá en `https://aid-austral-2025.github.io/trabajo-final-<usuario>/`. Mientras no haya un `index.html`, se verá el contenido del README.

### ShinyApp

No es necesario que la aplicación desarrollada esté alojada en un servicio de hosting online, ya que los docentes accederemos a ella utilizando la función `runGitHub()` de `shiny`. Para esto:

-   Llamar al archivo con el código `app.R`.
-   Colocarlo en el directorio principal del repo (no en subcarpetas). Pueden organizar el resto de los materiales (bases de datos, imágenes, archivos auxiliares, etc.) como deseen.

## Aspectos a considerar

Los siguientes aspectos deben ser tenidos en cuenta y formarán parte de la evaluación:

-   El código debe ser escrito de forma prolija. Se pueden observar ejemplos de la guía de estilo vigente en RStudio y Google para R [en este enlace](https://style.tidyverse.org/index.html).
-   El código debe estar bien documentado, explicando qué acción se realiza en cada paso, cuando esta no sea obvia.
-   El código debe poder ejecutarse sin errores. Esto implica, entre otras cosas, que deben informarse los paquetes utilizados.
-   El código debe ejecutarse teniendo como *working directory* la raíz del repositorio. **No deben incluirse** rutas o directorios de carpetas personales en el script final; toda ruta debe referirse a dicho *working directory*.
-   Los archivos entregados deben estar organizados de manera clara en el repo.
-   La redacción debe ser clara, concisa, **sin errores ortográficos** y organizada en secciones de forma coherente.
-   Se deben subir al repo los conjuntos de datos utilizados o proceder a su lectura desde la nube mediante los correspondientes enlaces en el código.
-   Se debe asentar la fuente de los datos utilizados.
-   Pueden dejar observaciones o comentarios que consideren necesarios en el archivo `README` del repo.

## Reglas

-   El trabajo es **individual**.
-   La fecha límite de entrega es el domingo 30/11 a la medianoche. Cumplida esa hora, los docentes clonarán el repo de cada grupo y con lo obtenido se procederá a la evaluación. No se tendrán en cuenta *commits* realizados con posterioridad.
-   Se promueve la discusión entre estudiantes y docentes, pero queda prohibido el intercambio de código u otro tipo de ayuda entre estudiantes.
-   Para realizar consultas con los docentes, abrir un *issue* en el repo del grupo, escribir la consulta y **etiquetar a ambos docentes*.

library(tidyverse)
library(janitor)
library(RColorBrewer)
library(gganimate)
# ===========================================
#            lectura de datos.
# ===========================================

# Utilizamos latin1 para poder tener Ñ y acentos sin problemas. Solo en Soja necesitamos utf-8.

maiz_serie_1923_2023 <- read_csv("data/maiz-serie-1923-2023.csv",
  col_types = cols(anio = col_integer()),
  locale = locale(encoding = "latin1")
)

soja_serie_1941_2023 <- read_csv("data/soja-serie-1941-2023.csv",
  col_types = cols(anio = col_integer()),
  locale = locale(encoding = "utf-8")
)

trigo_serie_1927_2024 <- read_csv("data/trigo-serie-1927-2024.csv",
  col_types = cols(anio = col_integer()),
  locale = locale(encoding = "latin1")
)

# # Revisamos cuando arranca cada serie de tiempo.
# min(maiz_serie_1923_2023$anio)
# max(maiz_serie_1923_2023$anio)
# min(soja_serie_1941_2023$anio)
# max(soja_serie_1941_2023$anio)
# min(trigo_serie_1927_2024$anio)
# max(trigo_serie_1927_2024$anio)

library(dplyr)


# trigo_serie_1927_2024 |>
#   arrange( provincia_nombre, departamento_nombre, anio )|>     # Ordena los datos cronológicamente
#   group_by( provincia_nombre, departamento_nombre ) |>          # Agrupa los datos por departamento
#   mutate(
#     crecio = superficie_sembrada_ha > lag(superficie_sembrada_ha)  #Compara cada año con el anterior
#   ) |>
#   summarise(
#     años_crecio = sum(crecio, na.rm = TRUE),   # Cuenta cuántas veces creció (TRUE se suma como 1)
#     total_anios = n() - 1                      # Número de comparaciones posibles (n años = n-1 saltos)
#   ) |>
#   mutate(
#     pct_anios_crecio = años_crecio / total_anios  # Porcentaje de años en los que hubo crecimiento
#   ) |>
#   arrange(desc(pct_anios_crecio))   # Ordena de mayor a menor crecimiento sostenido

# ===================== FUNCION DE CRECIMIENTO DE DEPARTAMENTO Formato ROXYGEN2=======================

#' Analiza el crecimiento sostenido de una variable en series de tiempo por departamento
#'
#' Esta función calcula cuántos años una variable aumentó respecto al año anterior
#' para cada departamento dentro de cada provincia, y el porcentaje de años con crecimiento.
#'
#' En la primera version de la funcion, encontre que los departamentos con menos datos
#' tenian ventaja. Entonces completamos la serie para todos.

analizar_crecimiento <- function(data, variable = superficie_sembrada_ha) {
  # Rango total de años presentes en el dataset
  años_totales <- range(data$anio, na.rm = TRUE)

  # Completar todas las combinaciones posibles de provincia, departamento y año
  data_completa <- data |>
    select(provincia_nombre, departamento_nombre, anio, {{ variable }}) |>
    complete(
      provincia_nombre, departamento_nombre,
      anio = full_seq(años_totales, 1)
    )

  data_completa |>
    arrange(provincia_nombre, departamento_nombre, anio) |>
    group_by(provincia_nombre, departamento_nombre) |>
    mutate(crecio = {{ variable }} > lag({{ variable }})) |>
    summarise(
      años_crecio = sum(crecio, na.rm = TRUE),
      total_anios = n() - 1
    ) |>
    mutate(
      pct_anios_crecio = años_crecio / total_anios
    ) |>
    arrange(desc(pct_anios_crecio))
}

trigo_serie_1927_2024 <- analizar_crecimiento(trigo_serie_1927_2024)
soja_serie_1941_2023 <- analizar_crecimiento(soja_serie_1941_2023)
maiz_serie_1923_2023 <- analizar_crecimiento(maiz_serie_1923_2023)


# Filtramos 1947 / 2023 para tener todos los cultivos todos los años y poder hacer otros análisis.

datos <- bind_rows(maiz_serie_1923_2023, soja_serie_1941_2023, trigo_serie_1927_2024) |>
  filter(anio >= 1947 & anio <= 2023)

# Parámetros Generales para todos los gráficos.
# theme_set(theme_light())
options(repr.plot.width = 20, repr.plot.height = 8)

# Evolución de las hectareas sembradas de cada cultivo.
library(ggrepel)
datos |>
  group_by(anio, cultivo_nombre) |>
  summarise(superficie_sembrada_ha_total = sum(superficie_sembrada_ha)) |>
  ggplot() +
  aes(x = anio, y = superficie_sembrada_ha_total / 1000000, colour = factor(cultivo_nombre)) +
  geom_line(size = 0.9) +
  geom_vline(
    aes(xintercept = 1970, linetype = "Comienzo Siembra directa\nFuente: AAPRESID"),
    lwd = 0.9, lineend = "round", col = "orange"
  ) + # Comienza la siembra directa
  geom_vline(
    aes(xintercept = 2008, linetype = "Conflicto con\nel campo"),
    lwd = 0.9, lineend = "round",
  ) + # Comienza la siembra directa
  labs(x = "Año", y = "Superficie Sembrada en millones de ha.") +
  scale_colour_brewer(palette = "Dark2") +
  geom_text_repel(
    data = \(d) d |>
      group_by(cultivo_nombre) |>
      filter(anio == max(anio)),
    aes(label = cultivo_nombre),
    nudge_x = 1.5, # desplaza un poco a la derecha del último punto
    direction = "y",
    hjust = 0,
    segment.color = NA, # sin líneas de unión
    size = 3
  ) + # nombre al final de las lineas
  theme(legend.position = "none")

# Evolución de las hectareas sembradas de cada cultivo.

datos |>
  filter(
    provincia_nombre %in% c("Santiago del Estero", "San Luis", "Tucumán","Salta"),
  #  provincia_nombre %in% c("Santa Fe"),
    cultivo_nombre == "soja"
  ) |>
  group_by(anio, provincia_nombre) |>
  summarise(superficie_sembrada_ha_total = sum(superficie_sembrada_ha)) |>
  ggplot() +
  aes(x = anio, y = superficie_sembrada_ha_total / 1000000, colour = factor(provincia_nombre)) +
  geom_line(size = 0.9) +
  geom_vline(
    aes(xintercept = 1970, linetype = "Comienzo Siembra directa\nFuente: AAPRESID"),
    lwd = 0.9, lineend = "round", col = "orange"
  ) + # Comienza la siembra directa
  geom_vline(
    aes(xintercept = 2008, linetype = "Conflicto con\nel campo"),
    lwd = 0.9, lineend = "round",
  ) + # Comienza la siembra directa
  labs(x = "Año", y = "Superficie Sembrada en millones de ha.") +
  scale_colour_brewer(palette = "Dark2") +
  geom_text_repel(
    data = \(d) d |>
      group_by(provincia_nombre) |>
      filter(anio == max(anio)),
    aes(label = provincia_nombre),
    nudge_x = 1.5, # desplaza un poco a la derecha del último punto
    direction = "y",
    hjust = 0,
    segment.color = NA, # sin líneas de unión
    size = 3
  ) + # nombre al final de las lineas
  theme(legend.position = "none")


 datos |>
  ungroup()

# Producción por Provincia.

datos |>
  group_by(provincia_nombre, cultivo_nombre) |>
  summarise(superficie_sembrada_ha = sum(superficie_sembrada_ha)) |>
  ggplot() +
  aes(
    x = superficie_sembrada_ha / 1000000,
    y = reorder(provincia_nombre, superficie_sembrada_ha),
    fill = cultivo_nombre
  ) +
  geom_bar(stat = "identity") +
  facet_wrap(~cultivo_nombre) +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none")


datos |>
  ungroup()

# Relación Siembra
datos |>
  ggplot() +
  aes(
    x = superficie_sembrada_ha / 1000000,
    y = produccion_tm / 1000,
    colour = cultivo_nombre
  ) +
  geom_point() +
  scale_colour_brewer(palette = "Dark2") +
  labs(y = "Producción en Miles de Toneladas", x = "Superficie Sembrada en millones de ha.", color = "Cultivos")

# Heatmap no se ve nada.
datos |>
  filter(
    cultivo_nombre == "soja",
    anio == 2023,
    provincia_nombre %in% c("Santa Fe", "Córdoba", "Buenos Aires", "Entre Ríos")
  ) |>
  ggplot() +
  aes(x = provincia_nombre, y = departamento_nombre, fill = mean(produccion_tm)) +
  geom_tile()

# Alluvial
library("ggalluvial")
datos |>
  group_by(provincia_nombre, cultivo_nombre) |>
  summarize(superficie_sembrada_ha = sum(superficie_sembrada_ha) / 1000000) |>
  ggplot() +
  aes(axis1 = provincia_nombre, axis2 = cultivo_nombre, fill = superficie_sembrada_ha) +
  geom_alluvium() + # flujos
  # geom_stratum(fill = "black", color = "lightgrey", width = 0.35) + #columnas
  # geom_label( #etiquetas
  #   stat = "stratum",
  #   aes(label = after_stat(stratum)),
  #   fill = "white",
  #   size = 3
  # ) +
  # scale_x_discrete(limits = c("Efector", "Sexo")) +
  # scale_y_continuous(name = "Frecuencias", limits = c(0, 21000), expand = c(0, 0)) +
  theme(legend.position = "none")
datos |>
  ggplot(
    aes(x = reorder(provincia_nombre, superficie_sembrada_ha), y = superficie_sembrada_ha)
  ) +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Comparación de medida decimal por categoría",
    x = "Categoría", y = "Medida"
  )

# tuki |>
#   ggplot() +
#   aes(axis1 = provincia_nombre, axis2 = cultivo_nombre, fill = superficie_sembrada_ha ) +
#   geom_alluvium() + #flujos
#   # geom_stratum(fill = "black", color = "lightgrey", width = 0.35) + #columnas
#   geom_label( #etiquetas
#      stat = "stratum",
#      aes(label = after_stat(stratum)),
#      fill = "white",
#      size = 3
#    ) +
#   # scale_x_discrete(limits = c("Efector", "Sexo")) +
#   # scale_y_continuous(name = "Frecuencias", limits = c(0, 21000), expand = c(0, 0)) +
#   theme(legend.position = "none")
#
#
#
#
# ================== TABLAS RESUMEN ==============
datos_por_anio_provincia <-
  datos |>
  group_by(cultivo_nombre, provincia_nombre) |>
  filter(anio == 2023) |>
  summarise(
    superficie_sembrada_ha = sum(superficie_sembrada_ha),
    .groups = "drop"
  ) |>
  pivot_wider(names_from = provincia_nombre, values_from = superficie_sembrada_ha, values_fill = 0) |>
  adorn_percentages("row") |>
  adorn_totals("row") |>
  adorn_pct_formatting(digits = 1)

datos_por_anio_provincia


# ========================= MAPAS ARG =====================================

library(plotly)
library(sf)
library(dplyr)
library(rnaturalearth)
# install.packages("devtools")
# install.packages("rnaturalearth")
# install.packages("geojsonio")
library(geojsonio)

arg_provincias <- rnaturalearth::ne_states(country = "Argentina", returnclass = "sf")
usethis::create_github_token()
gitcreds::gitcreds_set()


# ======================= ANIMACIONES ====================================

# =====================================================================================
# USAR THE R GALLERY / WTF VISUALIZATION para reirse de lo que no hay que hacer.
# CLASE VISUALIZACIONES
# =====================================================================================

paquetes <-
  c(
    "tidyverse", "cowplot", "ggridges", "ggbeeswarm", "GGally", "plotly",
    "treemapify", "car", "vcd", "colorspace", "ggcleveland", "corrplot", "readxl",
    "lubridate", "gganimate", "gapminder", "forcats", "janitor", "ggforce",
    "ggalluvial"
  )

# Revisar ideas en kaggle
g <-
  ggplot(data = gapminder) +
  aes(
    x = log(gdpPercap),
    y = lifeExp,
    size = pop,
    colour = continent
  ) +
  geom_point(alpha = 0.7, show.legend = FALSE) +
  scale_size(guide = "none") +
  scale_x_continuous() +
  facet_wrap(~year) +
  transition_time(year) +
  labs(
    title = "Año: {frame_time}",
    x = "esto es x"
  )

datos |>
  ggplot() +
  aes(x = cultivo_nombre, y = rendimiento_kgxha, fill = cultivo_nombre) +
  geom_boxplot() +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  theme(legend.position = "none") +
  labs(
    title = "Año: {frame_time}",
    x = "esto es x",
    y = "esto es y"
  )

library(tidyverse)
library(janitor)
#===========================================
#            lectura de datos.
#===========================================

# Utilizamos latin1 para poder tener Ñ y acentos sin problemas. Solo en Soja necesitamos utf-8.

maiz_serie_1923_2023 <- read_csv("data/maiz-serie-1923-2023.csv", 
                                 col_types = cols(anio = col_integer()), 
                                 locale = locale(encoding = "latin1"))

soja_serie_1941_2023 <- read_csv("data/soja-serie-1941-2023.csv", 
                                 col_types = cols(anio = col_integer()), 
                                 locale = locale(encoding = "utf-8"))

trigo_serie_1927_2024 <- read_csv("data/trigo-serie-1927-2024.csv", 
                                  col_types = cols(anio = col_integer()), 
                                  locale = locale(encoding = "latin1"))

# Revisamos cuando arranca cada serie de tiempo.
min(maiz_serie_1923_2023$anio)
max(maiz_serie_1923_2023$anio)
min(soja_serie_1941_2023$anio)
max(soja_serie_1941_2023$anio)
min(trigo_serie_1927_2024$anio)
max(trigo_serie_1927_2024$anio)


# Filtramos 1947 / 2023 para tener todos los cultivos todos los años y poder hacer otros análisis.
datos <- bind_rows(maiz_serie_1923_2023,soja_serie_1941_2023, trigo_serie_1927_2024) |> 
  filter(anio >= 1947 & anio <= 2023)

# Parámetros Generales para todos los gráficos.
theme_set(theme_light())
options(repr.plot.width=20, repr.plot.height=8)

# Evolución de las hectareas sembradas de cada cultivo.

datos |> 
  group_by(anio, cultivo_nombre) |> 
  summarise(superficie_sembrada_ha_total = sum(superficie_sembrada_ha)) |> 
    ggplot() +
      aes(x = anio, y = superficie_sembrada_ha_total / 1000000, colour = factor(cultivo_nombre)) +
      geom_line(size = 0.9) +
      geom_vline(
        aes(xintercept = 1970, linetype = "Comienzo Siembra directa\nFuente: AAPRESID"),
        lwd = 0.9, lineend = "round", col = "orange"
      ) + #Comienza la siembra directa
      geom_vline(
        aes(xintercept = 2008, linetype = "Conflicto con\nel campo"),
        lwd = 0.9, lineend = "round", 
      ) + #Comienza la siembra directa
      labs(x = 'Año', y = 'Superficie Sembrada en millones de ha.', color = "Cultivos")  

datos |> 
  ungroup()

# Producción por Provincia.

datos |> 
  group_by(provincia_nombre, cultivo_nombre) |> 
  summarise(superficie_sembrada_ha = sum(superficie_sembrada_ha)) |> 
  ggplot() +
    aes(x = superficie_sembrada_ha / 1000000, y = reorder(provincia_nombre, superficie_sembrada_ha), fill = cultivo_nombre) +
    geom_bar(stat='identity') + 
    facet_wrap(~ cultivo_nombre) 
  
datos |> 
  ungroup()

# 
# datos |> 
#   ggplot() +
#   aes(x = superficie_sembrada_ha) + 
#   geom_histogram()

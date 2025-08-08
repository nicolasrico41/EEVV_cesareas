## Generación de líneas de tiempo ##

## Cargar paquetes ----

rm(list=ls())

packageList<-c("tidyverse", "stringr", "RCurl", "glue","rgeos","readr","ggrepel",
               "haven","raster","rgdal","survey", "readxl", "gridExtra","xlsx",
               "XLConnect", "scales","colorspace", "broom","janitor","rlang",
               "samplingbook", "sf", "openxlsx", "RSQLite", "DBI", "networkD3",
               "ggalluvial", "ggplot2", "skimr", "lubridate", "svglite", "httr",
               "jsonlite", "purrr","robotstxt", "XML", "ggdensity", "gganimate",
               "ggpubr", "forecast", "tsibble", "timetk", "broom", "zoo")

lapply(packageList,require,character.only=TRUE)

## Generar base de EEV ----

datos <- "C:/Users/Usuario/OneDrive - ADRES/Bases de datos/Cesáreas"

# Abrir los años de nacimientos

nac_17 <- readr::read_csv(
  glue("{datos}/Datos/csv/nac2017.csv")
)

nac_18 <- readr::read_csv(
  glue("{datos}/Datos/csv/nac2018.csv")
)

nac_19 <- readr::read_csv(
  glue("{datos}/Datos/csv/nac2019.csv")
)

nac_20 <- readr::read_csv(
  glue("{datos}/Datos/csv/nac2020.csv")
)

nac_21 <- readr::read_csv(
  glue("{datos}/Datos/csv/nac2021.csv")
)

nac_22 <- readr::read_csv(
  glue("{datos}/Datos/csv/nac2022.csv")
)

nac_23 <- readr::read_csv(
  glue("{datos}/Datos/csv/nac2023.csv")
)

# Cambio de variable en 2020

nac_20 <- nac_20 %>% 
  dplyr::mutate(
    OTRO_SIT = as.character(OTRO_SIT)
  )

# Hacer apend de los años

nacimientos <- nac_17 %>% 
  bind_rows(
    nac_18,
    nac_19,
    nac_20,
    nac_21,
    nac_22,
    nac_23
  )

rm(list = ls(pattern = "^nac_"))

## Calcular porcentajes EEVV----

nacimientos_tot <- nacimientos %>% 
  dplyr::group_by(
    ANO
  ) %>% 
  summarise(
    tot_nac = n()
  )

nacimientos_cesare <- nacimientos %>% 
  dplyr::filter(
    TIPO_PARTO == 2
  ) %>% 
  dplyr::group_by(
    ANO
  ) %>% 
  summarise(
    tot_ces = n()
  )

nac_per <- nacimientos_tot %>% 
  left_join(
    nacimientos_cesare,
    by = "ANO"
  ) %>% 
  dplyr::mutate(
    per = (tot_ces / tot_nac) * 100, 
    FUENTE = 'EEVV'
  ) %>% 
  dplyr::filter(
    ANO != 2017
  )

## Cargar datos suficiencia ----

sufi <- readr::read_csv(glue("{datos}/Datos/csv/analisis_cesareas.csv"))

sufi <- sufi %>% 
  dplyr::group_by(
    anio_servicio
  ) %>% 
  dplyr::mutate(
    tot_partos = sum(NUM_PARTOS, na.rm = T)
  ) %>% 
  ungroup()

sufi_ces <- sufi %>% 
  dplyr::filter(
    PARTO == "CESAREA"
  ) %>% 
  dplyr::mutate(
    per = (NUM_PARTOS / tot_partos) * 100
  ) %>% 
  rename(
    ANO = anio_servicio,
    tot_nac = tot_partos,
    tot_ces = NUM_PARTOS
  ) %>% 
  dplyr::select(
    ANO, tot_nac, tot_ces, per, FUENTE
  )

## Cargar datos de RIPS

rips <- readxl::read_xlsx(
  glue("{datos}/Datos/xlsx/rips_casos_partos.xlsx")
)

rips <- rips %>% 
  dplyr::group_by(
    ANIO_SERVICIO
  ) %>% 
  dplyr::mutate(
    tot_partos = sum(NUM_PARTOS, na.rm = T)
  ) %>% 
  ungroup()

rips_ces <- rips %>% 
  dplyr::filter(
    PARTO == "CESAREA"
  ) %>% 
  dplyr::mutate(
    per = (NUM_PARTOS / tot_partos) * 100,
    FUENTE = "RIPS"
  ) %>% 
  rename(
    ANO = ANIO_SERVICIO,
    tot_nac = tot_partos,
    tot_ces = NUM_PARTOS
  ) %>% 
  dplyr::select(
    ANO, tot_nac, tot_ces, per, FUENTE
  )

## Unir bases ----

base <- sufi_ces %>% 
  bind_rows(nac_per,
            rips_ces)

grafica_per_ces <- base %>% 
  ggplot(
    aes(
      x = ANO,
      y = per,
      color = FUENTE
    )
  ) +
  geom_line(
    linewidth = 1.2
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      decimal.mark = ",",
      big.mark = ".",
      suffix = " %")
  ) +
  scale_color_manual(
    values = c(
      "Suficiencia" = "#1874CD",
      "EEVV" = "#7FFFD4",
      "RIPS" = "#2F4F4F"
    )
  ) +
  labs(
    x = "",
    y = "",
    color = "",
    caption = "Nota: De EEVV los datos son nacimientos, de suficiencia y RIPS son partos."
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 18),
    legend.text = element_text(size = 18),
    plot.caption = element_text(hjust = 0, size = 13, face = "italic")
  )

ggsave(
  glue("{datos}/Graficas/porcentaje_cesareas.jpg"),
  grafica_per_ces,
  width = 14,
  height = 10
)

## Mapa por departamentos ----

dpto_sufi_ces <- readxl::read_xlsx(
  glue("{datos}/Datos/xlsx/dpto_cesareas.xlsx")
)


mapa_dpto <- read_sf(
  glue("{datos}/Datos/shp/mapa_sd.shp")
)

# Agrupar para periodo 2018 - 2023

datos_dpto <- dpto_sufi_ces %>% 
  dplyr::group_by(
    DPTO,
    PARTO
  ) %>% 
  summarise(
    NUM_PARTOS = sum(NUM_PARTOS, na.rm = T)
  )

datos_dpto <- datos_dpto %>% 
  dplyr::group_by(
    DPTO
  ) %>% 
  dplyr::mutate(
    tot_nacimientos = sum(NUM_PARTOS, na.rm = T)
  ) %>% 
  ungroup()

datos_dpto <- datos_dpto %>% 
  filter(PARTO == "CESAREA") %>% 
  dplyr::mutate(
    per_ces = (NUM_PARTOS / tot_nacimientos) * 100
  )

mapa_dpto <- mapa_dpto %>% 
  left_join(
    datos_dpto,
    by = "DPTO"
  )

mapa_dpto <- mapa_dpto %>% 
  dplyr::mutate(nombre_corto = case_when(
    DPTO == "05" ~ "Antioq",
    DPTO == "08" ~ "Atlánt",
    DPTO == "11" ~ "Bogotá",
    DPTO == "13" ~ "Bolívar",
    DPTO == "15" ~ "Boyacá",
    DPTO == "17" ~ "Caldas",
    DPTO == "18" ~ "Caquetá",
    DPTO == "19" ~ "Cauca",
    DPTO == "20" ~ "Cesár",
    DPTO == "23" ~ "Córdoba",
    DPTO == "25" ~ "Cundina",
    DPTO == "27" ~ "Chocó",
    DPTO == "41" ~ "Huila",
    DPTO == "44" ~ "Guajir",
    DPTO == "47" ~ "Magdal",
    DPTO == "50" ~ "Meta",
    DPTO == "52" ~ "Nariño",
    DPTO == "54" ~ "N. Sant",
    DPTO == "63" ~ "Quindío",
    DPTO == "66" ~ "Risaral",
    DPTO == "68" ~ "Santand",
    DPTO == "70" ~ "Sucre",
    DPTO == "73" ~ "Tolima",
    DPTO == "76" ~ "Valle",
    DPTO == "81" ~ "Arauca",
    DPTO == "85" ~ "Casanar",
    DPTO == "86" ~ "Putuma",
    DPTO == "88" ~ "S. Andr",
    DPTO == "91" ~ "Amazon",
    DPTO == "94" ~ "Guainía",
    DPTO == "95" ~ "Guavia",
    DPTO == "97" ~ "Vaupés",
    DPTO == "99" ~ "Vichada",
    TRUE ~ "Otro"
   )
  )

mapa_per_ces <- mapa_dpto %>% 
  ggplot() +
  geom_sf(
    aes(
      fill = per_ces
    ),
    show.legend = F
  ) +
  ggrepel::geom_label_repel(
    data = mapa_dpto, ##%>% filter(per_ces >= 65),
    aes(label = paste0(round(per_ces,1)," %","\n", nombre_corto), , geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = Inf,
    box.padding = 0.15,
    segment.size = 0.2,
    seed = 42,
    size = 4,
    fontface = "bold",
    label.size = 0.2,            
    fill = scales::alpha("white", 0.6)  
  ) +
  scale_fill_gradient(
    low = "#7FFFD4",
    high = "#2F4F4F"
  ) +
  labs(
    fill = "Porcentaje \ncesáreas"
  )+
  theme_void() +
  theme(
    legend.position = c(0.1,0.2),
    legend.title = element_text(size = 16,
                                face = "bold"),
    legend.text = element_text(size = 14)
  )

ggsave(
  glue("{datos}/Graficas/mapa_porcentaje_cesareas_2018_2023.jpg"),
  mapa_per_ces,
  width = 14,
  height = 10,
  dpi = 300
)

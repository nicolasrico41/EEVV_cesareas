rm(list = ls())
library(tidyverse)
library(ggbreak)
library(plotly)
library(scales)
library(openxlsx)
library(ggrepel)
library(glue)
library(ggrepel)
library(glue)
# setwd("C:/Users/Usuario/OneDrive - ADRES/Bases de datos/PVS")

ruta_p <- "C:/Users/Usuario/OneDrive - ADRES/Bases de datos/Cesáreas"

pvs_poblacion <- readxl::read_xlsx(glue("{ruta_p}/Datos/xlsx/cesareas_general.xlsx"))

pvs_poblacion <- pvs_poblacion %>% 
  dplyr::rename(
    personas = `count(1)`,
    valor = `sum(NUM_VALOR_TOTAL)`,
    Anio = anio_servicio
  ) %>% 
  dplyr::filter(
    TIPO_PVS != "null"
  )

pvs_poblacion <- pvs_poblacion %>% 
  mutate(TIPO_PVS = case_when(
    TIPO_PVS == "PLANES DE ATENCIÓN COMPLEMENTARIA EN SALUD" ~ "Plan Atención Complementaria",
    TIPO_PVS == "PMP DIFFERENTE EPS" ~ "Plan de medicina prepagada de diferente EPS",
    TIPO_PVS == "PMP MISMA EPS" ~ "Plan de medicina prepagada de misma EPS",
    TIPO_PVS == "POLIZAS DE SALUD" ~ "Pólizas de salud",
    TIPO_PVS == "SIN PVS" ~ "Sin Plan Voluntario de Salud",
    T ~ TIPO_PVS)) %>% 
  dplyr::filter(Anio > 2017 & Anio < 2024)

colores <- c(
  "Plan Atención Complementaria" = "#528B8B",
  "Plan de Medicina Prepagada" = "#4E67B4",
  "Pólizas de salud" = "#79E9E8",
  "Varios Planes Voluntarios de Salud" = "#C1CDC1"
)

colores3 <- c(
  "Plan Atención Complementaria" = "#528B8B",
  "Plan de medicina prepagada de diferente EPS" = "#4E67B4",
  "Plan de medicina prepagada de misma EPS"="#00BFFF",
  "Pólizas de salud" = "cadetblue2",
  "Varios Planes Voluntarios de Salud" = "lightsteelblue3",
  "Sin Plan Voluntario de Salud"="#C1CDC1",
  "Plan de Medicina Prepagada" = "#4E67B9"
)

colores2 <- c(
  "Plan Atención Complementaria" = "black",
  "Plan de Medicina Prepagada" = "black",
  "Plan de medicina prepagada de diferente EPS" = "black",
  "Plan de medicina prepagada de misma EPS"="black",
  "Pólizas de salud" = "black",
  "Varios Planes Voluntarios de Salud" = "black"
)

pvs_poblacion <- pvs_poblacion %>% 
  filter(TIPO_PVS != "Sin Plan Voluntario de Salud",
         TIPO_PVS != "SUBSIDIADO")

label_total <- pvs_poblacion %>%
  group_by(Anio) %>%
  summarize(total_pob = sum(personas, na.rm = T))

orden_deseado <- c(
  "Plan Atención Complementaria",
  "Plan de medicina prepagada de diferente EPS",
  "Plan de medicina prepagada de misma EPS",
  "Pólizas de salud"
)

pvs_poblacion$TIPO_PVS <- factor(pvs_poblacion$TIPO_PVS, levels = orden_deseado)

pvs_poblacion <- pvs_poblacion %>%
  group_by(Anio) %>%
  arrange(Anio, desc(TIPO_PVS)) %>%  
  mutate(posicion_y = cumsum(personas) - personas / 2) %>%
  ungroup()

pvs_poblacion_ces <- pvs_poblacion %>% 
  dplyr::filter(
    PARTO == "CESAREA"
  )

## Gráfico área ----

grafico_area <- pvs_poblacion %>% 
  ggplot() +
  geom_area(
    aes(
      x = Anio,
      y = personas,
      fill = TIPO_PVS
    ),
    alpha = 0.7,
    color = "white",
    linewidth = 0.3
  ) +
  geom_text(
    aes(
      x = Anio,
      y = posicion_y,
      color = TIPO_PVS
    ),
    label = label_number(
      big.mark = ".",
      decimal.mark = ","
    )(pvs_poblacion$personas),
    size = 8,
    show.legend = F
  ) +
  geom_text(
    data = label_total,
    aes(
      x = Anio,
      y = total_pob
    ),
    label = label_number(
      big.mark = ".",
      decimal.mark = ","
    )(label_total$total_pob),
    show.legend = F,
    fontface = "bold",
    size = 8.5
  ) +
  # scale_y_continuous(
  #   labels = label_number(
  #     big.mark = ".",
  #     decimal.mark = ","
  #   ),
  #   breaks = seq(0, 2200000, by=500000)
  # ) +
  scale_x_continuous(
    breaks = seq(2018,2024,by = 1)
  ) +
  scale_color_manual(
    values = colores2
  ) +
  scale_fill_manual(
    values = colores3
  ) +
  labs(x = "",
       y = "",
       fill = "")+
  theme_minimal() +
  guides(
    fill = guide_legend(nrow = 2,
                        byrow = TRUE)
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 20),
    legend.text = element_text(size = 23),
    legend.spacing = unit(2, "cm")
  )

## Por departamento -----

pvs_dpto <- readxl::read_xlsx(glue("{ruta_p}/Datos/xlsx/cesareas_dpto.xlsx"))

mapa_dpto <- read_sf(glue("{ruta_p}/Datos/shp/mapa_sd.shp"))

datos_dpto <- pvs_dpto %>% 
  dplyr::rename(
    NUM_PARTOS = `count(1)`,
    valor_total = `sum(NUM_VALOR_TOTAL)`
  )
####

datos_dpto <- datos_dpto %>% 
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
  dplyr::mutate(
    nombre_corto = case_when(
    DPTO == "05" ~ "Antioquia",
    DPTO == "08" ~ "Atlántico",
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
    DPTO == "44" ~ "Guajira",
    DPTO == "47" ~ "Magdalena",
    DPTO == "50" ~ "Meta",
    DPTO == "52" ~ "Nariño",
    DPTO == "54" ~ "N. Sant",
    DPTO == "63" ~ "Quindío",
    DPTO == "66" ~ "Risaralda",
    DPTO == "68" ~ "Santander",
    DPTO == "70" ~ "Sucre",
    DPTO == "73" ~ "Tolima",
    DPTO == "76" ~ "Valle",
    DPTO == "81" ~ "Arauca",
    DPTO == "85" ~ "Casanare",
    DPTO == "86" ~ "Putumayo",
    DPTO == "88" ~ "S. Andr",
    DPTO == "91" ~ "Amazonas",
    DPTO == "94" ~ "Guainía",
    DPTO == "95" ~ "Guaviare",
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
    data = mapa_dpto,
    aes(label = paste0(
      scales::number(per_ces,
                     big.mark = ".",
                     decimal.mark = ",",
                     accuracy = 0.1),  
      "%\n",
      nombre_corto
    ),
        , geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = Inf,
    box.padding = 0.15,
    segment.size = 0.2,
    seed = 42,
    size = 4,
    fontface = "bold",
    label.size = 0.2,            
    fill = scales::alpha("white", 0.3)  
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
  glue("{ruta_p}/Graficas/mapa_porcentaje_cesareas_2018_2023_poblacional.jpg"),
  mapa_per_ces,
  width = 14,
  height = 10,
  dpi = 300
)

# Diferencia departamento -----

pvs_dpto <- pvs_dpto %>% 
  dplyr::rename(
    NUM_PARTOS = `count(1)`,
    valor_total = `sum(NUM_VALOR_TOTAL)`
  )

agregado_dpto <- pvs_dpto %>% 
  dplyr::filter(
    anio_servicio == 2018 | anio_servicio == 2023
  ) %>% 
  dplyr::group_by(
    DPTO,
    anio_servicio,
    PARTO
  ) %>% 
  dplyr::summarise(
    NUM_PARTOS = sum(NUM_PARTOS, na.rm = T),
    valor_total = sum(valor_total, na.rm = T)
  )

agregado_dpto <- agregado_dpto %>% 
  dplyr::group_by(
    DPTO,
    anio_servicio
  ) %>% 
  dplyr::mutate(
    casos_total = sum(NUM_PARTOS, na.rm = T),
    costo_total = sum(valor_total, na.rm = T)
  ) %>% 
  dplyr::ungroup()

agregado_dpto <- agregado_dpto %>% 
  dplyr::filter(
    PARTO != "VAGINAL"
  ) %>% 
  dplyr::mutate(
    per_ces = (NUM_PARTOS / casos_total) * 100,
    costo_per = costo_total / casos_total
  )

agregado_dpto_sel <- agregado_dpto %>% 
  dplyr::select(
    DPTO, anio_servicio, per_ces, costo_per, casos_total, costo_total
  )


agregado_dpto_pivot <- agregado_dpto_sel %>% 
  pivot_wider(
    names_from = anio_servicio,
    values_from = c(per_ces, costo_per, casos_total, costo_total)
  ) %>% 
  dplyr::mutate(
    dif_per_ces = per_ces_2023 - per_ces_2018,
    var_2018_2023_perca = ((costo_per_2023 - costo_per_2018)/costo_per_2018) * 100
  )

## Mapa de variacion positiva y negativa ----

mapa_dpto <- mapa_dpto %>% 
  left_join(
    agregado_dpto_pivot,
    by = "DPTO"
  )

x <- mapa_dpto$dif_per_ces
max_pos <- max(x, na.rm = TRUE)
min_neg <- min(x[x < 0], na.rm = TRUE)

mapa_diff_per_ces <- mapa_dpto %>% 
  ggplot() +
  geom_sf(
    aes(
      fill = dif_per_ces
    ),
    show.legend = F
  ) +
  ggrepel::geom_label_repel(
    data = mapa_dpto, 
    aes(label = paste0(
      scales::number(dif_per_ces,
                     big.mark = ".",
                     decimal.mark = ",",
                     accuracy = 0.1),  
      "%\n",
      nombre_corto
    ),
        , geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = Inf,
    box.padding = 0.15,
    segment.size = 0.2,
    seed = 42,
    size = 4,
    fontface = "bold",
    label.size = 0.2,            
    fill = scales::alpha("white", 0.3)  
  ) +
  scale_fill_gradientn(
    name   = "Δ % cesáreas",
    colours = c("#003300", "#FFFFFF", "#8B0000"),
    values  = rescale(c(min_neg, 0, max_pos), to = c(0, 1)),
    limits  = c(min_neg, max_pos),
    oob     = scales::squish,
    labels  = label_number(suffix = "%", accuracy = 0.1),
    space   = "Lab"
  ) +
  labs(
    fill = ""
  )+
  theme_void() +
  theme(
    legend.position = c(0.1,0.2),
    legend.title = element_text(size = 16,
                                face = "bold"),
    legend.text = element_text(size = 14)
  )

ggsave(
  glue("{ruta_p}/Graficas/mapa_diferencia_cesareas_2018_2023.jpg"),
  mapa_diff_per_ces,
  width = 14,
  height = 10,
  dpi = 300
)

## Mapa de variación porcentual costo per cápita ----

x_perca <- mapa_dpto$var_2018_2023_perca
max_pos_perca <- max(x_perca, na.rm = TRUE)
min_neg_perca <- min(x_perca[x_perca < 0], na.rm = TRUE)

mapa_var_perca_ces <- mapa_dpto %>% 
  ggplot() +
  geom_sf(
    aes(
      fill = var_2018_2023_perca
    ),
    show.legend = F
  ) +
  ggrepel::geom_label_repel(
    data = mapa_dpto, 
    aes(label = paste0(
      scales::number(var_2018_2023_perca,
                     big.mark = ".",
                     decimal.mark = ",",
                     accuracy = 0.1),  
      "%\n",
      nombre_corto
    ), 
        , geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = Inf,
    box.padding = 0.15,
    segment.size = 0.2,
    seed = 42,
    size = 4,
    fontface = "bold",
    label.size = 0.2,            
    fill = scales::alpha("white", 0.3)  
  ) +
  scale_fill_gradientn(
    name   = "Δ % cesáreas",
    colours = c("#003300", "#FFFFFF", "#8B0000"),
    values  = rescale(c(min_neg, 0, max_pos), to = c(0, 1)),
    limits  = c(min_neg_perca, max_pos_perca),
    oob     = scales::squish,
    labels  = label_number(suffix = "%", accuracy = 0.1),
    space   = "Lab"
  ) +
  labs(
    fill = ""
  )+
  theme_void() +
  theme(
    legend.position = c(0.1,0.2),
    legend.title = element_text(size = 16,
                                face = "bold"),
    legend.text = element_text(size = 14)
  )

ggsave(
  glue("{ruta_p}/Graficas/mapa_var_percapita_cesareas_2018_2023.jpg"),
  mapa_var_perca_ces,
  width = 14,
  height = 10,
  dpi = 300
)

## Mapa promedio costo cesareas ----

dpto_totales <- pvs_dpto %>% 
  dplyr::filter(
    PARTO == "CESAREA"
  ) %>% 
  dplyr::group_by(
    DPTO
  ) %>% 
  dplyr::summarise(
    conteo_ces = sum(NUM_PARTOS, na.rm = T),
    valor_tot_ces = sum(valor_total, na.rm = T)
  ) %>% 
  dplyr::mutate(
    perca_prom = valor_tot_ces/conteo_ces
  )

mapa_dpto <- mapa_dpto %>% 
  left_join(
    dpto_totales,
    by = "DPTO"
  )

mapa_perca_prom <- mapa_dpto %>% 
  ggplot() +
  geom_sf(
    aes(
      fill = perca_prom
    ),
    show.legend = F
  ) +
  ggrepel::geom_label_repel(
    data = mapa_dpto,
    aes(label = paste0(
      scales::number(perca_prom,
                     big.mark = ".",
                     decimal.mark = ",",
                     accuracy = 1),
      "\n",
      nombre_corto
    ),
        geometry = geometry),
    stat = "sf_coordinates",
    min.segment.length = 0,
    max.overlaps = Inf,
    box.padding = 0.15,
    segment.size = 0.2,
    seed = 42,
    size = 4,
    fontface = "bold",
    label.size = 0.2,            
    fill = scales::alpha("white", 0.3)  
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
  glue("{ruta_p}/Graficas/mapa_perca_prom_2018_2023.jpg"),
  mapa_perca_prom,
  width = 14,
  height = 10,
  dpi = 300
)

## Gráficos sección 3 ----

graph_pvs_dpto <- pvs_dpto %>% 
  dplyr::filter(
    !TIPO_PVS %in% c("null", "SUBSIDIADO")
  ) %>% 
  dplyr::mutate(
    TIPO_PVS = case_when(
      TIPO_PVS == "SIN PVS" ~ "Sin PVS",
      TRUE ~ "Con PVS"
    )
  ) %>% 
  dplyr::group_by(
    DPTO,
    TIPO_PVS,
    PARTO
  ) %>% 
  dplyr::summarise(
    partos = sum(NUM_PARTOS, na.rm = T),
    valor = sum(valor_total, na.rm = T)
  ) %>% 
  dplyr::group_by(
    DPTO,
    TIPO_PVS
  ) %>% 
  dplyr::mutate(
    total_partos = sum(partos, na.rm = T),
    total_valor = sum(valor, na.rm = T)
  ) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(
    PARTO == "CESAREA"
  ) %>% 
  pivot_wider(
    names_from = TIPO_PVS,
    values_from = c(partos, valor, total_partos, total_valor)
  ) %>% 
  dplyr::mutate(
    nombre_corto = case_when(
      DPTO == "05" ~ "Antioquia",
      DPTO == "08" ~ "Atlántico",
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
      DPTO == "44" ~ "Guajira",
      DPTO == "47" ~ "Magdalena",
      DPTO == "50" ~ "Meta",
      DPTO == "52" ~ "Nariño",
      DPTO == "54" ~ "N. Sant",
      DPTO == "63" ~ "Quindío",
      DPTO == "66" ~ "Risaralda",
      DPTO == "68" ~ "Santander",
      DPTO == "70" ~ "Sucre",
      DPTO == "73" ~ "Tolima",
      DPTO == "76" ~ "Valle",
      DPTO == "81" ~ "Arauca",
      DPTO == "85" ~ "Casanare",
      DPTO == "86" ~ "Putumayo",
      DPTO == "88" ~ "S. Andr",
      DPTO == "91" ~ "Amazonas",
      DPTO == "94" ~ "Guainía",
      DPTO == "95" ~ "Guaviare",
      DPTO == "97" ~ "Vaupés",
      DPTO == "99" ~ "Vichada",
      TRUE ~ "Otro"
    ),
    per_ces_con_pvs = (`partos_Con PVS` / `total_partos_Con PVS`) * 100,
    per_ces_sin_pvs = (`partos_Sin PVS` / `total_partos_Sin PVS`) * 100
  )

grafico_puntos <- graph_pvs_dpto %>% 
  mutate(nombre_corto = reorder(
    nombre_corto,
    per_ces_con_pvs,
    na.last = TRUE
    )
  ) %>%
  ggplot() +
  geom_point(
    aes(
      x = nombre_corto,
      y = per_ces_con_pvs,
      color = "PVS cesárea"
    ),
    size = 2.5
  ) +
  geom_point(
    aes(
      x = nombre_corto,
      y = per_ces_sin_pvs,
      color = "No PVS cesárea"
    ),
    size = 2.5
  ) +
  geom_segment(
    aes(
    x = nombre_corto, xend = nombre_corto,
    y =per_ces_sin_pvs , yend = per_ces_con_pvs),
    color = "gray40", linetype = "dashed"
  ) +
  scale_color_manual(
    name = "",
    values = c("PVS cesárea" = "red", 
               "No PVS cesárea" = "blue")
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      suffix = " %",
      big.mark = ".",
      decimal.mark = ","
    )
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "",
    y = ""
  ) +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 20),
    axis.text = element_text(size = 20)
  )

ggsave(
  glue("{ruta_p}/Graficas/dpto_pvs_compar.jpg"),
  grafico_puntos,
  width = 10,
  height = 14,
  dpi = 300
)

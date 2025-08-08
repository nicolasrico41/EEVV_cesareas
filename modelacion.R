## Objetivo es realizar el modelo logístico ####

## Cargar paquetes ----

rm(list=ls())

packageList<-c("tidyverse", "stringr", "RCurl", "glue","rgeos","readr","ggrepel",
               "haven","raster","rgdal","survey", "readxl", "gridExtra","xlsx",
               "XLConnect", "scales","colorspace", "broom","janitor","rlang",
               "samplingbook", "sf", "openxlsx", "RSQLite", "DBI", "networkD3",
               "ggalluvial", "ggplot2", "skimr", "lubridate", "svglite", "httr",
               "jsonlite", "purrr","robotstxt", "XML", "ggdensity", "gganimate",
               "ggpubr", "forecast", "tsibble", "timetk", "broom", "zoo",
               "synthdid", "glue", "pracma", "BMisc", "nlme", "pROC", "caret",
               "FactoMineR", "factoextra", "lavaan", "knitr", "sjPlot")

lapply(packageList,require,character.only=TRUE)

## Fuente de datos ----

datos <- "C:/Users/Usuario/OneDrive - ADRES/Bases de datos/Cesáreas"

sufi_ces <- readr::read_csv(
  glue("{datos}/Datos/csv/Análisis_cesáreas.csv")
)

sufi_ces <- sufi_ces %>% 
  dplyr::filter(
    numero_parto <= 6,
    edad_servidio <=50,
    TIPO_PVS != "null"
  )
# Acomodar a variables factores ----

sufi_ces <- sufi_ces %>% 
  dplyr::mutate(
    pormil_prestadores = (cantidad_prestadores / pob) * 1000,
    pormil_camas_adul = (camas_adultos / pob) * 1000,
    pormil_camas_pedi = (camas_pediatricas / pob) * 1000,
    pormil_salas_partos = (salas_partos / pob) * 1000,
    pormil_camas_cui_neonat = (camas_cuidado_basico_neonatal / pob) * 1000,
    pormil_salas_proce = (salas_procedimientos / pob) * 1000,
    Tipo_parto = as.factor(Tipo_parto),
    TIPO_PVS = as.factor(TIPO_PVS),
    clasificacion_edad = as.factor(case_when(
      edad_servidio < 20 ~ "Embarazo adolescente",
      edad_servidio >= 20 & edad_servidio < 34 ~ "Edad típica",
      T ~ "Maternal avanzado"
    ))
  ) %>% 
  dplyr::mutate(
    Tipo_parto = relevel(Tipo_parto, ref = "CESAREA"),
    TIPO_PVS = relevel(TIPO_PVS, ref = "SIN PVS"),
    clasificacion_edad = relevel(clasificacion_edad, ref = "Edad típica"),
    STR_REGIMEN = case_when(
      STR_REGIMEN %in% c("C", "M") ~ "Contributivo",
      TRUE ~ "Subsidiado"
    )
  )

## Distribución suficiencia tipo ----

sufi_ces_nac <- sufi_ces %>% 
  dplyr::group_by(STR_REGIMEN, Tipo_parto, anio_servicio) %>% 
  summarise(
    num_partos = n()
  )

sufi_ces_nac <- sufi_ces_nac %>% 
  dplyr::group_by(STR_REGIMEN, anio_servicio) %>% 
  dplyr::mutate(
    tot_partos = sum(num_partos, na.rm = T)
  ) %>% 
  ungroup()

sufi_ces_nac <- sufi_ces_nac %>% 
  dplyr::mutate(
    per_partos = (num_partos/tot_partos)*100
  )

Paleta_colores_ADRES_ordenada <- c(
  
  "#00E6E8", "#00E0E0", "#00DADC", "#01C4C7", "#01B3B6",
  
  "#00CCCC", "#00B4B3", "#00B1B3", "#009EA0", "#00A3A3",
  
  "#008F8F", "#007375", "#016B6D", "#016061", "#02787A",
  
  "#003B3D", "#004D4F", "#003133", "#00292B", "#01292B",
  
  "#012224", "#022123", "#001819", "#001112", "#000809",
  
  "#000203"
  
)

## Grafico por tipo de régimen

graf_regimen_tipo <- sufi_ces_nac %>% 
  ggplot(
    aes(
      x = anio_servicio,
      y = per_partos,
      color = Tipo_parto
    )
  ) +
  geom_line(
    linewidth = 1.3
  ) +
  facet_wrap(~ STR_REGIMEN) +
  scale_y_continuous(
    labels = scales::label_number(
      suffix = " %"
    )
  ) +
  scale_color_manual(
    values = c(
      "CESAREA" = "#1874CD",
      "VAGINAL" = "#7FFFD4"
    )
  ) +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16,
                              face = "bold"),
    legend.text = element_text(size = 16)
  )

ggsave(
  glue("{datos}/Graficas/tipo_parto_regimen_tendencia.jpg"),
  graf_regimen_tipo,
  width = 12,
  height = 8,
  dpi = 300
)


## Gráfico para PVS ----

sufi_ces_pvs <- sufi_ces %>% 
  dplyr::filter(TIPO_PVS != "null") %>% 
  dplyr::group_by(
    anio_servicio,
    TIPO_PVS,
    Tipo_parto
  ) %>% 
  dplyr::summarise(
    num_partos = n()
  )

sufi_ces_pvs <- sufi_ces_pvs %>% 
  dplyr::group_by(
    anio_servicio, TIPO_PVS
  ) %>% 
  dplyr::mutate(
    tot_partos = sum(num_partos, na.rm = T)
  ) %>% 
  ungroup()

sufi_ces_pvs <- sufi_ces_pvs %>% 
  dplyr::mutate(
    per_parto = (num_partos/tot_partos)*100
  )

graf_regimen_pvs <- sufi_ces_pvs %>% 
  dplyr::mutate(
    TIPO_PVS = case_when(
      TIPO_PVS == "PLANES DE ATENCIÓN COMPLEMENTARIA EN SALUD" ~ "PLANES DE ATENCIÓN\n COMPLEMENTARIA EN SALUD",
      T ~ TIPO_PVS
    )
  ) %>% 
  ggplot(
    aes(
      x = anio_servicio,
      y = per_parto,
      color = Tipo_parto
    )
  ) +
  geom_line(
    linewidth = 1.3
  ) +
  facet_wrap(~ TIPO_PVS) +
  scale_y_continuous(
    labels = scales::label_number(
      suffix = " %"
    )
  ) +
  scale_color_manual(
    values = c(
      "CESAREA" = "#1874CD",
      "VAGINAL" = "#7FFFD4"
    )
  ) +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    color = ""
  ) +
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    strip.text = element_text(size = 16,
                              face = "bold"),
    legend.text = element_text(size = 16)
  )

ggsave(
  glue("{datos}/Graficas/tipo_parto_pvs_tendencia.jpg"),
  graf_regimen_pvs,
  width = 12,
  height = 8,
  dpi = 300
)

## Cuadro para PVS ----

resumen_pvs <- sufi_ces %>% 
  dplyr::filter(
    TIPO_PVS != "null"
  ) %>% 
  dplyr::group_by(
    TIPO_PVS,
    Tipo_parto
  ) %>% 
  summarise(
    partos = n()
  ) %>% 
  pivot_wider(
    names_from = Tipo_parto,
    values_from = partos
  ) %>% 
  dplyr::mutate(
    Total_partos = CESAREA + VAGINAL,
    per_cesa = round((CESAREA / (CESAREA + VAGINAL)) * 100,1),
    per_total = round((CESAREA + VAGINAL) / nrow(sufi_ces %>% dplyr::filter(TIPO_PVS!="null"))*100,1)
  ) %>%
  arrange(-per_cesa)
  
write.xlsx(
  resumen_pvs,
  glue("{datos}/Datos/xlsx/resumen_variables_pvs.xlsx"),
  overwrite = T
)

## Cuadro de descripción de variables ----

variables <- c(
   "numero_parto", "cesarea_previa",
  "sobre_peso", "diabetes", "hipertension"
)

resumen <- sufi_ces %>%
  pivot_longer(cols = all_of(variables), names_to = "variable", values_to = "valor") %>%
  group_by(variable, valor, Tipo_parto) %>%
  summarise(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = Tipo_parto, values_from = n, values_fill = 0) %>%
  mutate(
    Total = CESAREA + VAGINAL,
    Porcentaje_cesareas = round(CESAREA / Total * 100, 1)
  ) %>%
  arrange(variable, desc(Porcentaje_cesareas)) %>% 
  dplyr::mutate(
    TOTAL_REGISTROS = nrow(sufi_ces),
    per_tot_registros = (Total / nrow(sufi_ces)) * 100
  )

write.xlsx(
  resumen,
  glue("{datos}/Datos/xlsx/resumen_variables.xlsx"),
  overwrite = T
)

resumen %>%
  dplyr::select(variable, valor, CESAREA, VAGINAL, Total, Porcentaje_cesareas, per_tot_registros) %>%
  kable(digits = 1, caption = "Distribución de partos por variable y tipo de parto")

## Modelo logit PVS ----

modelo_null_pvs <- glm(Tipo_parto ~ 1, data = sufi_ces, family = binomial)

modelo_logit_pvs <- glm(Tipo_parto ~ TIPO_PVS,
                        data = sufi_ces,
                        family = binomial)

summary(modelo_logit_pvs)

# Salida de tabla

tab_model(modelo_logit_pvs, show.se = TRUE, show.ci = TRUE, show.p = TRUE)

## Pseudo R2 - R2 de McFadden

ll_modelo_pvs <- as.numeric(logLik(modelo_logit_pvs))

modelo_nulo_pvs <- update(modelo_logit_pvs, . ~ 1)
ll_nulo_pvs   <- as.numeric(logLik(modelo_nulo_pvs))

R2_McFadden_pvs <- 1 - ll_modelo_pvs / ll_nulo_pvs
R2_McFadden_pvs

## Modelo logit interactivo ----

modelo_logit_pvs_int <- glm(Tipo_parto ~ TIPO_PVS*cesarea_previa + TIPO_PVS*numero_parto,
                        data = sufi_ces,
                        family = binomial)

summary(modelo_logit_pvs_int)

# Salida de tabla

tab_model(modelo_logit_pvs_int, show.se = TRUE, show.ci = TRUE, show.p = TRUE)


ll_modelo_pvs_int <- as.numeric(logLik(modelo_logit_pvs_int))

modelo_nulo_pvs_int <- update(modelo_logit_pvs_int, . ~ 1)
ll_nulo_pvs_int   <- as.numeric(logLik(modelo_nulo_pvs_int))

R2_McFadden_pvs_int <- 1 - ll_modelo_pvs_int / ll_nulo_pvs_int
R2_McFadden_pvs_int

probs_pvs_int <- predict(modelo_logit_pvs_int, type = "response")

roc_obj_pvs_int <- roc(sufi_ces$Tipo_parto, probs_pvs_int)
plot(roc_obj_pvs_int, main = sprintf("ROC curve (AUC = %.3f)", auc(roc_obj_pvs_int)))

# Ver verosimilitud del nulo respecto al full

anova(modelo_null, modelo_logit_pvs_int, test = "Chisq")

## Modelo logit ----

modelo_logit <- glm(Tipo_parto ~ clasificacion_edad + numero_parto + pormil_prestadores +
                      pormil_camas_adul + pormil_camas_pedi + pormil_salas_partos +
                      pormil_camas_cui_neonat + pormil_salas_proce + cesarea_previa +
                      sobre_peso + diabetes + hipertension,
                    data = sufi_ces,
                    family = binomial)

summary(modelo_logit)

# Salida de tabla

tab_model(modelo_logit, show.se = TRUE, show.ci = TRUE, show.p = TRUE)

probs <- predict(modelo_logit, type = "response")

## Pseudo R2 - R2 de McFadden

ll_modelo <- as.numeric(logLik(modelo_logit))

modelo_nulo <- update(modelo_logit, . ~ 1)
ll_nulo   <- as.numeric(logLik(modelo_nulo))

R2_McFadden <- 1 - ll_modelo / ll_nulo
R2_McFadden

## Curva ROC y AUC

roc_obj <- roc(sufi_ces$Tipo_parto, probs)
plot(roc_obj, main = sprintf("ROC curve (AUC = %.3f)", auc(roc_obj)))

## Matriz de confusión

pred_class <- factor(ifelse(probs >= 0.5, 1, 0),
                     levels = c(0,1))
obs_class  <- factor(sufi_ces$Tipo_parto, levels = c(0,1))

conf_mat <- confusionMatrix(pred_class, obs_class, positive = "1")
print(conf_mat)

## ANÁLISIS FACTORIAL ----

sufi_ces[] <- lapply(sufi_ces, function(x) if(is.character(x)) as.factor(x) else x)

base_afm <- sufi_ces %>% 
  dplyr::select(
    clasificacion_edad, numero_parto, pormil_prestadores,
    pormil_camas_adul, pormil_camas_pedi, pormil_salas_partos,
    pormil_camas_cui_neonat, pormil_salas_proce, cesarea_previa,
    sobre_peso, diabetes, hipertension
  ) %>% 
  dplyr::mutate(
    cesarea_previa = as.factor(cesarea_previa),
    sobre_peso = as.factor(sobre_peso),
    diabetes = as.factor(diabetes),
    hipertension = as.factor(hipertension)
  )



## Crear el AFM

grupo <- c(2,6,4)
nombres_grupos <- c("Mujer", "Sociodemográficas", "Salud")
type = c("m", "c", "n")

afm_resultado <- MFA(
  base_afm,
  group = grupo,
  type = type,
  name.group = nombres_grupos,
  graph = F
)

summary(afm_resultado)

fviz_mfa_ind(afm_resultado, palette = "jco")
fviz_mfa_var(afm_resultado, "group")


## Logit con AFM ----

coord_mfa <- as.data.frame(afm_resultado$ind$coord[, 1:4])
names(coord_mfa) <- c("Dim1", "Dim2", "Dim3", "Dim4")

modelo_data <- bind_cols(coord_mfa, cesarea_binaria = sufi_ces$Tipo_parto)

modelo_logit_afm <- glm(cesarea_binaria ~ Dim1 + Dim2 + Dim3 + Dim4, 
                    data = modelo_data, 
                    family = binomial)

summary(modelo_logit_afm)

probs_afm <- predict(modelo_logit_afm, type = "response")

# Salida de tabla

tab_model(modelo_logit_afm, show.se = TRUE, show.ci = TRUE, show.p = TRUE)

## Calidad del modelo

## Pseudo R2 - R2 de McFadden

ll_modelo_afm <- as.numeric(logLik(modelo_logit_afm))

modelo_nulo_afm <- update(modelo_logit_afm, . ~ 1)
ll_nulo_afm   <- as.numeric(logLik(modelo_nulo_afm))

R2_McFadden_afm <- 1 - ll_modelo_afm / ll_nulo_afm
R2_McFadden_afm

## Curva ROC y AUC

roc_obj_afm <- roc(sufi_ces$Tipo_parto, probs_afm)
plot(roc_obj_afm, main = sprintf("ROC curve (AUC = %.3f)", auc(roc_obj_afm)))

## Análisis de distibución de datos ----

variables <- names(base_afm)
dummies <- c("cesarea_previa", "sobre_peso", "diabetes","hipertension")

funcion_boxplot <- function(x){
  
  graf_box <- sufi_ces %>% 
    ggplot(
      aes(x = Tipo_parto,
          y = !!rlang::sym(x))
    ) +
    geom_boxplot() +
    theme_minimal() +
    labs(x = "",
         y = glue("{x}")
    ) +
    theme(
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 16)
    )
  
  ggsave(
    glue("{datos}/Graficas/boxplot_{x}.jpg"),
    graf_box,
    width = 12,
    height = 8,
    dpi = 300
  )
}

lapply(variables, funcion_boxplot)

funcion_bar <- function(x){
  
  graf <- sufi_ces %>%
    ggplot(
      aes(x = as.factor(!!rlang::sym(x)),
          fill = Tipo_parto)
    ) + 
    geom_bar(
      position = "dodge"
    ) +
    geom_text(
      stat = "count",
      aes(label = after_stat(scales::label_number(big.mark = ".", decimal.mark = ",")(count))),
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      fontface = "bold",
      size = 5
    ) +
    scale_y_continuous(
      labels = scales::label_number(
        big.mark = ".",
        decimal.mark = ","
      )
    ) +
    scale_fill_manual(
      values = c(
        "CESAREA" = "#1874CD",
        "VAGINAL" = "#2F4F4F"
      )
    ) +
    theme_minimal() +
    labs(
      x = glue("{x}"),
      y = "Número de partos",
      fill = ""
    ) +
    theme(
      legend.position = "bottom",
      axis.text = element_text(size = 15),
      legend.text = element_text(size = 16),
      axis.title = element_text(size = 15)
    )
  
  ggsave(
    glue("{datos}/Graficas/bar_{x}.jpg"),
    graf,
    width = 12,
    height = 8,
    dpi = 300
  )
}
lapply(dummies, funcion_bar)

## Promedio dummy 

funcion_porcentaje_dummy <- function(x){
  
  graf <- sufi_ces %>%
    ggplot(
      aes(x = Tipo_parto,
          y = !!rlang::sym(x),
          fill = Tipo_parto)
    ) +
    stat_summary(
      fun = mean,
      geom = "col",
      position = position_dodge(width = 0.9)
    ) +
    stat_summary(
      fun = mean,
      geom = "text",
      aes(label = after_stat(paste0(round(y * 100, 1), "%"))),
      position = position_dodge(width = 0.9),
      vjust = -0.5,
      fontface = "bold",
      size = 5
    ) +
    scale_y_continuous(
      labels = scales::label_percent(scale = 100, big.mark = ".", decimal.mark = ",")
    ) +
    scale_fill_manual(
      values = c(
        "CESAREA" = "#1874CD",
        "VAGINAL" = "#2F4F4F"
      )
    ) +
    theme_minimal() +
    labs(
      title = glue("{x}"),
      x = "",
      y = "Porcentaje",
      fill = ""
    ) +
    theme(
      legend.position = "none",
      axis.text = element_text(size = 15),
      axis.title = element_text(size = 15),
      plot.title = element_text(size = 18, hjust = 0.5)
    ) 
  
  ggsave(
    glue("{datos}/Graficas/porcentaje_{x}.jpg"),
    graf,
    width = 12,
    height = 8,
    dpi = 300
  )
}

lapply(dummies, funcion_porcentaje_dummy)

## MODELO FUNCIÓN ITERATIVA ----

variables_funcion <- names(base_afm)
tipo_parto <- "Tipo_parto"


comb_list <- unlist(
  lapply(1:length(variables_funcion), function(i) {
    combn(variables_funcion, i, simplify = FALSE)
  }),
  recursive = FALSE
)

# Función loop modelos logit

resultados <- lapply(comb_list, function(vars) {
  
  formula_str <- as.formula(paste(tipo_parto, "~",
                                  paste(vars, collapse = " + ")))
  
  modelo_logit <- glm(formula_str, 
                      data = sufi_ces,
                      family = binomial)
  
  ll_modelo <- as.numeric(logLik(modelo_logit))
  
  modelo_nulo <- glm(as.formula(paste(tipo_parto, "~ 1")),
                     data = sufi_ces,
                     family = binomial)
  ll_nulo <- as.numeric(logLik(modelo_nulo))
  
  r2_mcfadden <- 1 - (ll_modelo / ll_nulo)
  
  list(
    variables = paste(vars, collapse = " + "),
    r2_mcfadden = r2_mcfadden
  )
}
)

df_resultados <- do.call(rbind, lapply(resultados, as.data.frame))
df_resultados <- df_resultados[order(-df_resultados$r2_mcfadden), ]

print(df_resultados)

write.xlsx(df_resultados,
           glue("{datos}/Datos/xlsx/resultados_logit.xlsx")
           )


# Cargar librerías y datos -----------------------------------------------------
pacman::p_load(tidyverse,
               sjmisc,
               knitr,
               kableExtra,
               sjPlot,
               stargazer,
               janitor,
               crosstable,
               table1,
               haven,
               psych,
               car,
               summarytools,
               ggplot2,
               dplyr,
               hrbrthemes) 
if (!require(survey)) install.packages('survey'); library(survey)
if (!require(calidad)) install.packages('calidad'); library(calidad)

options(scipen = 999)
rm(list = ls())

enut2023 <- readRDS("input/250117-ii-enut-bdd-r.rds")

# 1. FIltrar y seleccionar variables -------------------------------------------
## a Selección de variables de interés ---------------------------------------

data <- enut2023 %>%
  select(
    edad,
    sexo,
    t_tcnr_dt,  #trabajo cuidados no remunerado
    t_tdnr_dt,  #trabajo doméstico no remunerado para el propio hogar
    t_tvaoh_dt, #trabajo voluntario y ayudas a otros hogares
    t_cpaf_dt, t_vsyomcm_dt, # bienestar objetivo
    bs1, #satisfaccion con la vida
    bs12, bs13, bs15, bs16, bs17, bs18, bs19 #bienestar subjetivo
  )

## Muestra >=60 años------------------------------
data <- data %>%
  filter(edad >= 60)

names(data)
dim(data)

# Recodificación de NAs------------------------------
## preguntas encadenadas, por lo que NA = no, por lo que NA = 0

data <- data %>%
  mutate(across(ends_with("_dt"), ~coalesce(., 0.00000000)))

data <- data %>%
  mutate(across(ends_with("_dt"), ~ifelse(. == 96.000000, NA, .)))

data<- data %>% 
  mutate(across(starts_with("bs"), ~ifelse(. == 85, NA, .)))

data<- data %>% 
  mutate(across(starts_with("bs"), ~ifelse(. == 99, NA, .)))

colSums(is.na(data)) 

# recodificar direccion

data <- data %>% 
  mutate(bs12 = case_when(
    bs12 == 1 ~ "5",
    bs12 == 2 ~ "4",
    bs12 == 4 ~ "2",
    bs12 == 5 ~ "1"))

data <- data %>% 
  mutate(bs13 = case_when(
    bs13 == 1 ~ "5",
    bs13 == 2 ~ "4",
    bs13 == 4 ~ "2",
    bs13 == 5 ~ "1"))

data <- data %>% 
  mutate(bs15 = case_when(
    bs15 == 1 ~ "5",
    bs15 == 2 ~ "4",
    bs15 == 4 ~ "2",
    bs15 == 5 ~ "1"))

data <- data %>% 
  mutate(bs16 = case_when(
    bs16 == 1 ~ "5",
    bs16 == 2 ~ "4",
    bs16 == 4 ~ "2",
    bs16 == 5 ~ "1"))

data <- data %>% 
  mutate(bs17 = case_when(
    bs17 == 1 ~ "5",
    bs17 == 2 ~ "4",
    bs17 == 4 ~ "2",
    bs17 == 5 ~ "1"))

data <- data %>% 
  mutate(bs18 = case_when(
    bs18 == 1 ~ "5",
    bs18 == 2 ~ "4",
    bs18 == 4 ~ "2",
    bs18 == 5 ~ "1"))

data <- data %>% 
  mutate(bs19 = case_when(
    bs19 == 1 ~ "5",
    bs19 == 2 ~ "4",
    bs19 == 4 ~ "2",
    bs19 == 5 ~ "1"))

# convertir en numericas 

data$bs12 <- as.numeric(data$bs12)
data$bs13 <- as.numeric(data$bs13)
data$bs15 <- as.numeric(data$bs15)
data$bs16 <- as.numeric(data$bs16)
data$bs17 <- as.numeric(data$bs17)
data$bs18 <- as.numeric(data$bs18)
data$bs19 <- as.numeric(data$bs19)

# trabajo no remunerado total--------------------------------

data$t_nr <- data$t_tdnr_dt + data$t_tcnr_dt + data$t_tvaoh_dt

describe(data$t_nr)

summary(data$t_nr)

#tiempo dedicado a bienestar objetivo------------------------

data$cpaf <- data$t_cpaf_dt + data$t_vsyomcm_dt

# eliminar valores fuera de rango 

limite_inferior <- 0
limite_superior <- 24

data <- data %>%
  filter(t_nr >= limite_inferior & t_nr <= limite_superior,
         cpaf >= limite_inferior & cpaf <= limite_superior)


## escala sumativa bienestar subjetivo

data$escala_bsc <- rowMeans(data[ c("bs12", "bs13", "bs15", "bs16", "bs17", "bs18", "bs19")], 
                            na.rm = TRUE)
summary(data$escala_bsc)

# bivariados -----------------------------------------------

cor.test(data$cpaf, data$t_nr, method = "pearson", use = "complete.obs")
cor.test(data$bs1, data$t_nr, method = "spearman", use = "complete.obs")

library(sjPlot)

# Crear dataframe con resultados
tabla_resultados <- data.frame(
  Variables = c("Bienestar objetivo ~ Horas de trabajo no remunerado", "Satisfacción con la vida ~ Horas de trabajo no remunerado"),
  Método = c("Pearson", "Spearman"),
  Correlación = c(
    cor.test(data$cpaf, data$t_nr, method = "pearson")$estimate,
    cor.test(data$bs1, data$t_nr, method = "spearman")$estimate
  ),
  `Valor p` = c(
    cor.test(data$cpaf, data$t_nr, method = "pearson")$p.value,
    cor.test(data$bs1, data$t_nr, method = "spearman")$p.value
  ),
  `IC 95%` = c(
    paste(round(cor.test(data$cpaf, data$t_nr)$conf.int, 2), collapse = " a "),
    paste(round(cor.test(data$bs1, data$t_nr)$conf.int, 2), collapse = " a ")
  )
)

# Mostrar tabla bonita
tab_df(tabla_resultados,
       title = "Resultados de Correlación",
       show.rownames = FALSE,
       alternate.rows = TRUE)

selected_vars <- data %>%  
  select(t_nr, cpaf)

M <- cor(selected_vars, use = "pairwise.complete.obs") #matriz de correlaciom

n_observaciones <- nrow(na.omit(data[, colnames(M)]))
print(n_observaciones)

rownames(M) <- c("Horas de trabajo no remunerado", 
                 "Bienestar objetivo")
colnames(M) <- rownames(M) 

corrplot::corrplot(M,
                   method = "color",
                   tl.cex = 0.6,
                   mar = c(0, 0, 0, 0),
                   addCoef.col = "black",
                   number.cex = 0.6,
                   cl.cex = 0.6,
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#a118f3", "white",  "#ff0cae"))(12),
                   bg = "white",
                   na.label = "-")





sjPlot::tab_corr(
  selected_vars,
  triangle = "lower",
  var.labels = rownames(M),
  show.p = TRUE,                     # Mostrar valores p
  p.numeric = TRUE,                  # Mostrar valores p numéricos
  digits = 2,                        # Número de decimales
  file = NULL                        # Puedes poner un nombre para exportar a HTML
)

# filtrar por mujeres

mujeres <- data %>% filter(sexo == 2)

cor.test(mujeres$escala_bsc, mujeres$t_nr, method = "spearman", use = "complete.obs")
cor.test(mujeres$cpaf, mujeres$t_nr, method = "pearson", use = "complete.obs")
cor.test(mujeres$bs1, mujeres$t_nr, method = "spearman", use = "complete.obs")

sjPlot::plot_scatter(data = mujeres, # diagrama de dispersión
                     x = t_nr,
                     y = escala_bsc)

sjPlot::plot_scatter(data = mujeres, # diagrama de dispersión
                     x = t_nr,
                     y = cpaf)

selected_vars_muj <- mujeres %>%  
  select(t_nr, cpaf, escala_bsc, bs1)

# matriz pairwise

M_mujeres <- cor(selected_vars_muj, use = "pairwise.complete.obs", method = "spearman") #matriz de correlaciom

rownames(M_mujeres) <- c("Horas de trabajo no remunerado", 
                 "Bienestar objetivo", 
                 "Bienestar subjetivo",
                 "Satisfacción con la vida")
colnames(M_mujeres) <- rownames(M_mujeres) 

corrplot::corrplot(M_mujeres,
                   method = "color",
                   tl.cex = 0.6,
                   mar = c(0, 0, 0, 0),
                   addCoef.col = "black",
                   number.cex = 0.6,
                   cl.cex = 0.6,
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#a118f3", "white",  "#ff0cae"))(12),
                   bg = "white",
                   na.label = "-")

#matriz listwise

sjPlot::tab_corr(selected_vars_muj, 
                 triangle = "lower")

M_mujeres2 <- cor(selected_vars_muj, use = "complete.obs", method = "spearman")
rownames(M_mujeres2) <- c("Horas de trabajo no remunerado", 
                         "Bienestar objetivo", 
                         "Bienestar subjetivo",
                         "Satisfacción con la vida")
colnames(M_mujeres2) <- rownames(M_mujeres2) 

corrplot::corrplot(M_mujeres2,
                   method = "color",
                   tl.cex = 0.6,
                   mar = c(0, 0, 0, 0),
                   addCoef.col = "black",
                   number.cex = 0.6,
                   cl.cex = 0.6,
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#a118f3", "white",  "#ff0cae"))(12),
                   bg = "white",
                   na.label = "-")

# filtrar por hombres

hombres <- data %>% filter(sexo == 1)

cor.test(hombres$escala_bsc, hombres$t_nr, method = "spearman", use = "complete.obs")
cor.test(hombres$cpaf, hombres$t_nr, method = "pearson", use = "complete.obs")
cor.test(hombres$bs1, hombres$t_nr, method = "spearman", use = "complete.obs")

cor.test(hombres$escala_bsc, hombres$t_nr, method = "spearman", use = "complete.obs")

sjPlot::plot_scatter(data = hombres, # diagrama de dispersión
                     x = t_nr,
                     y = escala_bsc)

sjPlot::plot_scatter(data = hombres, # diagrama de dispersión
                     x = t_nr,
                     y = cpaf)

selected_vars_hom <- hombres %>%  
  select(t_nr, cpaf, escala_bsc, bs1)

#pairwise

M_hombres <- cor(selected_vars_hom, use = "pairwise.complete.obs") #matriz de correlaciom

rownames(M_hombres) <- c("Horas de trabajo no remunerado", 
                 "Bienestar objetivo", 
                 "Bienestar subjetivo",
                 "Satisfacción con la vida")
colnames(M_hombres) <- rownames(M) 

corrplot::corrplot(M_hombres,
                   method = "color",
                   tl.cex = 0.6,
                   mar = c(0, 0, 0, 0),
                   addCoef.col = "black",
                   number.cex = 0.6,
                   cl.cex = 0.6,
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#a118f3", "white",  "#ff0cae"))(12),
                   bg = "white",
                   na.label = "-")

#listwise

M_hombres2 <- cor(selected_vars_hom, use = "complete.obs") #matriz de correlaciom

sjPlot::tab_corr(selected_vars_hom, 
                 triangle = "lower")

rownames(M_hombres2) <- c("Horas de trabajo no remunerado", 
                         "Bienestar objetivo", 
                         "Bienestar subjetivo",
                         "Satisfacción con la vida")
colnames(M_hombres2) <- rownames(M) 

corrplot::corrplot(M_hombres2,
                   method = "color",
                   tl.cex = 0.6,
                   mar = c(0, 0, 0, 0),
                   addCoef.col = "black",
                   number.cex = 0.6,
                   cl.cex = 0.6,
                   type = "upper",
                   tl.col = "black",
                   col = colorRampPalette(c("#a118f3", "white",  "#ff0cae"))(12),
                   bg = "white",
                   na.label = "-")

colSums(is.na(mujeres)) 

colSums(is.na(hombres)) 


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
    bs1, bs2, bs3, bs12, bs13, bs15, bs16 #bienestar subjetivo
  )

## Muestra >=65 años------------------------------
data <- data %>%
  filter(edad >= 60)

names(data)

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

data$bs12 <- as.numeric(data$bs12)
data$bs13 <- as.numeric(data$bs13)
data$bs15 <- as.numeric(data$bs15)
data$bs16 <- as.numeric(data$bs16)

# trabajo no remunerado total--------------------------------

data$t_nr <- data$t_tdnr_dt + data$t_tcnr_dt + data$t_tvaoh_dt

describe(data$t_nr)

#tiempo dedicado a bienestar objetivo------------------------

data$cpaf <- data$t_cpaf_dt + data$t_vsyomcm_dt

#medición de bienestar subjetivo-------------------------------
## 2 personas por hogar o más

data$ebs2 <- rowMeans(data[c("bs1", "bs2")])

## 3 personas por hogar o más

data$ebs3 <- rowMeans(data[c("bs1", "bs2", "bs3")])

## escala sumativa bienestar subjetivo de cuidador(a)

data$escala_bsc <- rowMeans(data[ c("bs12", "bs13", "bs15", "bs16")], 
                            na.rm = TRUE)
summary(data$escala_bsc)

# bivariados -----------------------------------------------

# matriz de la muestra total 

cor.test(data$escala_bsc, data$t_nr, method = "spearman", use = "complete.obs")

sjPlot::plot_scatter(data = data, # diagrama de dispersión entre carga de trabajo y bienestar subjetivo
                     x = t_nr,
                     y = escala_bsc)

sjPlot::plot_scatter(data = data, # diagrama de dispersión entre carga de trabajo y bienestar objetivo
                     x = t_nr,
                     y = cpaf)

selected_vars <- data %>%  
  select(t_nr, cpaf, escala_bsc)

M <- cor(selected_vars, use = "complete.obs") #matriz de correlaciom

sjPlot::tab_corr(selected_vars, 
                 triangle = "lower")

rownames(M) <- c("Horas de trabajo no remunerado", 
                 "Bienestar objetivo", 
                 "Bienestar subjetivo")
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

# filtrar por mujeres

mujeres <- data %>% filter(sexo == 2)

cor.test(mujeres$escala_bsc, mujeres$t_nr, method = "spearman", use = "complete.obs")

sjPlot::plot_scatter(data = mujeres, # diagrama de dispersión
                     x = t_nr,
                     y = escala_bsc)

sjPlot::plot_scatter(data = mujeres, # diagrama de dispersión
                     x = t_nr,
                     y = cpaf)

selected_vars_muj <- mujeres %>%  
  select(t_nr, cpaf, escala_bsc)

M_mujeres <- cor(selected_vars_muj, use = "complete.obs") #matriz de correlaciom

sjPlot::tab_corr(selected_vars_muj, 
                 triangle = "lower")

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

# filtrar por hombres

hombres <- data %>% filter(sexo == 1)

cor.test(hombres$escala_bsc, hombres$t_nr, method = "spearman", use = "complete.obs")

sjPlot::plot_scatter(data = hombres, # diagrama de dispersión
                     x = t_nr,
                     y = escala_bsc)

sjPlot::plot_scatter(data = hombres, # diagrama de dispersión
                     x = t_nr,
                     y = cpaf)

selected_vars_hom <- hombres %>%  
  select(t_nr, cpaf, escala_bsc)

M_hombres <- cor(selected_vars_hom, use = "complete.obs") #matriz de correlaciom

sjPlot::tab_corr(selected_vars_hom, 
                 triangle = "lower")

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
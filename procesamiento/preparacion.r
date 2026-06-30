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
enut2023 <- readRDS("input/data-orig/250117-ii-enut-bdd-r.RDS")
# 1. FIltrar y seleccionar variables -------------------------------------------
## a Selección de variables de interés
data <- enut2023 %>%
 select(
 edad,
 sexo,
 t_tcnr_dt, #trabajo cuidados no remunerado
 t_tdnr_dt, #trabajo doméstico no remunerado para el propio hogar
 t_tvaoh_dt, #trabajo voluntario y ayudas a otros hogares
 t_cpaf_dt, t_vsyomcm_dt, # bienestar objetivo
 bs1, #satisfaccion con la vida
 bs12, bs13, bs15, bs16, bs17, bs18, bs19 #bienestar subjetivo
 )
## Muestra >=60 años
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
mutate(bs15= case_when(
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
## escala sumativa bienestar subjetivo -----
data$escala_bsc <- rowMeans(data[ c("bs12", "bs13", "bs15", "bs16", "bs17", "bs18", "bs19")],
 na.rm = TRUE)
summary(data$escala_bsc)

saveRDS(data, file = "input/data-proc.rds")
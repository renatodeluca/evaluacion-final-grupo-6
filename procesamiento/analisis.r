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


data <- readRDS("output/data_enut_limpia.rds")

selected_vars <- data %>%  
  select(t_nr, cpaf, escala_bsc)
M <- cor(selected_vars, use = "complete.obs") #matriz de correlaciom

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

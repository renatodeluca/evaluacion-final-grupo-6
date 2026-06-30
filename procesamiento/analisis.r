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
data <- readRDS("input/data-proc/data-proc.rds")
# graficos univariados ----- ---------------------------------------------
# Versión ultra simple con base R (sin paquetes adicionales)
cat("\nESTADÍSTICOS DESCRIPTIVOS\n\n")
for(var in c("t_nr", "cpaf", "escala_bsc", "bs1")) {
 x <- na.omit(data[[var]])

 cat("\n", switch(var,
 "t_nr" = "Horas trabajo no remunerado",
 "cpaf" = "Bienestar objetivo",
 "escala_bsc" = "Bienestar subjetivo",
 "bs1" = "Satisfacción con la vida"), ":\n", sep = "")

 cat("- Mínimo:", min(x), "\n")
 cat("- Máximo:", max(x), "\n")
 cat("- Media:", mean(x), "\n")
 cat("- Mediana:", median(x), "\n")
 cat("- Asimetría:", sum((x - mean(x))^3)/length(x)/(sd(x)^3), "\n")
 cat("- N observaciones:", length(x), "\n")
}
#graficos
data <- data %>%
 mutate(t_nr_horas = t_nr / 60)
ggplot(data %>% group_by(sexo) %>% summarise(promedio_tnr = mean(t_nr_horas, na.rm =
TRUE)),
 aes(x = factor(sexo), y = promedio_tnr, fill = factor(sexo))) +
 geom_col() +
 scale_fill_manual(values = c("#8efbff", "#3af8ff"),
 labels = c("Hombres", "Mujeres")) +
 labs(
 title = "Promedio de trabajo no remunerado por sexo (en horas)",
 x = "Sexo",
 y = "Horas diarias"
 ) +
 theme_minimal()
# 2. Promedio de bienestar objetivo (cpaf) por sexo en horas ----------------------------
data <- data %>%
 mutate(cpaf_horas = cpaf / 60)
ggplot(data %>% group_by(sexo) %>% summarise(promedio_cpaf = mean(cpaf_horas, na.rm =
TRUE)),
 aes(x = factor(sexo), y = promedio_cpaf, fill = factor(sexo))) +
 geom_col() +
 scale_fill_manual(values = c("#00bfc4", "#f8766d"),
 labels = c("Hombres", "Mujeres")) +
 labs(
 title = "Promedio de horas dedicadas a bienestar objetivo por sexo",
 x = "Sexo",
 y = "Horas diarias"
 ) +
 theme_minimal()
# 3. Promedio de bienestar subjetivo (escala_bsc) por sexo --------------------------------
ggplot(data %>% group_by(sexo) %>% summarise(promedio_bsc = mean(escala_bsc, na.rm =
TRUE)),
 aes(x = factor(sexo), y = promedio_bsc, fill = factor(sexo))) +
 geom_col() +
 scale_fill_manual(values = c("#00bfc4", "#f8766d"),
 labels = c("Hombres", "Mujeres")) +
 labs(
 title = "Promedio de bienestar subjetivo del cuidador/a por sexo",
 x = "Sexo",
 y = "Puntaje promedio (escala 1-5)"
 ) +
 theme_minimal()
# correlaciones muestra completa -----------------------------------------------
cor.test(data$cpaf, data$t_nr, method = "pearson", use = "complete.obs")
cor.test(data$bs1, data$t_nr, method = "spearman", use = "complete.obs")
library(sjPlot)
# dataframe con resultados
tabla_resultados <- data.frame(
 Variables = c("Bienestar objetivo ~ Horas de trabajo no remunerado", "Satisfacción con la vida
~ Horas de trabajo no remunerado"),
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
# ver tabla
tab_df(tabla_resultados,
 title = "Resultados de Correlación",
 show.rownames = FALSE,
 alternate.rows = TRUE)
# filtrar por cuidadores ----
cuidadores <- data %>%
 filter(!is.na(escala_bsc))
library(broom)
library(knitr)
resultado <- cor.test(cuidadores$cpaf, cuidadores$t_nr, method = "pearson", use =
"complete.obs")
tidy(resultado) %>%
 kable(format = "html", digits = 3,
 caption = "Resultados de Correlación") %>%
 kable_styling(bootstrap_options = "striped", full_width = FALSE)

cor.test(cuidadores$bs1, cuidadores$t_nr, method = "spearman", use = "complete.obs")
cor.test(cuidadores$escala_bsc, cuidadores$t_nr, method = "spearman", use = "complete.obs")
selected_vars <- cuidadores %>%
 select(t_nr, escala_bsc, bs1)
M <- cor(selected_vars, method = "spearman", use = "complete.obs") #matriz de correlaciom
sjPlot::tab_corr(selected_vars,
 triangle = "lower")
rownames(M) <- c("Horas de trabajo no remunerado",
 "Bienestar subjetivo",
 "Satisfacción con la vida")
colnames(M) <- rownames(M)
corrplot::corrplot(M,
 method = "color",
 tl.cex = 0.7,
 mar = c(0, 0, 0, 0),
 addCoef.col = "black",
 number.cex = 0.6,
 cl.cex = 0.6,
 type = "upper",
 tl.col = "black",
 col = colorRampPalette(c("#a118f3", "white", "#ff0cae"))(12),
 bg = "white",
 na.label = "-")
# filtrar por mujeres cuidadoras----
mujeres <- cuidadores %>% filter(sexo == 2)
resultado2 <- cor.test(mujeres$cpaf, mujeres$t_nr, method = "pearson", use = "complete.obs")
cor.test(mujeres$cpaf, mujeres$t_nr, method = "pearson", use = "complete.obs")
tidy(resultado2) %>%
 kable(format = "html", digits = 3,
 caption = "Resultados de Correlación") %>%
 kable_styling(bootstrap_options = "striped", full_width = FALSE)
cor.test(mujeres$bs1, mujeres$t_nr, method = "spearman", use = "complete.obs")
cor.test(mujeres$escala_bsc, mujeres$t_nr, method = "spearman", use = "complete.obs")
selected_vars2 <- mujeres %>%
 select(t_nr, escala_bsc, bs1)
M2 <- cor(selected_vars2, method = "spearman", use = "complete.obs") #matriz de correlaciom
rownames(M2) <- c("Horas de trabajo no remunerado",
 "Bienestar subjetivo",
 "Satisfacción con la vida")
colnames(M2) <- rownames(M2)


corrplot::corrplot(M2,
 method = "color",
 tl.cex = 0.7,
 mar = c(0, 0, 0, 0),
 addCoef.col = "black",
 number.cex = 0.6,
 cl.cex = 0.6,
 type = "upper",
 tl.col = "black",
 col = colorRampPalette(c("#a118f3", "white", "#ff0cae"))(12),
 bg = "white",
 na.label = "-")




 
# filtrar por hombres cuidadores----
hombres <- cuidadores %>% filter(sexo == 1)
resultado3 <- cor.test(hombres$cpaf, hombres$t_nr, method = "pearson", use = "complete.obs")
tidy(resultado3) %>%
 kable(format = "html", digits = 3,
 caption = "Resultados de Correlación") %>%
 kable_styling(bootstrap_options = "striped", full_width = FALSE)

cor.test(hombres$escala_bsc, hombres$t_nr, method = "spearman", use = "complete.obs")
cor.test(hombres$bs1, hombres$t_nr, method = "spearman", use = "complete.obs")
selected_vars3 <- hombres %>%
 select(t_nr, escala_bsc, bs1)
M3 <- cor(selected_vars3, method = "spearman", use = "complete.obs") #matriz de correlaciom
sjPlot::tab_corr(selected_vars3,
 triangle = "lower")
rownames(M3) <- c("Horas de trabajo no remunerado",
 "Bienestar subjetivo",
 "Satisfacción con la vida")
colnames(M3) <- rownames(M3)

corrplot::corrplot(M3,
 method = "color",
 tl.cex = 0.7,
 mar = c(0, 0, 0, 0),
 addCoef.col = "black",
 number.cex = 0.6,
 cl.cex = 0.6,
 type = "upper",
 tl.col = "black",
 col = colorRampPalette(c("#a118f3", "white", "#ff0cae"))(12),
 bg = "white",
 na.label = "-")







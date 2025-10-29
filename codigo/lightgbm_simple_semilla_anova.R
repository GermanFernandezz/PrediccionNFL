library(broom)
library(knitr)
library(dplyr)
library(tidyr)
library(car)
library(FSA)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]
modelos <- c("modelo_1", "modelo_2", "modelo_3", "modelo_4")
metricas <- c("exactitud", "precision", "precision_secundaria" )
optimizaciones <- c("grid", "bayesiana", "defecto")

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple_semillas")
for (equipo in equipos) {
  archivo <- paste0("resultado_lightgbm_simple_semilla_",equipo, ".csv")
  resultados <- read.csv(archivo)
  for (n in seq_along(metricas)) {
    if (exists("anovas")) rm(anovas)
    if (exists("resultados_tukey")) rm(resultados_tukey)
    data_list <- list()
    for (i in seq_along(optimizaciones)) {
      for (j in seq_along(modelos)) {
        archivo <- paste0("resultado_lightgbm_simple_", equipo,"_", modelos[j],"_",optimizaciones[i], ".csv")
        df <- read.csv(archivo)
        df$learning_rate <- as.numeric(df$learning_rate)
        df$feature_fraction <- as.numeric(df$feature_fraction)
        df$num_leaves <- as.numeric(df$num_leaves)
        df$min_data_in_leaf <- as.numeric(df$min_data_in_leaf)
        df$max_depth <- as.numeric(df$max_depth)
        data_list <- append(data_list, list(df))
        
      }
    }
    # unir todos los CSVs de manera segura
    data <- bind_rows(data_list)
    
    data$modelaje <- paste0(data$modelo,"_", data$optimizacion)
    metrica_anova <- aov(as.formula(paste0(metricas[n], " ~ modelaje")), data = data)
    resumen <- summary(metrica_anova)
    anovas <- data.frame("equipo" = equipo,
                         "p-value" = resumen[[1]]["Pr(>F)"][1]$`Pr(>F)`[1],
                         "shapiro" = shapiro.test(residuals(metrica_anova))$p.value,
                         "levene" = leveneTest(as.formula(paste0(metricas[n], " ~ modelaje")), data = data)$`Pr(>F)`[1],
                         "bartlett" = bartlett.test(as.formula(paste0(metricas[n], " ~ modelaje")), data = data)$p.value)
    tukey_result_df <- as.data.frame(TukeyHSD(metrica_anova)$modelaje)
    tukey_result_df$comparacion <- row.names(tukey_result_df)
    tukey_result_df$equipo <- rep(equipo,choose(length(modelos)*length(optimizaciones),2))
    if(!exists("resultados_tukey")) {
      resultados_tukey <- tukey_result_df
    } else {resultados_tukey <- rbind(resultados_tukey, tukey_result_df)
    
    }
    fila_max_metrica <- resultados[which.max(resultados[[ metricas[n] ]]), ]
    modelaje_ref <- paste0(fila_max_metrica$modelo,"_", fila_max_metrica$optimizacion)
    tukey_no_dif <- subset(tukey_result_df, grepl(modelaje_ref, comparacion) & `p adj` >= 0.05)
    
    write.csv(anovas, paste0("resultados_lightgbm_simple_",equipo,"_anova_", metricas[n], ".csv"), row.names = FALSE)
    write.csv(resultados_tukey, paste0("resultados_lightgbm_simple_",equipo,"_tukey_", metricas[n], ".csv"), row.names = FALSE)
    write.csv(tukey_no_dif,paste0("resultados_lightgbm_simple_",equipo,"_tukey_no_dif_", metricas[n], ".csv"), row.names = FALSE)
  
    
    }
  
}

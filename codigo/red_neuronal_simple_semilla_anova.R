library(broom)
library(knitr)
library(dplyr)
library(tidyr)
library(car)
library(FSA)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]
equipos <- "KC"
modelos <- c("modelo_1", "modelo_2", "modelo_3", "modelo_4")
metricas <- c("exactitud", "precision", "precision_secundaria" )
modelados <- c("no_profunda", "profunda")

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
for (equipo in equipos) {
  archivo <- paste0("resultado_red_simple_semillas_",equipo, ".csv")
  resultados <- read.csv(archivo)
  for (n in seq_along(metricas)) {
    if (exists("kruskales")) rm(anovas)
    if (exists("resultados_dunn")) rm(resultados_tukey)
    data_list <- list()
    for (i in seq_along(modelados)) {
      for (j in seq_along(modelos)) {
        archivo <- paste0("resultados_red_simple_semillas_", equipo,"_", modelados[i],"_",modelos[j], ".csv")
        df <- read.csv(archivo)
        df$dropout1 <- as.numeric(df$gota1)
        df$dropout2 <- as.numeric(df$gota2)
        df$dropout3 <- as.numeric(df$gota3)
        df$learning_rate <- as.numeric(df$lerner)
        df$epoca <- as.numeric(df$epocas)
        data_list <- append(data_list, list(df))
        
      }
    }
    # unir todos los CSVs de manera segura
    data <- bind_rows(data_list)
    
    data$modelaje <- paste0(data$modelado,"_", data$modelo)
    metrica_anova <- aov(as.formula(paste0(metricas[n], " ~ modelaje")), data = data)
    resumen <- summary(metrica_anova)
    anovas <- data.frame("equipo" = equipo,
                         "p-value" = resumen[[1]]["Pr(>F)"][1]$`Pr(>F)`[1],
                         "shapiro" = shapiro.test(residuals(metrica_anova))$p.value,
                         "levene" = leveneTest(as.formula(paste0(metricas[n], " ~ modelaje")), data = data)$`Pr(>F)`[1],
                         "bartlett" = bartlett.test(as.formula(paste0(metricas[n], " ~ modelaje")), data = data)$p.value)
    tukey_result_df <- as.data.frame(TukeyHSD(metrica_anova)$modelaje)
    tukey_result_df$comparacion <- row.names(tukey_result_df)
    tukey_result_df$equipo <- rep(equipo,choose(length(modelos)*length(modelados),2))
    if(!exists("resultados_tukey")) {
      resultados_tukey <- tukey_result_df
    } else {resultados_tukey <- rbind(resultados_tukey, tukey_result_df)
    
    }
    fila_max_metrica <- resultados[which.max(resultados[[ metricas[n] ]]), ]
    modelaje_ref <- paste0(fila_max_metrica$modelado,"_", fila_max_metrica$modelo)
    tukey_no_dif <- subset(tukey_result_df, grepl(modelaje_ref, comparacion) & `p adj` >= 0.05)
    
    write.csv(anovas, paste0("resultados_red_simple_semillas_",equipo,"_anova_", metricas[n], ".csv"), row.names = FALSE)
    write.csv(resultados_tukey, paste0("resultados_red_simple_semillas_",equipo,"_tukey_", metricas[n], ".csv"), row.names = FALSE)
    write.csv(tukey_no_dif,paste0("resultados_red_simple_semillas_",equipo,"_tukey_no_dif_", metricas[n], ".csv"), row.names = FALSE)
  
    
    }
  
}

library(broom)
library(knitr)
library(dplyr)
library(tidyr)
library(car)
library(FSA)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]
metricas <- c("exactitud", "precision", "precision_secundaria" )


resultados_anovas <- data.frame()



for (n in seq_along(metricas)) {
  resultado_anova <- data.frame()
  if (exists("resultados_tukey")) rm(resultados_tukey)
  
  
  
  for (e in seq_along(equipos)) {
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
    resultados_red <- read.csv("resultados_red_simple_semillas.csv")
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple_semillas")
    resultados_lightgbm <- read.csv("resultados_lightgbm_simple_semilla.csv")
    

    fila_max_metrica_red <- resultados_red[ resultados_red$equipo == equipos[e], ]
    fila_max_metrica_red <- fila_max_metrica_red[which.max(fila_max_metrica_red[[ metricas[n] ]]) , ]
    fila_max_metrica_lightgbm <- resultados_lightgbm[resultados_lightgbm$equipo == equipos[e], ]
    fila_max_metrica_lightgbm <- fila_max_metrica_lightgbm[which.max(fila_max_metrica_lightgbm[[ metricas[n] ]]) , ]
    
    
    archivo_max_metrica_red <- paste0("resultados_red_simple_semillas_",
                                      equipos[e], "_",
                                      fila_max_metrica_red$modelado,"_",
                                      fila_max_metrica_red$modelo, ".csv")
    archivo_max_metrica_lightgbm <- paste0("resultado_lightgbm_simple_",
                                           equipos[e], "_",
                                           fila_max_metrica_lightgbm$modelo,"_",
                                           fila_max_metrica_lightgbm$optimizacion, ".csv")
    archivo_metrica_azar <- paste0("resultado_azar_semilla_",equipos[e], ".csv")
    
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
    resultados_red_equipo <- read.csv(archivo_max_metrica_red)
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple_semillas")
    resultados_lightgbm_equipo <- read.csv(archivo_max_metrica_lightgbm)
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/azar")
    resultados_azar_equipo <- read.csv(archivo_metrica_azar)
    
    
    metrica_red <- resultados_red_equipo[[metricas[n]]]
    metrica_lightgbm <- resultados_lightgbm_equipo[[metricas[n]]]
    metrica_azar <- resultados_azar_equipo[[metricas[n]]]
    
    # combino métricas
    valores <- c(metrica_red, metrica_lightgbm, metrica_azar)
    
    # creo un factor con etiquetas
    modelo <- factor(c(rep("red", length(metrica_red)),
                       rep("lightgbm", length(metrica_lightgbm)),
                       rep("azar", length(metrica_azar))))
    
    # data frame
    df <- data.frame(valor = valores, modelo = modelo)
    
    # ANOVA
    metrica_anova <- aov(valor ~ modelo, data = df)
    resumen <- summary(metrica_anova)
    
    resultado_anova <- data.frame("equipo" = equipos[e],
                                  "metrica" = metricas[n],
                                  "modelo_red" = fila_max_metrica_red$modelo,
                                  "modelado_red" = fila_max_metrica_red$modelado,
                                  "modelo_lightgbm" = fila_max_metrica_lightgbm$modelo,
                                  "optimizacion_lightgbm" = fila_max_metrica_lightgbm$optimizacion,
                                  "metrica_red" = fila_max_metrica_red[[metricas[n]]],
                                  "metrica_lightgbm" = fila_max_metrica_lightgbm[[metricas[n]]],
                                  "metrica_azar" = mean(metrica_azar),
                                  "p-value" = resumen[[1]]["Pr(>F)"][1]$`Pr(>F)`[1],
                                  "shapiro" = shapiro.test(residuals(metrica_anova))$p.value,
                                  "levene" = leveneTest(valor ~ modelo, data = df)$`Pr(>F)`[1],
                                  "bartlett" = bartlett.test(valor ~ modelo, data = df)$p.value
                                  )
    
    resultados_anovas <- rbind(resultados_anovas, resultado_anova)
    
    
    tukey_result_df <- as.data.frame(TukeyHSD(metrica_anova)$modelo)
    tukey_result_df$comparacion <- row.names(tukey_result_df)
    tukey_result_df$equipo <- rep(equipos[e],3)
    if(!exists("resultados_tukey")) {
      resultados_tukey <- tukey_result_df
    } else {resultados_tukey <- rbind(resultados_tukey, tukey_result_df)
    
    }
    
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones")
    write.csv(tukey_result_df, paste0("resultados_machine_learning_",equipo,"_tukey_", metricas[n], ".csv"), row.names = FALSE)
    
    
    
    
  }
  
  
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/")
  write.csv(resultados_tukey, paste0("resultados_machine_learning_tukey_", metricas[n], ".csv"), row.names = FALSE)
}

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/")
write.csv(resultados_anovas, "resultados_anovas_mejores_modelos.csv")

tukey_no_dif <- subset(resultados_tukey, `p adj` >= 0.05)
write.csv(tukey_no_dif, "resultados_tukey_no_diferenciales_mejores_modelos.csv")


    
    
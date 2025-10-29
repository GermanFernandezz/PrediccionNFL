#library(broom)
#library(knitr)
library(dplyr)
#library(tidyr)
#library(car)
#library(FSA)
library(e1071)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]
metricas <- c("exactitud", "precision", "precision_secundaria" )


setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
resultados_red <- read.csv("resultados_red_simple_semillas.csv")
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple_semillas")
resultados_lightgbm <- read.csv("resultados_lightgbm_simple_semilla.csv")


for (e in seq_along(equipos)) {
  for (n in seq_along(metricas)) {
  
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
  resultados_red_equipo$modelaje <- paste0(resultados_red_equipo$modelado,"_", 
                                           resultados_red_equipo$modelo)
  modelaje_red <- paste0(resultados_red_equipo$modelado,"_", 
                         resultados_red_equipo$modelo)[1]
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple_semillas")
  resultados_lightgbm_equipo <- read.csv(archivo_max_metrica_lightgbm)
  resultados_lightgbm_equipo$modelaje <- paste0(resultados_lightgbm_equipo$modelo,"_", 
                                                resultados_lightgbm_equipo$optimizacion)
  modelaje_lightgbm <- paste0(resultados_lightgbm_equipo$modelo,"_", 
                              resultados_lightgbm_equipo$optimizacion)[1]
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/azar")
  resultados_azar_equipo <- read.csv(archivo_metrica_azar)
  resultados_azar_equipo$modelaje <- paste0(resultados_azar_equipo$modelo)
  modelaje_azar <- "azar"
  
  combinaciones <- c(modelaje_red,modelaje_lightgbm,modelaje_azar)

  dif = resultados_red_equipo[[metricas[n]]] - resultados_lightgbm_equipo[[metricas[n]]]
  wilcoxon <- data.frame("equipos" = equipos[e],
                         "metrica" = metricas[n],
                         "comparacion_a" = modelaje_red,
                         "comparacion_b" = modelaje_lightgbm,
                         "comparacion" = paste0(modelaje_red,"_", modelaje_lightgbm),
                         "metrica_a" = mean(resultados_red_equipo[[metricas[n]]]),
                         "metrica_b" = mean(resultados_lightgbm_equipo[[metricas[n]]]),
                         "p_value" = wilcox.test(resultados_red_equipo[[metricas[n]]],
                                                 resultados_lightgbm_equipo[[metricas[n]]],
                                                 paired = TRUE, 
                                                 exact = FALSE)$p.value,
                         "skewness" = skewness(dif))
  if(!exists("resultados_wilcoxon")) {
    resultados_wilcoxon <- wilcoxon
  } else {resultados_wilcoxon <- rbind(resultados_wilcoxon, wilcoxon)
  
  }
  
  dif = resultados_red_equipo[[metricas[n]]] - resultados_azar_equipo[[metricas[n]]]
  wilcoxon <- data.frame("equipos" = equipos[e],
                         "metrica" = metricas[n],
                         "comparacion_a" = modelaje_red,
                         "comparacion_b" = modelaje_azar,
                         "comparacion" = paste0(modelaje_red,"_", modelaje_azar),
                         "metrica_a" = mean(resultados_red_equipo[[metricas[n]]]),
                         "metrica_b" = mean(resultados_azar_equipo[[metricas[n]]]),
                         "p_value" = wilcox.test(resultados_red_equipo[[metricas[n]]],
                                                 resultados_azar_equipo[[metricas[n]]],
                                                 paired = TRUE, 
                                                 exact = FALSE)$p.value,
                         "skewness" = skewness(dif))
  if(!exists("resultados_wilcoxon")) {
    resultados_wilcoxon <- wilcoxon
  } else {resultados_wilcoxon <- rbind(resultados_wilcoxon, wilcoxon)
  
  }
  
  dif = resultados_lightgbm_equipo[[metricas[n]]] - resultados_azar_equipo[[metricas[n]]]
  wilcoxon <- data.frame("equipos" = equipos[e],
                         "metrica" = metricas[n],
                         "comparacion_a" = modelaje_lightgbm,
                         "comparacion_b" = modelaje_azar,
                         "comparacion" = paste0(modelaje_lightgbm,"_", modelaje_azar),
                         "metrica_a" = mean(resultados_lightgbm_equipo[[metricas[n]]]),
                         "metrica_b" = mean(resultados_azar_equipo[[metricas[n]]]),
                         "p_value" = wilcox.test(resultados_lightgbm_equipo[[metricas[n]]],
                                                 resultados_azar_equipo[[metricas[n]]],
                                                 paired = TRUE, 
                                                 exact = FALSE)$p.value,
                         "skewness" = skewness(dif))
  if(!exists("resultados_wilcoxon")) {
    resultados_wilcoxon <- wilcoxon
  } else {resultados_wilcoxon <- rbind(resultados_wilcoxon, wilcoxon)
  
  }
  }
  }
      
 

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/")
write.csv(resultados_wilcoxon, "resultados_wilcoxon_mejores_modelos.csv")



    
    
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


resultados_kruskales <- data.frame()



for (n in seq_along(metricas)) {
  resultado_kruskal <- data.frame()
  if (exists("resultados_dunn")) rm(resultados_dunn)
  
  
  
  for (equipo in equipos) {
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
    resultados_red <- read.csv("resultados_red_simple_semillas.csv")
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple_semillas")
    resultados_lightgbm <- read.csv("resultados_lightgbm_simple_semilla.csv")
    
    
    fila_max_metrica_red <- resultados_red[ resultados_red$equipo == equipo, ]
    fila_max_metrica_red <- fila_max_metrica_red[which.max(fila_max_metrica_red[[ metricas[n] ]]) , ]
    fila_max_metrica_lightgbm <- resultados_lightgbm[resultados_lightgbm$equipo == equipo, ]
    fila_max_metrica_lightgbm <- fila_max_metrica_lightgbm[which.max(fila_max_metrica_lightgbm[[ metricas[n] ]]) , ]
  
    
    archivo_max_metrica_red <- paste0("resultados_red_simple_semillas_",
                                      equipo, "_",
                                      fila_max_metrica_red$modelado,"_",
                                      fila_max_metrica_red$modelo, ".csv")
    archivo_max_metrica_lightgbm <- paste0("resultado_lightgbm_simple_",
                                           equipo, "_",
                                           fila_max_metrica_lightgbm$modelo,"_",
                                           fila_max_metrica_lightgbm$optimizacion, ".csv")
    archivo_metrica_azar <- paste0("resultado_azar_semilla_",equipo, ".csv")
    
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
    
    
    resultado_kruskal <- data.frame("equipo" = equipo,
                                    "metrica" = metricas[n],
                                    "modelo_red" = fila_max_metrica_red$modelo,
                                    "modelado_red" = fila_max_metrica_red$modelado,
                                    "modelo_lightgbm" = fila_max_metrica_lightgbm$modelo,
                                    "optimizacion_lightgbm" = fila_max_metrica_lightgbm$optimizacion,
                                    "metrica_red" = fila_max_metrica_red[[metricas[n]]],
                                    "metrica_lightgbm" = fila_max_metrica_lightgbm[[metricas[n]]],
                                    "metrica_azar" = mean(metrica_azar),
                                    "p-value" = kruskal.test(valor ~ modelo, data = df)$p.value
    )
    
    resultados_kruskales <- rbind(resultados_kruskales, resultado_kruskal)
    
    dunn_bonferroni_result <- dunnTest(valor ~ modelo, 
                                       data = df, 
                                       method = "bonferroni")$res[,c(1,4)]
    dunn_holm_result <- dunnTest(valor ~ modelo, 
                                 data = df, 
                                 method = "holm")$res[,4]
    dunn_bh_result <- dunnTest(valor ~ modelo, 
                               data = df, 
                               method = "bh")$res[,4]
    
    dunn_result <- cbind(cbind(dunn_bonferroni_result, dunn_holm_result), dunn_bh_result)
    colnames(dunn_result)[c(2,3,4)] <- c("bonferroni", "holm", "bh")
    dunn_result$equipo <- rep(equipo,3)
    
    if(!exists("resultados_dunn")) {
      resultados_dunn <- dunn_result
    } else {resultados_dunn <- rbind(resultados_dunn, dunn_result)
    
    }
    
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones")
    write.csv(dunn_result, paste0("resultados_machine_learning_",equipo,"_dunn_", metricas[n], ".csv"), row.names = FALSE)
    
  }
  
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones")
  write.csv(resultados_dunn, paste0("resultados_machine_learning_dunn_", metricas[n], ".csv"), row.names = FALSE)
  
  
  }

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/")
write.csv(resultados_kruskales, "resultados_kruskales_mejores_modelos.csv")

tukey_no_dif <- subset(resultados_dunn, `bh` >= 0.05)
write.csv(tukey_no_dif, "resultados_tukey_no_diferenciales_mejores_modelos.csv")


    
    
resultados_dunn$bh
    
    
  }
}

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/")
write.csv(resultados_kruskales, "resultados_kruskal_mejores_modelos.csv")

    
    
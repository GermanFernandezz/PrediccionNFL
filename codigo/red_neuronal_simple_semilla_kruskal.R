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
modelados <- c("no_profunda", "profunda")

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
for (equipo in equipos) {
  archivo <- paste0("resultado_red_simple_semillas_",equipo, ".csv")
  resultados <- read.csv(archivo)
  for (n in seq_along(metricas)) {
    if (exists("kruskales")) rm(kruskales)
    if (exists("resultados_dunn")) rm(resultados_dunn)
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
    
    kruskales <- data.frame("equipo" = equipo,
                            "p-value" = kruskal.test(as.formula(paste0(metricas[n], " ~ modelaje")),data = data)$p.value)
                         
    
    dunn_bonferroni_result <- dunnTest(as.formula(paste0(metricas[n], " ~ modelaje")), 
                                       data = data, 
                                       method = "bonferroni")$res[,c(1,4)]
    dunn_holm_result <- dunnTest(as.formula(paste0(metricas[n], " ~ modelaje")), 
                                 data = data, 
                                 method = "holm")$res[,4]
    dunn_bh_result <- dunnTest(as.formula(paste0(metricas[n], " ~ modelaje")), 
                               data = data, 
                               method = "bh")$res[,4]
    
    dunn_result <- cbind(cbind(dunn_bonferroni_result, dunn_holm_result), dunn_bh_result)
    colnames(dunn_result)[c(2,3,4)] <- c("bonferroni", "holm", "bh")
    
    dunn_result$equipo <- rep(equipo,choose(length(modelos)*length(modelados),2))
    
    
    
    if(!exists("resultados_dunn")) {
      resultados_dunn <- dunn_result
    } else {resultados_dunn <- rbind(resultados_dunn, dunn_result)
    
    }
    fila_max_metrica <- resultados[which.max(resultados[[ metricas[n] ]]), ]
    modelaje_ref <- paste0(fila_max_metrica$modelado,"_", fila_max_metrica$modelo)
    dunn_no_dif <- subset(dunn_result, grepl(modelaje_ref, Comparison) & `bonferroni` >= 0.05)
    
    write.csv(kruskales, paste0("resultados_red_simple_",equipo,"_kruskal_", metricas[n], ".csv"), row.names = FALSE)
    write.csv(resultados_dunn, paste0("resultados_red_simple_",equipo,"_dunn_", metricas[n], ".csv"), row.names = FALSE)
    write.csv(dunn_no_dif,paste0("resultados_red_simple_",equipo,"_dunn_no_dif_", metricas[n], ".csv"), row.names = FALSE)
  
    
    }
  
}

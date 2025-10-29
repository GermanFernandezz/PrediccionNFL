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
    if (exists("kruskales")) rm(kruskales)
    if (exists("resultados_dunn")) rm(resultados_dunn)
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
    
    dunn_result$equipo <- rep(equipo,choose(length(modelos)*length(optimizaciones),2))
    
    
    
    if(!exists("resultados_dunn")) {
      resultados_dunn <- dunn_result
    } else {resultados_dunn <- rbind(resultados_dunn, dunn_result)
    
    }
    fila_max_metrica <- resultados[which.max(resultados[[ metricas[n] ]]), ]
    modelaje_ref <- paste0(fila_max_metrica$modelo,"_", fila_max_metrica$optimizacion)
    dunn_no_dif <- subset(dunn_result, grepl(modelaje_ref, Comparison) & `bonferroni` >= 0.05)
    
    write.csv(kruskales, paste0("resultados_lightgbm_simple_",equipo,"_kruskal_", metricas[n], ".csv"), row.names = FALSE)
    write.csv(resultados_dunn, paste0("resultados_lightgbm_simple_",equipo,"_dunn_", metricas[n], ".csv"), row.names = FALSE)
    write.csv(dunn_no_dif,paste0("resultados_lightgbm_simple_",equipo,"_dunn_no_dif_", metricas[n], ".csv"), row.names = FALSE)
    
    
  }
  
}


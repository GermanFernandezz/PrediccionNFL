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

modelos <- c("modelo_1", "modelo_2", "modelo_3", "modelo_4")
metricas <- c("exactitud", "precision", "precision_secundaria" )
modelados <- c("no_profunda", "profunda")


combinaciones <- expand.grid(modelado = modelados, modelo = modelos) %>%
  apply(1, function(x) paste(x[1], x[2], sep = "_"))
l <- as.numeric(length(combinaciones))

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
for (e in seq_along(equipos)) {
  archivo <- paste0("resultado_red_simple_semillas_",equipos[e], ".csv")
  resultados <- read.csv(archivo)
  
  data_list <- list()
  for (i in seq_along(modelados)) {
    for (j in seq_along(modelos)) {
      archivo <- paste0("resultados_red_simple_semillas_", equipos[e],"_", modelados[i],"_",modelos[j], ".csv")
      
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
  for (k in seq_along(combinaciones)) {
    if (k < l) {
    comparacion_a <- combinaciones[k]
    data_a <- subset(data, modelaje == comparacion_a)
    m = k+1
    for (q in m:l) {
      comparacion_b <- combinaciones[q]
      data_b <- subset(data, modelaje == comparacion_b)
      
      for (n in seq_along(metricas)) {
        dif <- data_a[[metricas[n]]] - data_b[[metricas[n]]]
        wilcoxon <- data.frame("equipo" = equipos[e],
                               "metrica" = metricas[n],
                               "comparacion_a" = comparacion_a,
                               "comparacion_b" = comparacion_b,
                               "comparacion" = paste0(comparacion_a,"_",comparacion_b),
                               "metrica_a" = mean(data_a[[metricas[n]]]),
                               "metrica_b" = mean(data_b[[metricas[n]]]),
                               "p_value" = wilcox.test(data_a[[metricas[n]]],
                                                       data_b[[metricas[n]]],
                                                       paired = TRUE, 
                                                       exact = FALSE)$p.value,
                               "skewness" = skewness(dif)
        )
        
        if(!exists("resultados_wilcoxon")) {
          resultados_wilcoxon <- wilcoxon
        } else {resultados_wilcoxon <- rbind(resultados_wilcoxon, wilcoxon)
        
        }
      
    }
    
  }
  
  
  
  
    
    
    
    }}
  
  fila_max_metrica <- resultados[which.max(resultados[[ metricas[n] ]]), ]
  modelaje_ref <- paste0(fila_max_metrica$modelo,"_", fila_max_metrica$optimizacion)
  wilcoxon_no_dif <- subset(resultados_wilcoxon, grepl(modelaje_ref, comparacion) & p_value > 0.05 & equipo == equipos[e])
  
  if(!exists("resultados_wilcoxon_no_dif")) {
    resultados_wilcoxon_no_dif <- wilcoxon_no_dif
  } else {resultados_wilcoxon_no_dif <- rbind(resultados_wilcoxon_no_dif, wilcoxon_no_dif)
  
  }
  
  
}


write.csv(resultados_wilcoxon, "resultados_wilcoxon.csv", row.names = FALSE)

write.csv(resultados_wilcoxon_no_dif, "resultados_wilcoxon_no_dif.csv", row.names = FALSE)  
    
    
   
   
  
    
    
    


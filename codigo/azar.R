library(lightgbm)

library(caret)

library(dplyr)

library(yardstick)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]

n_seeds <- 20
set.seed(28749658)
semillas <- sample(1:9999, n_seeds)

resultados <- data.frame()

for (equipo in equipos) {
  # Directorio donde están los datos
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
  # Archivo con los datos preprocesados listos para usar
  archivo_equipo <- paste0("plays_team_", equipo, ".csv")
  # Carga de datos
  data <- read.csv(archivo_equipo)
  etiquetas <- data$isDropback
  corte <- as.numeric(table(etiquetas)[2] / length(etiquetas)) # proporcion de dropback TRUE
  
  # data frame donde voy a cargar los resultados de cada equipo
  resultado_equipo <- data.frame()
  
  # Proporción de entrenamiento (80%)
  set.seed(28749658) # Para reproducibilidad
  #train_id <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
  train_id <- createDataPartition(etiquetas, p = 0.8, list = FALSE)
 
  label_train <- as.numeric(etiquetas[train_id])
  label_test <- as.numeric(etiquetas[-train_id])
  
  total_label <- length(label_test)
  num_true <- round(total_label * corte)
  num_false <- total_label - num_true 
  
  resultado_equipo_semilla <- data.frame()
  for (n in seq_along(semillas)){
    set.seed(semillas[n])
    clases_predichas <- sample(c(rep(TRUE, num_true), rep(FALSE, num_false)))
    clases_predichas <- as.numeric(clases_predichas)
    
    datita <- tibble(
      truth = factor(label_test),
      estimate = factor(clases_predichas, levels = levels(factor(label_test)))
    )
    
    matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test)
    )
    
    
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/azar")
    write.csv(matriz_confusion,
              paste0("matriz_confusion_azar_",equipo,"_", semillas[n], ".csv"),
              row.names = FALSE)
    
    resultado_equipo_semilla <- rbind(resultado_equipo_semilla,
                                      data.frame(equipo = equipo,
                                                 modelo = "azar",
                                                 semilla = semillas[n],
                                                 exactitud = mean(clases_predichas == label_test),
                                                 precision = yardstick::precision(datita, 
                                                                                  truth = truth, 
                                                                                  estimate = estimate, 
                                                                                  event_level = "second"
                                                 )$.estimate,
                                                 precision_secundaria = yardstick::precision(datita, 
                                                                                             truth = truth, 
                                                                                             estimate = estimate
                                                 )$.estimate,
                                                 promedio = (mean(clases_predichas == label_test) + 
                                                               yardstick::precision(datita, 
                                                                                    truth = truth, 
                                                                                    estimate = estimate, 
                                                                                    event_level = "second"
                                                               )$.estimate +
                                                               yardstick::precision(datita, 
                                                                                    truth = truth, 
                                                                                    estimate = estimate
                                                               )$.estimate)/3,
                                                 f1 = f_meas(
                                                   datita,
                                                   truth = truth,
                                                   estimate = estimate,
                                                   event_level = "second"
                                                 )$.estimate,
                                                 recall = yardstick::recall(
                                                   datita,
                                                   truth = truth,
                                                   estimate = estimate,
                                                   event_level = "second"
                                                 )$.estimate)
    )
    
    
  
  
  
  }
  
  resultado_equipo <-rbind(resultado_equipo,
                           data.frame(equipo = equipo,
                                      modelo = "azar",
                                      exactitud = mean(resultado_equipo_semilla$exactitud),
                                      precision = mean(resultado_equipo_semilla$precision),
                                      precision_secundaria = mean(resultado_equipo_semilla$precision_secundaria),
                                      promedio = mean(resultado_equipo_semilla$promedio),
                                      f1 = mean(resultado_equipo_semilla$f1),
                                      recall = mean(resultado_equipo_semilla$recall)))
  
  # guardo resultados del equipo
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/azar")
  write.csv(
    resultado_equipo_semilla,
    paste0("resultado_azar_semilla_",equipo,".csv"),
    row.names = FALSE
  )
  
  resultados <- rbind(resultados, resultado_equipo)
  
}

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/azar")   
write.csv(resultados,
          "resultados_azar_semilla.csv",
          row.names = FALSE) 





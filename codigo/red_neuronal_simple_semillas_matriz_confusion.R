#library(keras)
#library(dplyr)
library(caret)
#install.packages("tensorflow")
#library(tensorflow)
#install.packages("torch")
#library(torch)
#library(yardstick) 
#library(pROC)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]
#equipos <- c("PHI", "KC")
# Los modelos que definen las variables que se van a usar
modelos <- c("modelo_1", "modelo_2", "modelo_3", "modelo_4")

n_seeds <- 20
set.seed(28749658)
semillas <- sample(1:9999, n_seeds)


# nombres de las configuraciones de las redes
nombre_modelados <- c("no_profunda", 
                      "profunda")

# data frame para cargar los resultados de todos los equipos
resultados <- data.frame()

for (e in seq_along(equipos)) {
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
  # Archivo con los datos preprocesados listos para usar
  archivo_equipo <- paste0("plays_team_", equipos[e], ".csv")
  # Carga de datos
  data <- read.csv(archivo_equipo)
  etiquetas <- data$isDropback
  y <- data$isDropback
  corte <- as.numeric(table(etiquetas)[2] / length(etiquetas)) # proporcion de dropback TRUE
  set.seed(57522978)
  idx <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data)-)
  idx <- createDataPartition(y, p = 0.8, list = FALSE)
  y_test <- y[-idx]
  
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
  for (p in seq_along(nombre_modelados)) {
    for (m in seq_along(modelos)) {
      if (exists("predicciones")) rm(predicciones)
      for (s in seq_along(semillas)) {
        nombre <- paste0("probabilidades_red_simple_", 
                         equipos[e], "_",
                         nombre_modelados[p], "_",
                         modelos[m], "_",
                         semillas[s], ".csv")
        archivo <- read.csv(nombre)
        preds <- archivo$preds
        if (!exists("predicciones")) {
          predicciones <- preds
        } else {
          predicciones <- cbind(predicciones, preds)
        }
        
      }
      preds <- rowMeans(predicciones)
      
      orden <- order(preds, decreasing = TRUE)
      #asigno cuantos valores dropback deberían tener las predicciones
      n_positivos <- round(corte * length(preds))
      # Inicializar predicción binaria
      clases_predichas <- rep(0, length(preds)) #asigno 0 a todo el vector de clases predichas
      clases_predichas[orden[1:n_positivos]] <- 1 #asigno 1 a las posiciones 1:n_positivos del vector clases predichas0
      label_test <- as.numeric(y_test) #genero vector de vector real de entrenamiento
      
      matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test))
      
      nombre2 <- paste0("matriz_confusion_red_simple_", 
                        equipos[e], "_",
                        nombre_modelados[p], "_",
                        modelos[m], ".csv")
      
      write.csv(matriz_confusion, nombre2, row.names = FALSE)
      
      
    }
  }
}
  
  
  
  
  
  
  
  
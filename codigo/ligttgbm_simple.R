#install.packages("lightgbm")
library(lightgbm)
#install.packages("tidyverse")
library(tidyverse)
#install.packages("yardstick")  # Instalar el paquete (si no lo tienes)
library(yardstick)  # Cargar el paquete
#install.packages("caret")
library(caret)
#install.packages("pROC")
library(pROC)
library(dplyr)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]

# Directorio
modelos <- c("modelo_1", "modelo_2", "modelo_3", "modelo_4")

optimizaciones <- c("defecto", "grid", "bayesiana")

resultados <- data.frame()

for (equipo in equipos) {
  # Directorio donde están los datos
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
  # Archivo con los datos preprocesados listos para usar
  archivo_equipo <- paste0("plays_team_", equipo, ".csv")
  # Carga de datos
  data <- read.csv(archivo_equipo)
  etiquetas <- data$isDropback
  # Lista con los diferentes sets de datos según la variables que vamos a usar
  datos_modelos <- list(data[, c(2:7, 10:12, as.numeric(ncol(data) - 2):as.numeric(ncol(data) - 1))],# Modelo 1
                        data[, c(2:7, 10:as.numeric(ncol(data) - 1))],# Modelo 2
                        data[, c(2:as.numeric(ncol(data) - 1))],# Modelo 3
                        data[, c(2:12, as.numeric(ncol(data) - 2), as.numeric(ncol(data) - 1))] # Modelo 4
  )
  corte <- as.numeric(table(etiquetas)[2] / length(etiquetas)) # proporcion de dropback TRUE
  
  # data frame donde voy a cargar los resultados de cada equipo
  resultado_equipo <- data.frame()
  
  #Bucle para recorrer los modelos de las variables
  for (j in seq_along(modelos)) {
    # Preparo los datos 
    datos <- datos_modelos[[j]]
    # Proporción de entrenamiento (80%)
    set.seed(28749658) # Para reproducibilidad
    train_id <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))
    train_id <- createDataPartition(etiquetas, p = 0.8, list = FALSE)
    # Crear conjuntos
    data_train <- datos[train_id, ]
    data_test <- datos[-train_id, ]
    label_train <- as.numeric(etiquetas[train_id])
    label_test <- as.numeric(etiquetas[-train_id])
    # Convertir a dataset de LightGBM
    dtrain <- lgb.Dataset(data = as.matrix(data_train), label = label_train,
                          params = list(feature_pre_filter = FALSE))
    vtrain <- lgb.Dataset(data = as.matrix(data_test), label = label_test)
    
    #Bucle para recorrer las optimizaciones
    for (i in seq_along(optimizaciones)) {
      if (optimizaciones[i] == "defecto") {
        params_opt <- list(
          boosting = "gbdt",
          # puede ir  dart  , ni pruebe random_forest
          objective = "binary",
          metric = "auc",
          seed = 28231347
        )
        model <- lgb.train(
          params = params_opt,
          data = dtrain,
          nrounds = 100,
          #valids = list(test = vtrain),
          #early_stopping_rounds = 10,
          verbose = -1
        )
        importancia <- lgb.importance(model)
        preds <- predict(model, as.matrix(data_test), type = "response")
        orden_preds <- order(preds, decreasing = TRUE)
        n_positivos_preds <- round(corte * length(preds))
        clases_predichas <- rep(0, length(preds))
        clases_predichas[orden_preds[1:n_positivos_preds]] <- 1
        datita <- tibble(
          truth = factor(label_test),
          estimate = factor(clases_predichas, levels = levels(factor(label_test))),
          probabilidades = preds
        )
        probabilidades <- cbind(data_test, datita)
        matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test)
        )
        setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple")
        write.csv(importancia,
                  paste0("importancia_lightgbm_simple_",equipo,"_",modelos[j],"_", optimizaciones[i],".csv"),
                  row.names = FALSE)
        write.csv(matriz_confusion,
                  paste0("matriz_confusion_lightgbm_simple_",equipo,"_",modelos[j], "_", optimizaciones[i],".csv"),
                  row.names = FALSE)
        write.csv(probabilidades,
                  paste0("probabilidades_lightgbm_simple_",equipo,"_",modelos[j], "_", optimizaciones[i],".csv"),
                  row.names = FALSE)
        
        resultado_equipo <-rbind(resultado_equipo,
                                 data.frame(equipo = equipo,
                                            modelo = modelos[j],
                                            optimizacion = optimizaciones[i],
                                            learning_rate = "defecto",
                                            feature_fraction = "defecto",
                                            num_leaves = "defecto",
                                            min_data_in_leaf = "defecto",
                                            max_depth = "defecto",
                                            auc = "NA",
                                            promedio = "NA",
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
        
        
        
        
      } else if (optimizaciones[i] == "grid") {
        setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/grid")
        archivo_param <- paste0("log_grid_lightgbm_", equipo, "_", modelos[j], ".csv")
        data_param <- read.csv(archivo_param)
        # Encontrar la fila con el mayor valor en 'promedio'
        fila_max_promedio <- data_param[which.max(data_param$promedio), ]
        params_opt <- list(
          boosting = "gbdt",
          # puede ir  dart  , ni pruebe random_forest
          objective = "binary",
          metric = "auc",
          seed = 28231347,
          learning_rate = fila_max_promedio$lerners,
          num_leaves = fila_max_promedio$hojas,
          feature_fraction = fila_max_promedio$fracciones_variables,
          min_data_in_leaf = fila_max_promedio$min_data,
          max_depth = fila_max_promedio$profundidad
        )
        model <- lgb.train(
          params = params_opt,
          data = dtrain,
          nrounds = 100,
          #valids = list(test = vtrain),
          #early_stopping_rounds = 10,
          verbose = -1
        )
        preds <- predict(model, as.matrix(data_test), type = "response")
        orden_preds <- order(preds, decreasing = TRUE)
        n_positivos_preds <- round(corte * length(preds))
        clases_predichas <- rep(0, length(preds))
        clases_predichas[orden_preds[1:n_positivos_preds]] <- 1
        datita <- tibble(
          truth = factor(label_test),
          estimate = factor(clases_predichas, levels = levels(factor(label_test))),
          probabilidades = preds
        )
        probabilidades <- cbind(data_test, datita)
        matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test)
        )
        setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple")
        write.csv(importancia,
                  paste0("importancia_lightgbm_simple_",equipo,"_",modelos[j],"_", optimizaciones[i],".csv"),
                  row.names = FALSE)
        write.csv( matriz_confusion,
                   paste0("matriz_confusion_lightgbm_simple_",equipo,"_",modelos[j], "_", optimizaciones[i],".csv"),
                   row.names = FALSE)
        write.csv(probabilidades,
                  paste0("probabilidades_lightgbm_simple_",equipo,"_",modelos[j], "_", optimizaciones[i],".csv"),
                  row.names = FALSE)
        
        resultado_equipo <-rbind(resultado_equipo,
                                 data.frame(equipo = equipo,
                                            modelo = modelos[j],
                                            optimizacion = optimizaciones[i],
                                            learning_rate = fila_max_promedio$lerners,
                                            feature_fraction = fila_max_promedio$fracciones_variables,
                                            num_leaves = fila_max_promedio$hojas,
                                            min_data_in_leaf = fila_max_promedio$min_data,
                                            max_depth = fila_max_promedio$profundidad,
                                            auc = "NA",
                                            promedio = fila_max_promedio$promedio,
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
        
        
        
      } else { setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/bayesiana")
        archivo_param <- paste0("log_grid_lightgbm_", equipo, "_", modelos[j], ".csv")
        data_param <- read.csv(archivo_param)
        # Encontrar la fila con el mayor valor en 'promedio'
        fila_max_auc <- data_param[which.max(data_param$auc), ]
        #parametros optimizables
        params_opt <- list(
          boosting = "gbdt",
          # puede ir  dart  , ni pruebe random_forest
          objective = "binary",
          metric = "auc",
          learning_rate = fila_max_auc$learning_rate,
          num_leaves = fila_max_auc$num_leaves,
          feature_fraction = fila_max_auc$feature_fraction,
          min_data_in_leaf = fila_max_auc$min_data_in_leaf,
          max_depth = fila_max_auc$max_depth
        )
        model <- lgb.train(
          params = params_opt,
          data = dtrain,
          nrounds = 100,
          #valids = list(test = vtrain),
          #early_stopping_rounds = 10,
          verbose = -1
        )
        preds <- predict(model, as.matrix(data_test), type = "response")
        orden_preds <- order(preds, decreasing = TRUE)
        n_positivos_preds <- round(corte * length(preds))
        clases_predichas <- rep(0, length(preds))
        clases_predichas[orden_preds[1:n_positivos_preds]] <- 1
        datita <- tibble(
          truth = factor(label_test),
          estimate = factor(clases_predichas, levels = levels(factor(label_test))),
          probabilidades = preds
        )
        matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test)
        )
        probabilidades <- cbind(data_test, datita)
        setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple")
        write.csv(importancia,
                  paste0("importancia_lightgbm_simple_",equipo,"_",modelos[j],"_", optimizaciones[i],".csv"),
                  row.names = FALSE)
        write.csv(matriz_confusion,
                  paste0("matriz_confusion_lightgbm_simple_",equipo,"_",modelos[j], "_", optimizaciones[i],".csv"),
                  row.names = FALSE)
        write.csv(probabilidades,
                  paste0("probabilidades_lightgbm_simple_",equipo,"_",modelos[j], "_", optimizaciones[i],".csv"),
                  row.names = FALSE)
        
        resultado_equipo <-rbind(resultado_equipo,
                                 data.frame(equipo = equipo,
                                            modelo = modelos[j],
                                            optimizacion = optimizaciones[i],
                                            learning_rate = fila_max_auc$learning_rate,
                                            feature_fraction = fila_max_auc$feature_fraction,
                                            num_leaves = fila_max_auc$num_leaves,
                                            min_data_in_leaf = fila_max_auc$min_data_in_leaf,
                                            max_depth = fila_max_auc$max_depth,
                                            auc = fila_max_auc$auc,
                                            promedio = "NA",
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
      
      
    }
  
  }
  # guardo resultados del equipo
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple")
  write.csv(
    resultado_equipo,
    paste0("resultado_lightgbm_simple_",equipo,".csv"),
    row.names = FALSE
  )
  
  resultados <- rbind(resultados, resultado_equipo)
}
 
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple")   
write.csv(resultados,
          "resultados_lightgbm_simple.csv",
          row.names = FALSE) 











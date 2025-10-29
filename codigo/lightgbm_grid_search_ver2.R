library(rsample)
library(yardstick)
library(lightgbm)
library(tidyverse)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")

equipos <- read.csv("equipos.csv", header = TRUE)[[1]]
modelos <- c("modelo_1", "modelo_2", "modelo_3", "modelo_4")
bo_iteraciones <- 150 # iteraciones de la Optimizacion Bayesiana
# Estableces HP del grid search
hojas <- c(15, 30, 35, 50)  # Cantidad de hojas
min_data <- c(5, 10, 15, 20, 25, 30, 35, 40)
lerners <- c(0.01,0.03,0.05, 0.1, 0.2)
profundidades <- c(3,5,7,9)
fracciones_variables <- c(0.5, 0.6,0.7,0.8,0.9)
parametros <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "auc"
)


for (equipo in equipos) {
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
  archivo_equipos <- paste0("plays_team_", equipo, ".csv")
  data <- read.csv(archivo_equipos)
  etiquetas <- data$isDropback
  corte <- as.numeric(table(etiquetas)[2]/length(etiquetas))
  set.seed(57522978)   # Para reproducibilidad
  # Proporción de entrenamiento (80%)
  train_id <- sample(seq_len(nrow(data)), size = 0.8 * nrow(data))
  # Crear conjuntos
  data <- data[train_id, ]
  etiquetas <- etiquetas[train_id]
  datos_modelos <- list(data[,c(2:7,10:12,as.numeric(ncol(data)-2):as.numeric(ncol(data)-1))], # Modelo 1
                        data[,c(2:7,10:as.numeric(ncol(data)-1))], # Modelo 2
                        data[,c(2:as.numeric(ncol(data)-1))], # Modelo 3
                        data[,c(2:12,as.numeric(ncol(data)-2),as.numeric(ncol(data)-1))] # Modelo 4
  )
  for (modelo in 1:as.numeric(length(modelos))) {
    nombre <- paste0("log_grid_lightgbm_", equipo,"_", modelos[modelo], ".csv")
    datos <- datos_modelos[[modelo]]
    # Crear folds
    set.seed(123)
    cv_folds <- vfold_cv(data = data.frame(datos, etiquetas), v = 3)
    resultados <- data.frame(
      hoja = integer(), 
      min_dat = integer(), 
      fraccion_variables = numeric(), 
      lerner <- numeric(),
      profundida <- integer(),
      lambda <- integer(),
      exactitud = numeric(), 
      precision = numeric(),
      precision_sec = numeric(),
      promedio = numeric()
    )
    for (h in hojas){
      for (m in min_data){
        for (b in fracciones_variables){
          for (l in lerners){
            for (p in profundidades){
              precisiones <- c()
              precisiones_sec <- c()
              exactitudes <- c()
              opt_params <- list(
                learning_rate = l,
                num_leaves = h,
                min_data_in_leaf = m,
                max_depth = p,
                feature_fraction = b
              )
              params <- c(parametros, opt_params)
              for (fold in cv_folds$splits) {
                train_data <- analysis(fold)
                val_data <- assessment(fold)
                
                
                # Crear conjuntos
                data_train <- train_data[, 1:ncol(datos)]
                data_test <- val_data[, 1:ncol(datos)]
                
                label_train <- as.numeric(train_data$etiquetas)
                label_test <- as.numeric(val_data$etiquetas)
                
                # Convertir a dataset de LightGBM
                dtrain <- lgb.Dataset(data = as.matrix(data_train), label = label_train)
                
                modelo <- lgb.train(
                  params = params,
                  data = dtrain,
                  #valids = list(valid = dvalid),
                  #early_stopping_rounds = 10,
                  nrounds = 100   # Número de iteraciones (árboles)
                )
                # Hacer predicciones con el modelo entrenado
                predicciones <- predict(modelo, as.matrix(data_test))
                
                # Paso 2: Ordenar y asignar clase 1 al 70% con mayor probabilidad
                orden <- order(predicciones, decreasing = TRUE)
                n_positivos <- round(corte * length(predicciones))
                
                # Inicializar predicción binaria
                clases_predichas <- rep(0, length(predicciones))
                clases_predichas[orden[1:n_positivos]] <- 1
                
                exactitud <- mean(clases_predichas == label_test)
                
                tabla <- tibble(
                  truth = factor(label_test),
                  estimate = factor(clases_predichas, levels = levels(factor(label_test)))
                )
                
                # Calcular la precisión
                precision_value <- yardstick::precision(tabla, truth = truth, estimate = estimate, event_level = "second")
                precision_value_secundario <- yardstick::precision(tabla, truth = truth, estimate = estimate)
                
                precisiones <- c(precisiones, precision_value$.estimate)
                precisiones_sec <- c(precisiones_sec, precision_value_secundario$.estimate)
                exactitudes <- c(exactitudes, exactitud)
              }
              resultados <- rbind(resultados, data.frame(
                hojas = h, 
                min_data = m, 
                fracciones_variables = b, 
                lerners = l,
                profundidad = p,
                exactitud = mean(exactitudes),
                precision = mean(precisiones),
                precision_secundaria = mean(precisiones_sec),
                promedio = (mean(exactitudes)+mean(precisiones)+mean(precisiones_sec))/3
              ))
              }
            }
          }
        }
    }
    setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/grid")
    write.csv(resultados, nombre)
    }
  }












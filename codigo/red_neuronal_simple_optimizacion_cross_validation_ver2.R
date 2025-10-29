library(keras)
library(dplyr)
library(caret)
#install.packages("tensorflow")
library(tensorflow)
#install.packages("torch")
library(torch)
library(yardstick) 
library(pROC)
#install.packages("rsample")
library(rsample)

# Directorio
#  Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")

equipos <- read.csv("equipos.csv", header = TRUE)[[1]]
equipos <- "NO"
modelos <- c("modelo_1", "modelo_2", "modelo_3","modelo_4")


epochs_vec <- c(10, 20, 40, 60)  # Cantidad de épocas
gotas <- c(0.2,0.3, 0.4,0.5, 0.6)
lerners <- c(0.01, 0.02,0.03,0.05,0.06, 0.1)




#configuracion de la red
modelo <- nn_module(
  initialize = function(input_dim, 
                        hidden1 = input_dim*2,
                        hidden2 = input_dim*1
  ) {
    self$fc1 <- nn_linear(input_dim, hidden1)
    self$drop1 <- nn_dropout(p = p1) 
    self$fc2 <- nn_linear(hidden1, hidden2)
    self$drop2 <- nn_dropout(p = p2) 
    self$fc3 <- nn_linear(hidden2, 1)
  },
  
  forward = function(x) {
    x %>%
      self$fc1() %>%
      nnf_relu() %>%  
      self$drop1() %>%
      self$fc2() %>%
      nnf_relu() %>% 
      self$drop2() %>%
      self$fc3() %>%
      nnf_sigmoid()
  }
)

#configuracion de la red profunda
modelo_profundo <- nn_module(
  initialize = function(input_dim, 
                        hidden1 = input_dim*2,
                        hidden2 = input_dim*1.5,
                        hidden3 = input_dim*1
  ) {
    self$fc1 <- nn_linear(input_dim, hidden1)
    self$drop1 <- nn_dropout(p = gota1) 
    self$fc2 <- nn_linear(hidden1, hidden2)
    self$drop2 <- nn_dropout(p = gota2) 
    self$fc3 <- nn_linear(hidden2, hidden3)
    self$drop3 <- nn_dropout(p = gota3)
    self$fc4 <- nn_linear(hidden3, 1)
  },
  
  forward = function(x) {
    x %>%
      self$fc1() %>%
      nnf_relu() %>%  
      self$drop1() %>%
      self$fc2() %>%
      nnf_relu() %>% 
      self$drop2() %>%
      self$fc3() %>%
      self$drop3() %>%
      self$fc4() %>%
      nnf_sigmoid()
  }
)

modelados <- list(#modelo, 
                  modelo_profundo)
nombre_modelados <- c(#"no_profunda", 
                      "profunda")

for (equipo in equipos) {
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
  archivo <- paste0("plays_team_red_", equipo, ".csv")
  archivo2 <- paste0("plays_team_", equipo, ".csv")
  data <- read.csv(archivo)
  data2 <- read.csv(archivo2)
  corte <- as.numeric(table(data$isDropback)[2]/(nrow(data)))
  a <- 6+length(unique(data2$gameId))
  b <- a+1+length(unique(data2$offenseFormation))+length((unique(data2$receiverAlignment)))
  c <- b+7
  d <- ncol(data)-4
  e <- as.numeric(ncol(data))
  
  datos_modelos <- list(data[,c(2:a,b:c,d:e)], # Modelo 1
                        data[,c(2:a,b:e)], # Modelo 2
                        data[,c(2:e)], # Modelo 3
                        data[,c(2:c,d:e)] # Modelo 4
  )
  
  
  for (modelo in seq_along(modelos)) {
    datos <- datos_modelos[[modelo]]
    X <- datos[,c(1:(ncol(datos)-1))]
    y <- datos$isDropback
    # Separo los datos que voy a utilizar, 80%
    set.seed(57522978)
    idx <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))
    idx <- createDataPartition(y, p = 0.8, list = FALSE)
    X_train <- X[idx, ]
    y_train <- y[idx]
    X_test <- X[-idx, ]
    y_test <- y[-idx]
    # Crear folds
    set.seed(123)
    cv_folds <- vfold_cv(data = data.frame(X_train, y_train), v = 3)
    
      
    for (modelado in seq_along(modelados)) {
      
      if (nombre_modelados[modelado] == "no_profunda" ) {
        resultados <- data.frame(
          gota1 = numeric(), 
          gota2 = numeric(),
          lr = numeric(), 
          epocas = integer(), 
          exactitud = numeric(), 
          precision = numeric(),
          precision_secundaria = numeric(),
          promedio = numeric())
        for(gota1 in gotas) {
          
          for (gota2 in gotas) {
            
            for(lerner in lerners) {
              
              for (fold in seq_along(cv_folds$splits)) {
                train_data <- analysis(cv_folds$splits[[fold]])
                val_data <- assessment(cv_folds$splits[[fold]])
                X_tr <- as.matrix(train_data[, 1:ncol(X_train)])
                y_tr <- train_data$y_train
                X_val <- as.matrix(val_data[, 1:ncol(X_train)])
                y_val <- val_data$y_train
                # Convertir a tensores
                X_tr <- torch_tensor(X_tr, dtype = torch_float())
                y_tr <- torch_tensor(y_tr, dtype = torch_float())
                X_val <- torch_tensor(X_val, dtype = torch_float())
                y_val <- torch_tensor(y_val, dtype = torch_float())
                
                torch_manual_seed(123)
                net <- modelados[[modelado]](input_dim = ncol(X_tr))
                optimizer <- optim_adam(net$parameters, lr = lerner)
                loss_fn <- nnf_binary_cross_entropy
              
                for (epocas in epochs_vec) {
                  precisiones <- c()
                  precisiones_sec <- c()
                  exactitudes <- c()
                  for(epoca in seq_len(epocas)) {
                    optimizer$zero_grad()
                    output <- net(X_tr)
                    loss <- loss_fn(output, y_tr)
                    loss$backward()
                    optimizer$step()
                  }
                  with_no_grad({
                    preds <- as.numeric(net(X_val))})
                  orden <- order(preds, decreasing = TRUE)
                  n_positivos <- round(corte * length(preds))
                  # Inicializar predicción binaria
                  clases_predichas <- rep(0, length(preds))
                  clases_predichas[orden[1:n_positivos]] <- 1
                  label_test <- as.numeric(y_val)
                  exactitud <- mean(clases_predichas == label_test)
                  data <- tibble(
                    truth = factor(label_test),
                    estimate = factor(clases_predichas, levels = levels(factor(label_test)))
                  )
                  # Calcular la precisión
                  precision_value <- yardstick::precision(data, truth = truth, estimate = estimate, event_level = "second")$.estimate
                  precision_value_sec <- yardstick::precision(data, truth = truth, estimate = estimate)$.estimate
                  precisiones <- c(precisiones, precision_value)
                  precisiones_sec <- c(precisiones_sec, precision_value_sec)
                  exactitudes <- c(exactitudes, exactitud)
                  
                  resultados <- rbind(resultados, data.frame(
                    gota1 = gota1,
                    gota2 = gota2,
                    lr = lerner,
                    epocas = epocas,
                    exactitud = mean(exactitudes),
                    precision = mean(precisiones),
                    precision_secundaria = mean(precisiones_sec),
                    promedio = (mean(exactitudes)+mean(precisiones)+mean(precisiones_sec))/3))
                  
                }
                }
              }
          }
        }
            } else {
      #Data frame para guardar resultados
      resultados <- data.frame(
        gota1 = numeric(), 
        gota2 = numeric(),
        gota3 = numeric(),
        lr = numeric(), 
        epocas = integer(), 
        exactitud = numeric(), 
        precision = numeric(),
        precision_secundaria = numeric(),
        promedio = numeric())
      
      for(gota1 in gotas) {
        
        for (gota2 in gotas) {
          
          for (gota3 in gotas) {
            
            for(lerner in lerners) {
              
              for (fold in seq_along(cv_folds$splits)) {
                train_data <- analysis(cv_folds$splits[[fold]])
                val_data <- assessment(cv_folds$splits[[fold]])
                X_tr <- as.matrix(train_data[, 1:ncol(X_train)])
                y_tr <- train_data$y_train
                X_val <- as.matrix(val_data[, 1:ncol(X_train)])
                y_val <- val_data$y_train
                # Convertir a tensores
                X_tr <- torch_tensor(X_tr, dtype = torch_float())
                y_tr <- torch_tensor(y_tr, dtype = torch_float())
                X_val <- torch_tensor(X_val, dtype = torch_float())
                y_val <- torch_tensor(y_val, dtype = torch_float())
                
                torch_manual_seed(123)
                net <- modelados[[modelado]](input_dim = ncol(X_tr))
                optimizer <- optim_adam(net$parameters, lr = lerner)
                loss_fn <- nnf_binary_cross_entropy
                
                for (epocas in epochs_vec) {
                  precisiones <- c()
                  precisiones_sec <- c()
                  exactitudes <- c()
                  for(epoca in seq_len(epocas)) {
                    optimizer$zero_grad()
                    output <- net(X_tr)
                    loss <- loss_fn(output, y_tr)
                    loss$backward()
                    optimizer$step()
                  }
                  
                  with_no_grad({
                    preds <- as.numeric(net(X_val))})
                  orden <- order(preds, decreasing = TRUE)
                  n_positivos <- round(corte * length(preds))
                  
                  # Inicializar predicción binaria
                  clases_predichas <- rep(0, length(preds))
                  clases_predichas[orden[1:n_positivos]] <- 1
                  label_test <- as.numeric(y_val)
                  exactitud <- mean(clases_predichas == label_test)
                  data <- tibble(
                    truth = factor(label_test),
                    estimate = factor(clases_predichas, levels = levels(factor(label_test)))
                  )
                  # Calcular la precisión
                  precision_value <- yardstick::precision(data, truth = truth, estimate = estimate, event_level = "second")$.estimate
                  precision_value_sec <- yardstick::precision(data, truth = truth, estimate = estimate)$.estimate
                  precisiones <- c(precisiones, precision_value)
                  precisiones_sec <- c(precisiones_sec, precision_value_sec)
                  exactitudes <- c(exactitudes, exactitud)
                  resultados <- rbind(resultados, data.frame(
                    gota1 = gota1,
                    gota2 = gota2,
                    gota3 = gota3,
                    lr = lerner,
                    epocas = epocas,
                    exactitud = mean(exactitudes),
                    precision = mean(precisiones),
                    precision_secundaria = mean(precisiones_sec),
                    promedio = (mean(exactitudes)+mean(precisiones)+mean(precisiones_sec))/3))
                  
                }
                }
            }
          }
        }
      }
            }
      directorio <- paste0(setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red"))
      nombre <- paste0("log_red_", equipo, "_", nombre_modelados[modelado], "_", modelos[modelo], ".csv" )
      write.csv(resultados, nombre)
    }
  }
}


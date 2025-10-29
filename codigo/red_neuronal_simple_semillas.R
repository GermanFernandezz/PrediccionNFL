library(keras)
library(dplyr)
library(caret)
#install.packages("tensorflow")
library(tensorflow)
#install.packages("torch")
library(torch)
library(yardstick) 
library(pROC)

# Directorio
setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]

# Los modelos que definen las variables que se van a usar
modelos <- c("modelo_1", "modelo_2", "modelo_3", "modelo_4")

n_seeds <- 20
set.seed(28749658)
semillas <- sample(1:9999, n_seeds)

#configuracion de la red
modelo <- nn_module(
  initialize = function(input_dim, 
                        hidden1 = input_dim*2,
                        hidden2 = input_dim*1
  ) {
    self$fc1 <- nn_linear(input_dim, hidden1)
    self$drop1 <- nn_dropout(p = gota1) 
    self$fc2 <- nn_linear(hidden1, hidden2)
    self$drop2 <- nn_dropout(p = gota2) 
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

# Hago lista con las configuraciones de las redes
modelados <- list(modelo, 
                  modelo_profundo)

# nombres de las configuraciones de las redes
nombre_modelados <- c("no_profunda", 
                      "profunda")

# data frame para cargar los resultados de todos los equipos
resultados <- data.frame()

for (equipo in equipos) {
  # Directorio donde están los datos
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
  # Archivo con los datos preprocesados listos para usar en la red
  archivo <- paste0("plays_team_red_", equipo, ".csv")
  # Archivo que se usa para luego poder sacar la cantidad de variables del archivo anterior
  archivo2 <- paste0("plays_team_", equipo, ".csv")
  # Carga de ambos archivos
  data <- read.csv(archivo) #para poner en la red neuronal
  data2 <- read.csv(archivo2) #para poder seleccionar las variables
  corte <- as.numeric(table(data$isDropback)[2]/(nrow(data))) # proporcion de dropback TRUE
  a <- 6+length(unique(data2$gameId))
  b <- a+1+length(unique(data2$offenseFormation))+length((unique(data2$receiverAlignment)))
  c <- b+7
  d <- ncol(data)-4
  e <- as.numeric(ncol(data))
  
  # Lista con los diferentes sets de datos según la variables que vamos a usar
  datos_modelos <- list(data[,c(2:a,b:c,d:e)], # Modelo 1
                        data[,c(2:a,b:e)], # Modelo 2
                        data[,c(2:e)], # Modelo 3
                        data[,c(2:c,d:e)] # Modelo 4
  )
  # data frame donde voy a cargar los resultados de cada equipo
  resultado_equipo <- data.frame() 
  # Directorio donde está las optimizaciones y donde voy a guardar todos los resultados
  setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red")
  
  # Bucle donde selecciono los modelos de variables
  for (i in seq_along(modelos)) {
    datos <- datos_modelos[[i]] # Cargo datos según modelos
    X <- datos[,c(1:(ncol(datos)-1))] # Variables predictoras
    y <- datos$isDropback # variable a predecir
    
    # Separo los datos que voy a utilizar, 80%
    set.seed(57522978)
    idx <- sample(seq_len(nrow(datos)), size = 0.8 * nrow(datos))
    idx <- createDataPartition(y, p = 0.8, list = FALSE)
    X_train <- X[idx, ] #datos para entrenar
    y_train <- y[idx]   # variable a predecir para entrenar
    X_test <- X[-idx, ]  #datos para test
    y_test <- y[-idx]    #variable a predecir para test
    
    #paso a formato para meter en red
    X_train <- torch_tensor(as.matrix(X_train), dtype = torch_float()) 
    y_train <- torch_tensor(y_train, dtype = torch_float())
    X_test <- torch_tensor(as.matrix(X_test), dtype = torch_float())
    y_test <- torch_tensor(y_test, dtype = torch_float())
    
    ##Bucle donde selecciono los modelos de redes (profunda o no profunda)
    for (j in seq_along(modelados)) {
      #primero chequeo que tipo de modelado es, para saber profundidad de la red
      if (nombre_modelados[j] == "no_profunda" ) {
        #selecciono archivo con optmización de la red
        archivo_opt <- paste0("log_red_", equipo, "_no_profunda_", modelos[i], ".csv")
        #cargo los resultados de la optmización
        data_opt <- read.csv(archivo_opt)
        # Encontrar la fila con el mayor valor en 'promedio' y cargar los parametros
        fila_max_prom <- data_opt[which.max(data_opt$promedio), ]
        gota1 = fila_max_prom$gota1
        gota2 = fila_max_prom$gota2
        lerner = fila_max_prom$lr
        epocas = fila_max_prom$epocas
        
        #Entrenamiento de la red
        resultado_equipo_semilla <- data.frame()
        if (exists("predicciones")) rm(predicciones)
        
        for (n in seq_along(semillas)) {
          torch_manual_seed(semillas[n])
          net <- modelados[[j]](input_dim = ncol(X_train))
          optimizer <- optim_adam(net$parameters, lr = lerner)
          loss_fn <- nnf_binary_cross_entropy
          for(epoca in seq_len(epocas)) {
            optimizer$zero_grad()
            output <- net(X_train)
            loss <- loss_fn(output, y_train)
            loss$backward()
            optimizer$step()
          }
          with_no_grad({
            preds <- as.numeric(net(X_test))})
          
          if (!exists("predicciones")) {
            predicciones <- preds
          } else {
            predicciones <- cbind(predicciones, preds)
          }
          
          
          #ordeno las predicciones de mayor a menor
          orden <- order(preds, decreasing = TRUE)
          #asigno cuantos valores dropback deberían tener las predicciones
          n_positivos <- round(corte * length(preds))
          # Inicializar predicción binaria
          clases_predichas <- rep(0, length(preds)) #asigno 0 a todo el vector de clases predichas
          clases_predichas[orden[1:n_positivos]] <- 1 #asigno 1 a las posiciones 1:n_positivos del vector clases predichas0
          label_test <- as.numeric(y_test) #genero vector de vector real de entrenamiento
          exactitud <- mean(clases_predichas == label_test) 
          data <- tibble(
            truth = factor(label_test),
            estimate = factor(clases_predichas, levels = levels(factor(label_test)))
          )
          probabilidades <- cbind(X[-idx, ],data,preds)
          matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test)
          )
          write.csv(matriz_confusion,
                    paste0("matriz_confusion_red_simple_",equipo,"_",nombre_modelados[j], "_", modelos[i],"_", semillas[n],".csv"),
                    row.names = FALSE)
          write.csv(probabilidades,
                    paste0("probabilidades_red_simple_",equipo,"_",nombre_modelados[j], "_", modelos[i],"_", semillas[n],".csv"),
                    row.names = FALSE )
          
          resultado_equipo_semilla <-rbind(resultado_equipo_semilla,
                                           data.frame(equipo = equipo,
                                                      modelo = modelos[i],
                                                      modelado = nombre_modelados[j],
                                                      semilla = semillas[n],
                                                      gota1 = fila_max_prom$gota1,
                                                      gota2 = fila_max_prom$gota2,
                                                      gota3 = "NA",
                                                      lerner = fila_max_prom$lr,
                                                      epocas = fila_max_prom$epocas,
                                                      exactitud = mean(clases_predichas == label_test),
                                                      precision = yardstick::precision(data, 
                                                                                       truth = truth, 
                                                                                       estimate = estimate, 
                                                                                       event_level = "second"
                                                      )$.estimate,
                                                      precision_secundaria = yardstick::precision(data, 
                                                                                                  truth = truth, 
                                                                                                  estimate = estimate
                                                      )$.estimate,
                                                      f1 = f_meas(
                                                        data,
                                                        truth = truth,
                                                        estimate = estimate,
                                                        event_level = "second"
                                                      )$.estimate,
                                                      recall = yardstick::recall(
                                                        data,
                                                        truth = truth,
                                                        estimate = estimate,
                                                        event_level = "second"
                                                      )$.estimate)
          )
                                   
        }
        write.csv(resultado_equipo_semilla,
                  paste0("resultados_red_simple_semillas_",
                         equipo,"_",
                         nombre_modelados[j], "_", 
                         modelos[i],".csv"),
                  row.names = FALSE )
        resultado_equipo <- rbind(resultado_equipo,
                                  data.frame(equipo = equipo,
                                             modelo = modelos[i],
                                             modelado = nombre_modelados[j],
                                             gota1 = fila_max_prom$gota1,
                                             gota2 = fila_max_prom$gota2,
                                             gota3 = "NA",
                                             lerner = fila_max_prom$lr,
                                             epocas = fila_max_prom$epocas,
                                             exactitud = mean(resultado_equipo_semilla$exactitud),
                                             precision = mean(resultado_equipo_semilla$precision),
                                             precision_secundaria = mean(resultado_equipo_semilla$precision_secundaria),
                                             f1 = mean(resultado_equipo_semilla$f1),
                                             recall = mean(resultado_equipo_semilla$recall)))
        
        preds2 <- rowMeans(predicciones)
        orden <- order(preds2, decreasing = TRUE)
        #asigno cuantos valores dropback deberían tener las predicciones
        n_positivos <- round(corte * length(preds2))
        # Inicializar predicción binaria
        clases_predichas <- rep(0, length(preds2)) #asigno 0 a todo el vector de clases predichas
        clases_predichas[orden[1:n_positivos]] <- 1
        matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test))
        write.csv(matriz_confusion,
                  paste0("matriz_confusion_red_simple_",equipo,"_",nombre_modelados[j], "_", modelos[i], ".csv"),
                  row.names = FALSE)
        
          
        
 
        
        
      } else { 
        archivo_opt <- paste0("log_red_", equipo, "_profunda_", modelos[i], ".csv")
        data_opt <- read.csv(archivo_opt)
        # Encontrar la fila con el mayor valor en 'promedio'
        fila_max_prom <- data_opt[which.max(data_opt$promedio), ]
        gota1 = fila_max_prom$gota1
        gota2 = fila_max_prom$gota2
        gota3 = fila_max_prom$gota3
        lerner = fila_max_prom$lr
        epocas = fila_max_prom$epocas
        
        #Entrenamiento de la red
        resultado_equipo_semilla <- data.frame()
        if (exists("predicciones")) rm(predicciones)
        
        for (n in seq_along(semillas)) {
          torch_manual_seed(semillas[n])
          net <- modelados[[j]](input_dim = ncol(X_train))
          optimizer <- optim_adam(net$parameters, lr = lerner)
          loss_fn <- nnf_binary_cross_entropy
          for(epoca in seq_len(epocas)) {
            optimizer$zero_grad()
            output <- net(X_train)
            loss <- loss_fn(output, y_train)
            loss$backward()
            optimizer$step()
          }
          with_no_grad({
            preds <- as.numeric(net(X_test))})
          
          if (!exists("predicciones")) {
            predicciones <- preds
          } else {
            predicciones <- cbind(predicciones, preds)
          }
          
          #ordeno las predicciones de mayor a menor
          orden <- order(preds, decreasing = TRUE)
          #asigno cuantos valores dropback deberían tener las predicciones
          n_positivos <- round(corte * length(preds))
          # Inicializar predicción binaria
          clases_predichas <- rep(0, length(preds)) #asigno 0 a todo el vector de clases predichas
          clases_predichas[orden[1:n_positivos]] <- 1 #asigno 1 a las posiciones 1:n_positivos del vector clases predichas0
          label_test <- as.numeric(y_test) #genero vector de vector real de entrenamiento
          exactitud <- mean(clases_predichas == label_test) 
          data <- tibble(
            truth = factor(label_test),
            estimate = factor(clases_predichas, levels = levels(factor(label_test)))
          )
          probabilidades <- cbind(X[-idx, ],data,preds)
          matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test)
          )
          write.csv(matriz_confusion,
                    paste0("matriz_confusion_red_simple_",equipo,"_",nombre_modelados[j], "_", modelos[i],"_", semillas[n],".csv"),
                    row.names = FALSE)
          write.csv(probabilidades,
                    paste0("probabilidades_red_simple_",equipo,"_",nombre_modelados[j], "_", modelos[i],"_", semillas[n],".csv"),
                    row.names = FALSE )
          
          resultado_equipo_semilla <-rbind(resultado_equipo_semilla,
                                            data.frame(equipo = equipo,
                                                       modelo = modelos[i],
                                                       modelado = nombre_modelados[j],
                                                       semilla = semillas[n],
                                                       gota1 = fila_max_prom$gota1,
                                                       gota2 = fila_max_prom$gota2,
                                                       gota3 = fila_max_prom$gota3,
                                                       lerner = fila_max_prom$lr,
                                                       epocas = fila_max_prom$epocas,
                                                       exactitud = mean(clases_predichas == label_test),
                                                       precision = yardstick::precision(data, 
                                                                                        truth = truth, 
                                                                                        estimate = estimate, 
                                                                                        event_level = "second"
                                                       )$.estimate,
                                                       precision_secundaria = yardstick::precision(data, 
                                                                                                   truth = truth, 
                                                                                                   estimate = estimate
                                                       )$.estimate,
                                                       f1 = f_meas(
                                                         data,
                                                         truth = truth,
                                                         estimate = estimate,
                                                         event_level = "second"
                                                       )$.estimate,
                                                       recall = yardstick::recall(
                                                         data,
                                                         truth = truth,
                                                         estimate = estimate,
                                                         event_level = "second"
                                                       )$.estimate)
          )
                                   
        
        }
        write.csv(resultado_equipo_semilla,
                  paste0("resultados_red_simple_semillas_",
                         equipo,"_",
                         nombre_modelados[j], "_", 
                         modelos[i],".csv"),
                  row.names = FALSE )
        resultado_equipo <- rbind(resultado_equipo,
                                  data.frame(equipo = equipo,
                                             modelo = modelos[i],
                                             modelado = nombre_modelados[j],
                                             gota1 = fila_max_prom$gota1,
                                             gota2 = fila_max_prom$gota2,
                                             gota3 = fila_max_prom$gota3,
                                             lerner = fila_max_prom$lr,
                                             epocas = fila_max_prom$epocas,
                                             exactitud = mean(resultado_equipo_semilla$exactitud),
                                             precision = mean(resultado_equipo_semilla$precision),
                                             precision_secundaria = mean(resultado_equipo_semilla$precision_secundaria),
                                             f1 = mean(resultado_equipo_semilla$f1),
                                             recall = mean(resultado_equipo_semilla$recall)))
        
        preds2 <- rowMeans(predicciones)
        orden <- order(preds2, decreasing = TRUE)
        #asigno cuantos valores dropback deberían tener las predicciones
        n_positivos <- round(corte * length(preds2))
        # Inicializar predicción binaria
        clases_predichas <- rep(0, length(preds2)) #asigno 0 a todo el vector de clases predichas
        clases_predichas[orden[1:n_positivos]] <- 1
        matriz_confusion <- as.data.frame(table(Predicho = clases_predichas, Real = label_test))
        write.csv(matriz_confusion,
                  paste0("matriz_confusion_red_simple_",equipo,"_",nombre_modelados[j], "_", modelos[i], ".csv"),
                  row.names = FALSE)
        
        
      }
      

    }
  
  
  }
  # guardo resultados del equipo
  write.csv(
    resultado_equipo,
    paste0("resultado_red_simple_semillas_",equipo,".csv"),
    row.names = FALSE)
  resultados <- rbind(resultados, resultado_equipo)
}

write.csv(resultados,
          "resultados_red_simple_semillas.csv",
          row.names = FALSE)

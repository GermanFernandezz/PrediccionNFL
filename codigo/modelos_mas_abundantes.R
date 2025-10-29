library(ggplot2)
library(dplyr)


setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
equipos <- read.csv("equipos.csv", header = TRUE)[[1]]

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/red/simple_semillas")
resultados_max_red <- data.frame()
for (equipo in equipos) {
  archivo <- paste0("resultado_red_simple_semillas_",equipo, ".csv")
  resultados <- read.csv(archivo)
  max_exactitud <- max(resultados[[ "exactitud" ]])
  resultados_max_equipo <- resultados[resultados$exactitud == max_exactitud, ]
  resultados_max_red <- rbind(resultados_max_red, resultados_max_equipo)
}
resultados_max_red$modelaje <- paste0(resultados_max_red$modelo, "_",resultados_max_red$modelado)

modelo_red <- as.data.frame(table(resultados_max_red$modelo))
modelado_red <- as.data.frame(table(resultados_max_red$modelado))
modelaje_red <- as.data.frame(table(resultados_max_red$modelaje))

ggplot(modelo_red, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 10)
  ) +
  labs(x = "Complejidad Modelo", y = "Frecuencia")

ggplot(modelado_red, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 10)
  ) +
  labs(x = "Profundidad Modelo", y = "Frecuencia")

ggplot(modelaje_red, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 10,angle = 90)
  ) +
  labs(x = "Profundidad Modelo", y = "Frecuencia")


resultados_max_red_unico <- resultados_max_red %>%
  distinct(equipo, .keep_all = TRUE)

ggplot(resultados_max_red_unico, aes(x = reorder(equipo, -exactitud), y = exactitud)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 10, angle = 90)
  ) +
  labs(x = "Equipo", y = "Exactitud")


barplot(table(resultados_max_red$modelo))
barplot(table(resultados_max_red$modelado))
barplot(table(resultados_max_red$modelaje))

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple_semillas")
resultados_max_lgb <- data.frame()
for (equipo in equipos) {
  archivo <- paste0("resultado_lightgbm_simple_semilla_",equipo, ".csv")
  resultados <- read.csv(archivo)
  max_exactitud <- max(resultados[[ "exactitud" ]])
  resultados_max_equipo <- resultados[resultados$exactitud == max_exactitud, ]
  resultados_max_lgb <- rbind(resultados_max_lgb, resultados_max_equipo)
  
}

resultados_max_lgb$modelaje <- paste0(resultados_max_lgb$modelo, "_", resultados_max_lgb$optimizacion)

modelo_lgb <- as.data.frame(table(resultados_max_lgb$modelo))
optimizacion_lgb <- as.data.frame(table(resultados_max_lgb$optimizacion))
modelaje_lgb <- as.data.frame(table(resultados_max_lgb$modelaje))

ggplot(modelo_lgb, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 10)
  ) +
  labs(x = "Complejidad Modelo", y = "Frecuencia")

ggplot(optimizacion_lgb, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 10)
  ) +
  labs(x = "Optimizacion", y = "Frecuencia")

ggplot(modelaje_lgb, aes(x = reorder(Var1, -Freq), y = Freq)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 10,angle = 90)
  ) +
  labs(x = "Modelo completo", y = "Frecuencia")



resultados_max_lgb_unico <- resultados_max_lgb %>%
  distinct(equipo, .keep_all = TRUE)

ggplot(resultados_max_lgb_unico, aes(x = reorder(equipo, -exactitud), y = exactitud)) +
  geom_col() +
  theme_minimal() +
  theme(
    legend.text = element_text(size = 20),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text.x = element_text(size = 10, angle = 90)
  ) +
  labs(x = "Equipo", y = "Exactitud")





barplot(table(resultados_max$modelo))
barplot(table(resultados_max$optimizacion))
barplot(table(resultados_max$modelaje))


setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/azar")
resultados_azar <- read.csv("resultados_azar_semilla.csv")



resultados_max_lgb_unico$Modelo_ML <- "LightGBM"
resultados_max_red_unico$Modelo_ML <- "Red Neuronal"
resultados_azar$Modelo_ML <- "Azar"



resultados_max_lgb_unico_exactitud <- resultados_max_lgb_unico[,c(1,11,18)]
resultados_max_red_unico_exactitud <- resultados_max_red_unico[,c(1,9,15)]
resultados_azar_exactitud <- resultados_azar[,c(1,3,9)]

resultados_todos_max_unicos <- bind_rows(resultados_max_lgb_unico_exactitud, resultados_max_red_unico_exactitud, resultados_azar_exactitud)

# Elegir el modelo base para ordenar (puede ser "Red Neuronal" o "LightGBM")
modelo_base <- "Red Neuronal"

# Calcular el orden según las exactitudes del modelo elegido
orden_equipos <- resultados_todos_max_unicos %>%
  filter(Modelo_ML == modelo_base) %>%
  arrange(desc(exactitud)) %>%
  pull(equipo)

# Asegurar que el factor 'equipo' respete ese orden
resultados_todos_max_unicos$equipo <- factor(resultados_todos_max_unicos$equipo,
                                             levels = orden_equipos)

# Graficar ambos métodos respetando ese orden
ggplot(resultados_todos_max_unicos,
       aes(x = equipo, y = exactitud, fill = Modelo_ML)) +
  geom_col(position = "dodge",width = 0.7) +
  scale_fill_manual(
    values = c("Red Neuronal" = "#E69F00", "LightGBM" = "#56B4E9", "Azar" = "#D55E00")
  ) +
  theme_minimal() +
  labs(x = "Equipo", y = "Exactitud", fill = "Método") +
  theme(
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text.x = element_text(size = 10, angle = 90)
  )
  



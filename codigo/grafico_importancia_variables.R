library(ggplot2)

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados/optimizaciones/lightgbm/simple_semillas")

importancia_1 <- read.csv("importancia_lightgbm_simple_SF_modelo_1_grid.csv")
importancia_2 <- read.csv("importancia_lightgbm_simple_PHI_modelo_2_grid.csv")
importancia_3 <- read.csv("importancia_kansas_resultados_lightgbm_final_defecto_modelo_3.csv")[,c(2:3)]
importancia_4 <- read.csv("importancia_kansas_resultados_lightgbm_final_defecto_modelo_4.csv")[,c(2:3)]

importancia <- merge(importancia_1, importancia_2, by = "Feature", all = TRUE)
importancia <- merge(importancia, importancia_3, by = "Feature", all = TRUE)
importancia <- merge(importancia, importancia_4, by = "Feature", all = TRUE)


ggplot(importancia) +
  geom_point(aes(x = Feature, y = promedio_gain.x, color = "Modelo 1")) +
  geom_point(aes(x = Feature, y = promedio_gain.y, color = "Modelo 2")) +
  labs(color = "Modelo", x = "Variables") +
  theme(axis.text.x = element_text(angle = 65, hjust = 1))


ggplot(importancia_1) +
  geom_point(aes(x = Feature, y = promedio_gain)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  labs(x = "Variable", y = "Promedio Gain")

ggplot(importancia_2) +
  geom_point(aes(x = Feature, y = promedio_gain)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) +
  labs(x = "Variable", y = "Promedio Gain")

prueba <- importancia_1$Feature[order(importancia_1$promedio_gain)]

importancia_2_filtrado <- importancia_2[ !(importancia_2$Feature %in% prueba), ]
prueba2 <- importancia_2_filtrado$Feature[order(importancia_2_filtrado$promedio_gain)]

orden <- c(prueba, prueba2)

importancia_3_filtrado <- importancia_3[ !(importancia_3$Feature %in% orden), ]
prueba3 <- importancia_3_filtrado$Feature[order(importancia_3_filtrado$promedio_gain)]

orden <- c(orden, prueba3)

importancia$Feature <- factor(importancia$Feature,
                              levels = orden)

colnames(importancia) <- c("Feature", "modelo_1", "modelo_2", "modelo_3", "modelo_4")

ggplot(importancia) +
  geom_point(aes(x = Feature, y = modelo_1, color = "Modelo 1")) +
  geom_point(aes(x = Feature, y = modelo_2, color = "Modelo 2")) +
  geom_point(aes(x = Feature, y = modelo_3, color = "Modelo 3")) +
  geom_point(aes(x = Feature, y = modelo_4, color = "Modelo 4")) +
  labs(color = "Modelo", x = "Variables", y= "Gain") +
  theme_minimal()   +
  theme(axis.text.x = element_text(angle = 65, hjust = 1)) 

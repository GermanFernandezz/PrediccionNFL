library(dplyr)

setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
# Archivo con los datos preprocesados listos para usar
archivo_equipo <- paste0("plays_team_KC.csv")
# Carga de datos
data <- read.csv(archivo_equipo)


data <- data %>%
  mutate(
    timeCategory_bis = case_when(
      gameClock_mod >= 14 & gameClock_mod <= 15  ~ "0-1 min",
      gameClock_mod >= 13 & gameClock_mod < 14 ~ "1-2 min",
      gameClock_mod >= 12 & gameClock_mod < 13 ~ "2-3 min",
      gameClock_mod >= 11 & gameClock_mod < 12 ~ "3-4 min",
      gameClock_mod >= 10 & gameClock_mod < 11 ~ "4-5 min",
      gameClock_mod >= 9 & gameClock_mod <  10 ~ "5-6 min",
      gameClock_mod >= 8 & gameClock_mod <  9 ~ "6-7 min",
      gameClock_mod >= 7 & gameClock_mod <  8 ~ "7-8 min",
      gameClock_mod >= 6 & gameClock_mod <  7 ~ "8-9 min",
      gameClock_mod >= 5 & gameClock_mod <  6 ~ "9-10 min",
      gameClock_mod >= 4 & gameClock_mod <  5 ~ "10-11 min",
      gameClock_mod >= 3 & gameClock_mod <  4 ~ "11-12 min",
      gameClock_mod >= 2 & gameClock_mod <  3 ~ "12-13 min",
      gameClock_mod >= 1 & gameClock_mod <  2 ~ "13-14 min",
      gameClock_mod >= 0 & gameClock_mod <  1 ~ "14-15 min",
    ),
    timeCategory_bis = factor(timeCategory_bis, levels = c("0-1 min",
                                                           "1-2 min",
                                                           "2-3 min",
                                                           "3-4 min",
                                                           "4-5 min",
                                                           "5-6 min",
                                                           "6-7 min",
                                                           "7-8 min",
                                                           "8-9 min",
                                                           "9-10 min",
                                                           "10-11 min",
                                                           "11-12 min",
                                                           "12-13 min",
                                                           "13-14 min",
                                                           "14-15 min")) # Orden lógico
  )
    

ggplot(data, aes(x = timeCategory_bis, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("Acarreo", "Pase"), name = NULL) +
  labs(x = "Intervalo de tiempo", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1))



data <- data %>%
  mutate(
    yardsToGoCategory = case_when(
      yardsToGo >= 15   ~ "Mas de 15 yardas",
      yardsToGo >= 12 & yardsToGo < 15 ~ "12-15 yardas",
      yardsToGo >= 9 & yardsToGo < 12 ~ "9-12 yardas",
      yardsToGo >= 6 & yardsToGo < 9 ~ "6-9 yardas",
      yardsToGo >= 3 & yardsToGo < 6 ~ "3-6 yardas",
      yardsToGo >= 0 & yardsToGo < 3 ~ "0-3 yardas"
    ),
    yardsToGoCategory = factor(yardsToGoCategory, levels = c("0-3 yardas",
                                                             "3-6 yardas",
                                                             "6-9 yardas",
                                                             "9-12 yardas",
                                                             "12-15 yardas",
                                                             "Mas de 15 yardas")) # Orden lógico
  )


ggplot(data, aes(x = yardsToGoCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Yards To Go", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1))


data <- data %>%
  mutate(
    yardlineNumberCategory = case_when(
      yardlineNumber > 40 & yardlineNumber <= 50 & yardlineSide_mod == "KC"   ~ "KC 40-50 yardas",
      yardlineNumber > 30 & yardlineNumber <= 40 & yardlineSide_mod == "KC"   ~ "KC 30-40 yardas",
      yardlineNumber > 20 & yardlineNumber <= 50 & yardlineSide_mod == "KC"   ~ "KC 20-30 yardas",
      yardlineNumber > 10 & yardlineNumber <= 20 & yardlineSide_mod == "KC"   ~ "KC 10-20 yardas",
      yardlineNumber > 0 & yardlineNumber <= 10 & yardlineSide_mod == "KC"   ~ "KC 0-10 yardas",
      yardlineNumber == 50  ~ "Medio Campo",
      yardlineNumber > 0 & yardlineNumber <= 10 & yardlineSide_mod == "rival"   ~ "Rival 0-10 yardas",
      yardlineNumber > 10 & yardlineNumber <= 20 & yardlineSide_mod == "rival"   ~ "Rival 10-20 yardas",
      yardlineNumber > 20 & yardlineNumber <= 30 & yardlineSide_mod == "rival"   ~ "Rival 20-30 yardas",
      yardlineNumber > 30 & yardlineNumber <= 40 & yardlineSide_mod == "rival"   ~ "Rival 30-40 yardas",
      yardlineNumber > 40 & yardlineNumber <= 50 & yardlineSide_mod == "rival"   ~ "Rival 40-50 yardas"),
    yardlineNumberCategory = factor(yardlineNumberCategory, levels = c("KC 0-10 yardas",
                                                                       "KC 10-20 yardas",
                                                                       "KC 20-30 yardas",
                                                                       "KC 30-40 yardas",
                                                                       "KC 40-50 yardas",
                                                                       "Medio Campo",
                                                                       "Rival 40-50 yardas",
                                                                       "Rival 30-40 yardas",
                                                                       "Rival 20-30 yardas",
                                                                       "Rival 10-20 yardas",
                                                                       "Rival 0-10 yardas"))# Orden lógico
  )

ggplot(data, aes(x = yardlineNumberCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Posicion en campo de juego", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1))




data <- data %>%
  mutate(
    diferenciaCategory = case_when(
      diferencia >= 20 ~ "Gana por mas de 20 puntos",
      diferencia > 15 & diferencia <= 20 ~ "Gana 15-20 puntos",
      diferencia > 10 & diferencia <= 15 ~ "Gana 10-15 puntos",
      diferencia > 5 & diferencia <= 10 ~ "Gana 5-10 puntos",
      diferencia > 0 & diferencia <= 5 ~ "Gana 0-5 puntos",
      diferencia == 0  ~ "Empatado",
      diferencia <= -20 ~ "Pierde por mas de 20 puntos",
      diferencia <= -15 & diferencia > -20 ~ "Pierde 15-20 puntos",
      diferencia <= -10 & diferencia > -15 ~ "Pierde 10-15 puntos",
      diferencia <= -5 & diferencia > -10 ~ "Pierde 5-10 puntos",
      diferencia <= 0 & diferencia > -5 ~ "Pierde 0-5 puntos"),
    diferenciaCategory = factor(diferenciaCategory, levels = c("Gana por mas de 20 puntos",
                                                               "Gana 15-20 puntos",
                                                               "Gana 10-15 puntos",
                                                               "Gana 5-10 puntos",
                                                               "Gana 0-5 puntos",
                                                               "Empatado",
                                                               "Pierde por mas de 20 puntos",
                                                               "Pierde 15-20 puntos",
                                                               "Pierde 10-15 puntos",
                                                               "Pierde 5-10 puntos",
                                                               "Pierde 0-5 puntos"))# Orden lógico
  )

ggplot(data, aes(x = diferenciaCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Diferencia de puntos", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 65, hjust = 1))




data <- data %>%
  mutate(
    absoluteYardlineNumberCategory = case_when(
      absoluteYardlineNumber > 100 & absoluteYardlineNumber <= 110 ~ "100-110 yardas",
      absoluteYardlineNumber > 90 & absoluteYardlineNumber <= 100 ~ "90-100 yardas",
      absoluteYardlineNumber > 80 & absoluteYardlineNumber <= 90 ~ "80-90 yardas",
      absoluteYardlineNumber > 70 & absoluteYardlineNumber <= 80 ~ "70-80 yardas",
      absoluteYardlineNumber > 60 & absoluteYardlineNumber <= 70 ~ "60-70 yardas",
      absoluteYardlineNumber > 50 & absoluteYardlineNumber <= 60 ~ "50-60 yardas",
      absoluteYardlineNumber > 40 & absoluteYardlineNumber <= 50 ~ "40-50 yardas",
      absoluteYardlineNumber > 30 & absoluteYardlineNumber <= 40 ~ "30-40 yardas",
      absoluteYardlineNumber > 20 & absoluteYardlineNumber <= 30 ~ "20-30 yardas",
      absoluteYardlineNumber > 10 & absoluteYardlineNumber <= 20 ~ "10-20 yardas",
      absoluteYardlineNumber > 0 & absoluteYardlineNumber <= 10 ~ "0-10 yardas"),
    absoluteYardlineNumberCategory = factor(absoluteYardlineNumberCategory, levels = c("100-110 yardas",
                                                                       "90-100 yardas",
                                                                       "80-90 yardas",
                                                                       "70-80 yardas",
                                                                       "60-70 yardas",
                                                                       "50-60 yardas",
                                                                       "40-50 yardas",
                                                                       "30-40 yardas",
                                                                       "20-30 yardas",
                                                                       "10-20 yardas",
                                                                       "0-10 yardas"))# Orden lógico
  )

ggplot(data, aes(x = absoluteYardlineNumberCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Yaras absolutas", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1))




setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
# Archivo con los datos preprocesados listos para usar
archivo_equipo <- paste0("plays_team_PHI.csv")
# Carga de datos
data <- read.csv(archivo_equipo)


data <- data %>%
  mutate(
    timeCategory_bis = case_when(
      gameClock_mod >= 14 & gameClock_mod <= 15  ~ "0-1 min",
      gameClock_mod >= 13 & gameClock_mod < 14 ~ "1-2 min",
      gameClock_mod >= 12 & gameClock_mod < 13 ~ "2-3 min",
      gameClock_mod >= 11 & gameClock_mod < 12 ~ "3-4 min",
      gameClock_mod >= 10 & gameClock_mod < 11 ~ "4-5 min",
      gameClock_mod >= 9 & gameClock_mod <  10 ~ "5-6 min",
      gameClock_mod >= 8 & gameClock_mod <  9 ~ "6-7 min",
      gameClock_mod >= 7 & gameClock_mod <  8 ~ "7-8 min",
      gameClock_mod >= 6 & gameClock_mod <  7 ~ "8-9 min",
      gameClock_mod >= 5 & gameClock_mod <  6 ~ "9-10 min",
      gameClock_mod >= 4 & gameClock_mod <  5 ~ "10-11 min",
      gameClock_mod >= 3 & gameClock_mod <  4 ~ "11-12 min",
      gameClock_mod >= 2 & gameClock_mod <  3 ~ "12-13 min",
      gameClock_mod >= 1 & gameClock_mod <  2 ~ "13-14 min",
      gameClock_mod >= 0 & gameClock_mod <  1 ~ "14-15 min",
    ),
    timeCategory_bis = factor(timeCategory_bis, levels = c("0-1 min",
                                                           "1-2 min",
                                                           "2-3 min",
                                                           "3-4 min",
                                                           "4-5 min",
                                                           "5-6 min",
                                                           "6-7 min",
                                                           "7-8 min",
                                                           "8-9 min",
                                                           "9-10 min",
                                                           "10-11 min",
                                                           "11-12 min",
                                                           "12-13 min",
                                                           "13-14 min",
                                                           "14-15 min")) # Orden lógico
  )


ggplot(data, aes(x = timeCategory_bis, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Intervalo de tiempo", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1))

data <- data %>%
  mutate(
    yardlineNumberCategory = case_when(
      yardlineNumber > 40 & yardlineNumber <= 50 & yardlineSide_mod == "PHI"   ~ "PHI 40-50 yardas",
      yardlineNumber > 30 & yardlineNumber <= 40 & yardlineSide_mod == "PHI"   ~ "PHI 30-40 yardas",
      yardlineNumber > 20 & yardlineNumber <= 50 & yardlineSide_mod == "PHI"   ~ "PHI 20-30 yardas",
      yardlineNumber > 10 & yardlineNumber <= 20 & yardlineSide_mod == "PHI"   ~ "PHI 10-20 yardas",
      yardlineNumber > 0 & yardlineNumber <= 10 & yardlineSide_mod == "PHI"   ~ "PHI 0-10 yardas",
      yardlineNumber == 50  ~ "Medio Campo",
      yardlineNumber > 0 & yardlineNumber <= 10 & yardlineSide_mod == "rival"   ~ "Rival 0-10 yardas",
      yardlineNumber > 10 & yardlineNumber <= 20 & yardlineSide_mod == "rival"   ~ "Rival 10-20 yardas",
      yardlineNumber > 20 & yardlineNumber <= 30 & yardlineSide_mod == "rival"   ~ "Rival 20-30 yardas",
      yardlineNumber > 30 & yardlineNumber <= 40 & yardlineSide_mod == "rival"   ~ "Rival 30-40 yardas",
      yardlineNumber > 40 & yardlineNumber <= 50 & yardlineSide_mod == "rival"   ~ "Rival 40-50 yardas"),
    yardlineNumberCategory = factor(yardlineNumberCategory, levels = c("PHI 0-10 yardas",
                                                                       "PHI 10-20 yardas",
                                                                       "PHI 20-30 yardas",
                                                                       "PHI 30-40 yardas",
                                                                       "PHI 40-50 yardas",
                                                                       "Medio Campo",
                                                                       "Rival 40-50 yardas",
                                                                       "Rival 30-40 yardas",
                                                                       "Rival 20-30 yardas",
                                                                       "Rival 10-20 yardas",
                                                                       "Rival 0-10 yardas"))# Orden lógico
  )

ggplot(data, aes(x = yardlineNumberCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Posicion en campo de juego", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1))

data <- data %>%
  mutate(
    yardsToGoCategory = case_when(
      yardsToGo >= 15   ~ "Mas de 15 yardas",
      yardsToGo >= 12 & yardsToGo < 15 ~ "12-15 yardas",
      yardsToGo >= 9 & yardsToGo < 12 ~ "9-12 yardas",
      yardsToGo >= 6 & yardsToGo < 9 ~ "6-9 yardas",
      yardsToGo >= 3 & yardsToGo < 6 ~ "3-6 yardas",
      yardsToGo >= 0 & yardsToGo < 3 ~ "0-3 yardas"
    ),
    yardsToGoCategory = factor(yardsToGoCategory, levels = c("0-3 yardas",
                                                             "3-6 yardas",
                                                             "6-9 yardas",
                                                             "9-12 yardas",
                                                             "12-15 yardas",
                                                             "Mas de 15 yardas")) # Orden lógico
  )


ggplot(data, aes(x = yardsToGoCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Yards To Go", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1))


setwd("D:/MaestriaDataMining/tallerdetesisi/nfl-big-data-bowl-2025/equipos_preprocesados")
# Archivo con los datos preprocesados listos para usar
archivo_equipo <- paste0("plays_team_SF.csv")
# Carga de datos
data <- read.csv(archivo_equipo)


data <- data %>%
  mutate(
    diferenciaCategory = case_when(
      diferencia >= 20 ~ "Gana por mas de 20 puntos",
      diferencia > 15 & diferencia <= 20 ~ "Gana 15-20 puntos",
      diferencia > 10 & diferencia <= 15 ~ "Gana 10-15 puntos",
      diferencia > 5 & diferencia <= 10 ~ "Gana 5-10 puntos",
      diferencia > 0 & diferencia <= 5 ~ "Gana 0-5 puntos",
      diferencia == 0  ~ "Empatado",
      diferencia <= -20 ~ "Pierde por mas de 20 puntos",
      diferencia <= -15 & diferencia > -20 ~ "Pierde 15-20 puntos",
      diferencia <= -10 & diferencia > -15 ~ "Pierde 10-15 puntos",
      diferencia <= -5 & diferencia > -10 ~ "Pierde 5-10 puntos",
      diferencia <= 0 & diferencia > -5 ~ "Pierde 0-5 puntos"),
    diferenciaCategory = factor(diferenciaCategory, levels = c("Gana por mas de 20 puntos",
                                                               "Gana 15-20 puntos",
                                                               "Gana 10-15 puntos",
                                                               "Gana 5-10 puntos",
                                                               "Gana 0-5 puntos",
                                                               "Empatado",
                                                               "Pierde por mas de 20 puntos",
                                                               "Pierde 15-20 puntos",
                                                               "Pierde 10-15 puntos",
                                                               "Pierde 5-10 puntos",
                                                               "Pierde 0-5 puntos"))# Orden lógico
  )

ggplot(data, aes(x = diferenciaCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Diferencia de puntos", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 65, hjust = 1))


data <- data %>%
  mutate(
    yardsToGoCategory = case_when(
      yardsToGo >= 15   ~ "Mas de 15 yardas",
      yardsToGo >= 12 & yardsToGo < 15 ~ "12-15 yardas",
      yardsToGo >= 9 & yardsToGo < 12 ~ "9-12 yardas",
      yardsToGo >= 6 & yardsToGo < 9 ~ "6-9 yardas",
      yardsToGo >= 3 & yardsToGo < 6 ~ "3-6 yardas",
      yardsToGo >= 0 & yardsToGo < 3 ~ "0-3 yardas"
    ),
    yardsToGoCategory = factor(yardsToGoCategory, levels = c("0-3 yardas",
                                                             "3-6 yardas",
                                                             "6-9 yardas",
                                                             "9-12 yardas",
                                                             "12-15 yardas",
                                                             "Mas de 15 yardas")) # Orden lógico
  )


ggplot(data, aes(x = yardsToGoCategory, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Yards To Go", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10,angle = 45, hjust = 1))


ggplot(data, aes(x = down, fill = factor(isDropback))) +
  geom_bar(position = "fill") +
  geom_text(
    stat = "count",
    aes(label = scales::percent(after_stat(count) / tapply(after_stat(count),after_stat(x), sum)[after_stat(x)],
                                accuracy = 1)),
    position = position_fill(vjust = 0.5),
    color = "white", size = 3
  )+
  scale_fill_manual(values = c("#E69F00", "#56B4E9"), labels = c("No Dropback", "Dropback"), name = NULL) +
  labs(x = "Down", y = "Proporción", title = NULL) +
  theme_minimal() +
  theme(legend.text = element_text(size = 15),
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15), 
        axis.text.x = element_text(size = 10))

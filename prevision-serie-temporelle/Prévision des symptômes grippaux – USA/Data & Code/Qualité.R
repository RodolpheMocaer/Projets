#library
library(readxl)
library(ggplot2)
library(tidyr)

#importation données
prévision <- read_excel("/Users/rododo/Desktop/BASE DE DONNEES USA/Database_prev.xlsx")
#renomage
names(prévision) <- c("Incidence", "Méthodes", "Date")
#séparation de la base de données
prévision2 <- subset(prévision, Méthodes != "obs_grippe_2023")
#bon typage de la base de données
str(prévision)
#Selection des obs#
OBS <- subset(prévision, Méthodes == "obs_grippe_2023")

#Qualités de prévision
#Visualisation graphique de l'ensemble des prévisions
ggplot(prévision2, aes(x = Date, y = Incidence, colour = Méthodes)) +
  geom_line(aes(linetype = "Prévision"), size = 0.9, alpha = 0.7) +
  geom_line(data = OBS, aes(x = Date, y = Incidence, linetype = "Observation"), colour = "#00561b", size = 2) +
  theme_bw() + 
  ggtitle("Prévision de l'incidence des symptômes grippaux") +
  xlab("Date") + 
  scale_linetype_manual(name = "Prévision et Observation",
                        values = c("Prévision" = "longdash", "Observation" = "solid"),
                        labels = c("Observation", "Prévision"))
#Séparation par catégorie
analogue <- subset(prévision, grepl("^analogue", Méthodes))
compartimentale <- subset(prévision, grepl("prevision$", Méthodes))
multivariées <- subset(prévision, grepl("^(AR|lm)", Méthodes))
univariées <- subset(prévision2, !(grepl("^analogue", Méthodes) | grepl("prevision$", Méthodes) | grepl("^(AR|lm)", Méthodes)))

#Visualisation par catégorie
#Analogue & compartimentale
ggplot(analogue, aes(x = Date, y = Incidence, colour = Méthodes)) +
  geom_line(aes(linetype = "Prévision"), size = 0.9, alpha = 0.7) +
  geom_line(data = OBS, aes(x = Date, y = Incidence, linetype = "Observation"), colour = "#00561b", size = 2) +
  theme_bw() + 
  ggtitle("Prévision de l'incidence des symptômes grippaux") +
  xlab("Date") + 
  scale_linetype_manual(name = "Prévision et Observation",
                        values = c("Prévision" = "longdash", "Observation" = "solid"),
                        labels = c("Observation", "Prévision"))
#Compartimentale
ggplot(compartimentale, aes(x = Date, y = Incidence, colour = Méthodes)) +
  geom_line(aes(linetype = "Prévision"), size = 0.9, alpha = 0.7) +
  geom_line(data = OBS, aes(x = Date, y = Incidence, linetype = "Observation"), colour = "#00561b", size = 2) +
  theme_bw() + 
  ggtitle("Prévision de l'incidence des symptômes grippaux") +
  xlab("Date") + 
  scale_linetype_manual(name = "Prévision et Observation",
                        values = c("Prévision" = "longdash", "Observation" = "solid"),
                        labels = c("Observation", "Prévision"))
#Multivariées
ggplot(multivariées, aes(x = Date, y = Incidence, colour = Méthodes)) +
  geom_line(aes(linetype = "Prévision"), size = 0.9, alpha = 0.7) +
  geom_line(data = OBS, aes(x = Date, y = Incidence, linetype = "Observation"), colour = "#00561b", size = 2) +
  theme_bw() + 
  ggtitle("Prévision de l'incidence des symptômes grippaux") +
  xlab("Date") + 
  scale_linetype_manual(name = "Prévision et Observation",
                        values = c("Prévision" = "longdash", "Observation" = "solid"),
                        labels = c("Observation", "Prévision"))
#Univariées
ggplot(univariées, aes(x = Date, y = Incidence, colour = Méthodes)) +
  geom_line(aes(linetype = "Prévision"), size = 0.9, alpha = 0.7) +
  geom_line(data = OBS, aes(x = Date, y = Incidence, linetype = "Observation"), colour = "#00561b", size = 2) +
  theme_bw() + 
  ggtitle("Prévision de l'incidence des symptômes grippaux") +
  xlab("Date") + 
  scale_linetype_manual(name = "Prévision et Observation",
                        values = c("Prévision" = "longdash", "Observation" = "solid"),
                        labels = c("Observation", "Prévision"))

#pivotage des données
pc <- pivot_wider(prévision, 
                             names_from = "Méthodes", # Spécifiez la colonne à utiliser pour créer de nouvelles colonnes
                             values_from = "Incidence") # Spécifiez la colonne contenant les valeurs à éclater en colonnes
obs <- pc$obs_grippe_2023
pc <- pc[,-c(1,4)]
colnames(pc)

#MAE : erreur absolue moyenne
calculate_mae <- function(obs, predictions) {
  mae <- numeric(length(predictions))
  for (i in seq_along(predictions)) {
    mae[i] <- abs(1/12 * sum(predictions[[i]] - obs))
  }
  return(mae)
}
MAE <- calculate_mae(obs,pc)

#MAPE : erreur absolue moyenne en pourcentage
calculate_mape <- function(obs, predictions) {
  mape <- numeric(length(predictions))
  for (i in seq_along(predictions)) {
    mape[i] <- 1/12 * (sum(abs(predictions[[i]] - obs))/abs(obs))*100 
  }
  return(mape)
}
MAPE <- calculate_mape(obs,pc)

#DE : Distance euclidienne
calculate_de <- function(obs, predictions) {
  de <- numeric(length(predictions))
  for (i in seq_along(predictions)) {
    de[i] <- sqrt((obs - predictions[[i]])^2)
  }
  return(de)
}
DE <- calculate_de(obs,pc)

#CPS : Score de prédiction cohérente
calculate_cps <- function(obs, predictions) {
  cps <- numeric(length(predictions))
  for (i in seq_along(predictions)) {
    cps[i] <- 1 - (sum( (predictions[[i]] - mean( predictions[[i]]))^2) / sum( (obs - mean( predictions[[i]]))^2) )
  }
  return(cps)
}
CPS <- calculate_cps(obs,pc)

#RGME : erreur de moyenne géométrique relative
calculate_RGME <- function(obs, predictions) {
  rgme <- numeric(length(predictions))
  for (i in seq_along(predictions)) {
    rgme[i] <- (prod( predictions[[i]] / obs))^(1/12)
  }
  return(rgme)
}
RGME <- calculate_RGME(obs,pc)

#RlME : erreur de moyenne logarithmique relatives
calculate_RLME <- function(obs, predictions) {
  rlme <- numeric(length(predictions))
  for (i in seq_along(predictions)) {
    rlme[i] <- exp( 1/12 * sum( log( predictions[[i]] / obs)))
  }
  return(rlme)
}
RLME <- calculate_RLME(obs,pc)

#Création d'un data frame
df_prev <- data_frame(
  Méthodes = colnames(pc),
  MAE = MAE,
  MAPE = MAPE,
  DE = DE,
  CPS = CPS,
  RGME = RGME,
  cl_MAE = rank(MAE),
  cl_MAPE = rank(MAPE),
  cl_DE = rank(DE),
  cl_CPS = rank(CPS),
  cl_RGME = rank(desc(RGME)),
)

#Classement des différentes méthodes
df_prev$Classement_G <- rank((rowMeans(df_prev[,7:11])))
df_prev$Classement_G <- floor(df_prev$Classement_G)

#Visualisation
ggplot(data = df_prev) +
  geom_point(aes(x = Méthodes, y = cl_MAE, colour = "1"), shape = 8, size = 4, position = position_jitter(width = 0.1)) +  
  geom_point(aes(x = Méthodes, y = cl_MAPE, colour = "2"), shape = 8, size = 4, position = position_jitter(width = 0.1)) +  
  geom_point(aes(x = Méthodes, y = cl_DE, colour = "3"), shape = 8, size = 4, position = position_jitter(width = 0.1)) + 
  geom_point(aes(x = Méthodes, y = cl_CPS, colour = "4"), shape = 8, size = 4, position = position_jitter(width = 0.1)) + 
  geom_point(aes(x = Méthodes, y = cl_RGME, colour = "5"), shape = 8, size = 4, position = position_jitter(width = 0.1)) + 
  geom_point(aes(x = Méthodes, y = Classement_G, shape = "6"), col = "#00561b", size = 4) + 
  labs(x = "Méthodes", y = "Classement", 
       title = "Classement des différentes méthodes de prévisions") +  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 55, hjust = 1)) +  
  scale_y_continuous(breaks = seq(min(index(df_prev)), max(index(df_prev)), by = 1)) +
  scale_colour_manual(name = "Indicateur statistique",
                      values = c("1" = "#dc147c", "2" = "#9e0e40", "3" = "#357ab7", "4" = '#fee7f0', "5" = "#ccccff"), 
                      labels = c("MAE","MAPE","DE","CPS","RGME")) +
  scale_shape_manual(name = "Classement général des modèles",
                     values = c("6" = 16),
                     labels = c("Place"))





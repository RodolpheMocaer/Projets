#Prevision Analogues
#library
library(readxl)
library(forecast)
library(ggplot2)
library(e1071)
library(writexl)
library(dplyr)
library(car)
library(tseries)


#importation
Grippe_TSDBD <- read_excel("~/Desktop/BASE DE DONNEES USA/Grippe_TSDBD.xlsx")
#recadrage de la base de données 2004(673) - 2023(912)
grippe <- Grippe_TSDBD[c(673:912),]
#séparation des données à prédire (année 2023)
grippe_2023 <- grippe[c(229:240),]
grippe_donnée <- grippe[-c(229:240),]
str(grippe_2023)
#Mise en place des technique de prévision
#technique de prévision par la méthode des analogues
#selection du vecteur d'incidence actuel Xt
n <- 228
Yt <- grippe_donnée[(n-5):n, "ILITOTAL"]

#Vecteur d'incidence enlever du data frame
grippe_donnée_sans <- grippe_donnée[-c(223:228),]


#Calcule de la similarité de ce vecteur avec les vecteur historique de la série
plage <- 6 #plage du vecteur de 12o bs
distances <- numeric(length(grippe_donnée_sans$ILITOTAL) - plage + 1) #stockage des résultats
for (i in 1:(length(grippe_donnée_sans$ILITOTAL) - plage + 1)) {
  sous_serie <- grippe_donnée_sans$ILITOTAL[i:(i + plage - 1)]#Selection des sous série 
  distance <- sqrt(sum((Yt - sous_serie)^2)) #Calcle de la distance euclidienne entre Xt et la sous série
  distances[i] <- distance #Stockage de la distance
} #calcul de la distance euclidienne

#Sous-séries les plus similaire
indices_plus_similaires <- order(distances)[1:10]
# Initialiser une liste pour stocker les séries similaires
series_similaires <- list()
for (i in indices_plus_similaires) {
  grippe_serie_similaire <- grippe_donnée_sans$ILITOTAL[i:(i + plage - 1)]
  series_similaires[[length(series_similaires) + 1]] <- grippe_serie_similaire
}
similiratiré <- as.data.frame(do.call(cbind, series_similaires))

#Correspondance des vecteur et leurs date
nb_colonnes <- ncol(similiratiré)
lignes_correspondantes <- vector("list", nb_colonnes)
for (i in 1:nb_colonnes) {
  resultats <- grippe_donnée_sans$ILITOTAL %in% similiratiré[[i]]
  lignes_correspondantes[[i]] <- grippe_donnée_sans[resultats, "Date"][6,1]
}
date_sim <- t(as.data.frame(do.call(cbind, lignes_correspondantes)))
indices_lignes <- which(grippe_donnée_sans$Date %in% date_sim)


#Selection obs prévision
Incidence <- list()
for (i in 1:length(indices_lignes)) {
  indice_debut <- indices_lignes[i] + 1
  indice_fin <- indices_lignes[i] + 12
  Incidence[[i]] <- grippe_donnée_sans$ILITOTAL[seq(indice_debut, indice_fin)]
}
prévision <- as.data.frame(do.call(cbind, Incidence))

#Selection date
nb_colonnes <- ncol(prévision)
lignes_correspondantes2 <- vector("list", nb_colonnes)
for (i in 1:nb_colonnes) {
  resultats <- grippe_donnée_sans$ILITOTAL %in% prévision[[i]]
  lignes_correspondantes2[[i]] <- grippe_donnée_sans[resultats, "Date"][1,1]
}
date_sim <- as.data.frame(do.call(cbind, lignes_correspondantes2))
date_sim <- format(date_sim, "%Y-%m-%d")

#Prévision global (ensemble des 10 meilleurs)
prevision_tot <- (as.data.frame((prévision$V1 + prévision$V2 + prévision$V3 + prévision$V4
              + prévision$V5 + prévision$V6 + prévision$V7 + prévision$V8)/8))
colnames(prevision_tot) <- "prev_tot"

#Prévision 1,3,5
prevision_trois <- (as.data.frame((prévision$V1 + prévision$V2+ prévision$V3) /3))
colnames(prevision_trois) <- "prev_trois"

#Visualisation
#prévision_tot
graph <- ggplot(prevision_tot, aes(x = 1:length(prev_tot), y = prev_tot)) +
  geom_line(color = "#00561b") +
  geom_point(color = "#381a3c", size = 3) +
  labs(title = "",
       x = "Temps",      
       y = "Incidence") +        
  theme_minimal() +                
  theme(plot.title = element_text(hjust = 0.5)) 
print(graph)


#prévision_trois
graph <- ggplot(prevision_trois, aes(x = 1:length(prev_trois), y = prev_trois)) +
  geom_line(color = "#00561b") +
  geom_point(color = "#381a3c", size = 3) +
  labs(title = "",
       x = "Temps",      
       y = "Incidence") +        
  theme_minimal() +                
  theme(plot.title = element_text(hjust = 0.5)) 
print(graph)



#Autre Graphique
#Tot
mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", "Août", "Septembre", "Octobre", "Novembre", "Décembre")
ggplot() +
  geom_line(data = prevision_tot, aes(x = 1:12, y = prev_tot, linetype = "solid"), color = "#00561b", size = 1) +
  geom_point(data = prevision_tot, aes(x = 1:12, y = prev_tot), color = "#381a3c", size = 3) +
  geom_line(data = prévision, aes(x = 1:12, y = V1, color = "1"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V2, color = "2"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V3, color = "3"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V4, color = "4"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V5, color = "5"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V6, color = "6"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V7, color = "7"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V8, color = "8"), linetype = "dashed", alpha = 0.5) +
  labs(x = "Année 2023", y = "Incidences de la grippe", title = "") + 
  scale_x_continuous(breaks = 1:12, labels = mois) +
  scale_color_manual(name = "Données historique",
                     values = c("1" = "#fde9e0","2" = "#357ab7","3" = "#ac1e44","4" = "#ccccff",
                                "5" = "#22780f","6" = "#dc143c","7" = "#faf0e6","8" = "#1b4f08"), 
                     labels = c("Série analogue 1", "Série analogue 2", "Série analogue 3", "Série analogue 4",
                                "Série analogue 5", "Série analogue 6", "Série analogue 7", "Série analogue 8")) +
  scale_linetype_manual(name = "Données prévisionelle",
                        values = "solid",
                        labels = "Données prédites") +
  theme_minimal()

#Trois
ggplot() +
  geom_line(data = prevision_trois, aes(x = 1:12, y = prev_trois, linetype = "solid"), color = "#00561b", size = 1) +
  geom_point(data = prevision_trois, aes(x = 1:12, y = prev_trois), color = "#381a3c", size = 3)+
  geom_line(data = prévision, aes(x = 1:12, y = V1, color = "1"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V3, color = "2"), linetype = "dashed", alpha = 0.5) +
  geom_line(data = prévision, aes(x = 1:12, y = V4, color = "3"), linetype = "dashed", alpha = 0.5) +
  labs(x = "Année 2023", y = "Incidences de la grippe", title = "") + 
  scale_x_continuous(breaks = 1:12, labels = mois) +
  scale_color_manual(name = "Données historiques",
                     values = c("1" = "#fde9e0",
                                "2" = "#ac1e44",
                                "3" = "#ccccff"), 
                     labels = c("Série analogue 1", "Série analogue 3", "Série analogue 4")) +
  scale_linetype_manual(name = "Données prévisionelle",
                        values = "solid",
                        labels = "Données prédites") +
  theme_minimal()
  





#ajoute de sources
prevision_trois$source <- "analogue_prevision_trois"
prevision_trois$Date <- grippe_2023$Date
prevision_tot$source <- "analogue_prevision_tot"
prevision_tot$Date <- grippe_2023$Date
grippe_2023 <- grippe_2023[,c(1,3)]
grippe_2023$source <- "obs_grippe_2023"
# Renommer les colonnes pour les aligner
names(grippe_2023) <- c("Date", "Incidence", "source")
names(prevision_tot) <- c("Incidence", "source", "Date")
names(prevision_trois) <- c("Incidence", "source", "Date")

#Concaténation des bases prévison
Database_prev <- rbind(prevision_trois, prevision_tot, grippe_2023)
str(Database_prev)

#Exportation de la base de données
write_xlsx(Database_prev,"Database_prev.xlsx")





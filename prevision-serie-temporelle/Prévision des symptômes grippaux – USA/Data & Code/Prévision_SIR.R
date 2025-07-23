#library
library(dplyr)
library(readxl)
library(lubridate)
library(deSolve)
library(ggplot2)
library(pracma)

#importation donnée hebdomadaire
grippe <- read_excel("~/Desktop/BASE DE DONNEES USA/Grippe_hebdo_SIR.xlsx")
grippe <- grippe[c(328:1370),]
grippe_2023 <- grippe[c(992:1043),]
grippe_donnée <- grippe[-c(992:1043),]

#Nombre de patients / susceptibles#
grippe_donnée_patient <- aggregate(. ~ YEAR, data = grippe_donnée[,c(1,5)], FUN = sum, na.rm = TRUE)
#Calcule de l'évolution de chaque années#
diff_percentages <- numeric(nrow(grippe_donnée_patient))
for (i in 2:nrow(grippe_donnée_patient)) {
  diff_percentages[i] <- (grippe_donnée_patient[i, 2] - grippe_donnée_patient[i - 1, 2]) / grippe_donnée_patient[i - 1, 2] * 100
}
grippe_donnée_patient$Diff_Pourcentage <- diff_percentages
moyenne <- round(mean(grippe_donnée_patient$Diff_Pourcentage),3)
moyenne

#Sélection de la période voulu pour la prévision
Infecté_2022 <- subset(grippe_donnée, YEAR == 2022 & WEEK >= 31 & WEEK <= 52)
#Fonction pour effectuer l'interpolation / transformation de données hebdomadaire en mensuel
donnees_hebdomadaires <- Infecté_2022$ILITOTAL
interpolation_quotidienne <- function(donnees_hebdomadaires) {
  jours_semaine <- 7
  nombre_semaines <- length(donnees_hebdomadaires)
  #Initialisation du vecteur pour stocker les données quotidiennes
  donnees_quotidiennes <- numeric()
  #Pour chaque semaine
  for (i in 1:nombre_semaines) {
    #Donnée hebdomadaire
    donnee_hebdomadaire <- donnees_hebdomadaires[i]
    #Interpolation linéaire pour obtenir les données quotidiennes
    donnees_jour <- rep(0, jours_semaine)
    donnees_jour[1] <- round(runif(1, 0.3, 0.4) * donnee_hebdomadaire)
    for (j in 2:jours_semaine) {
      donnees_jour[j] <- round(runif(1, 0.6, 0.8) * donnee_hebdomadaire)
    }
    #Ajustement pour garantir que la somme des données quotidiennes est égale à la donnée hebdomadaire
    somme_donnees_jour <- mean(donnees_jour)
    rapport <- donnee_hebdomadaire / somme_donnees_jour
    donnees_jour <- round(donnees_jour * rapport)
    #Stockage des données quotidiennes
    donnees_quotidiennes <- c(donnees_quotidiennes, donnees_jour)
  }
  return(donnees_quotidiennes)
}
donnees_quotidiennes <- interpolation_quotidienne(donnees_hebdomadaires)
#Création de la dataframe
date_depart <- as.Date("2022-08-01")
nombre_jours <- length(donnees_quotidiennes)
dates <- seq.Date(from = date_depart, by = "day", length.out = nombre_jours)
grippe_quotidiennes <- as.data.frame(donnees_quotidiennes)
grippe_quotidiennes$Date <- dates

#Le modéle SIR#
SIR <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dI <- beta * S * I / N - gamma * I
    dR <- gamma * I
    return(list(c(dS, dI, dR)))
  })
}

#Initialisation des paramétres
N <- 333000000
#Calcule du nombres d'infecté total et non juste des symptomatique
Infecté_s <- grippe_quotidiennes$donnees_quotidiennes[1]
Infecté_tot <- round((100*Infecté_s)/33,0)
Infeté_as <- round((Infecté_tot*0.77),0)
Jour <- 1:365

init <- c(
  S = N - Infecté_tot[1],
  I = Infecté_tot[1],
  R = 0
)

#Ajustement du modéle 
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Jour, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((grippe_quotidiennes$donnees_quotidiennes - fit)^2)
}

#détermination des paramétres de maniére mathématiquement#
Opt <- optim(par = c(beta = 0.1, gamma = 0.1), fn = RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))
Opt_par <- setNames(Opt$par, c("beta", "gamma"))
Opt_par
t <- 1:365

#détermination des paramétres de maniére mathématiquement#
Opt_par <- setNames(c(0.5141,0.485), c("beta", "gamma"))
#Calcule à base des données cliniques
SIR_juillet <- data.frame(ode(
  y = init, times = t,
  func = SIR, parms = Opt_par
))
max(SIR_juillet$I*0.33)
max(SIR_juillet$I)

#Changement de base de donnnée
SIR_juillet_infectieux <- SIR_juillet
SIR_juillet_infectieux$I <- SIR_juillet$I*0.33
SIR_juillet_infectieux_154 <- head(SIR_juillet_infectieux, 154)
head(SIR_juillet$I,100)
# Créer votre graphique ggplot avec les données estimées
SIR_juillet_infectieux_154  |> 
  ggplot(aes(x = 1:154)) +
  geom_line(aes(y = I, colour = "1")) +  
  labs(y = "Nombre de cas", x = "Temps (en jours)", title = "") +
  geom_vline(xintercept = which.max(SIR_juillet_infectieux_154$I), colour = "#00561b") +
  geom_vline(xintercept = which.max(SIR_juillet$I), colour = "#00561b") +
  theme_minimal() + 
  geom_point(data = grippe_quotidiennes, aes(x = 1:154, y = donnees_quotidiennes, shape = "3"), colour = "#CCCCFF") +
  geom_vline(xintercept = which.max(grippe_quotidiennes$donnees_quotidiennes), colour = "#CCCCFF") +
  scale_colour_manual(name = "Données prévisionnel de août 2022 à juillet 2023",
                      values = c("1" = "#00561b"),
                      labels = c("Personnes infectés, infectieuses et symptomatique")) + 
  scale_shape_manual(name = "Données historique de août 2022 à juillet 2023",
                     values = c("3" = 19),
                     labels = c("Personne infecté et symptomatique"))

(0.527 - 0.5141)/0.5141 * 100 #Une augmentation de 2.45%

# Tracer le graphique avec ggplot clinique
SIR_juillet_infectieux |> 
  ggplot(aes(x = t)) +
  geom_line(aes(y = I), colour = "#00561b") +  
  labs(y = "Nombre de cas", title = "COVID-19 Estimé") +
  scale_colour_manual(name = "", values = c("#00561b" = "#00561b"), labels = c("Estimé")) +
  theme_minimal()

#Modélisation de la suite de l'année 2023
Nouvelle_incidence <- (SIR_juillet$I[365])
Nouvelle_incidence <- Nouvelle_incidence + (Nouvelle_incidence*12.928)/100
Nouvelle_incidence
#Nous augmentons de 12.928 la nouvelle incidence et le R0 de la nouvelle vague
Opt_par <- setNames(c(0.527,0.485), c("beta", "gamma"))
init2 <- c(
  S = N - Nouvelle_incidence,
  I = Nouvelle_incidence,
  R = 0
)

#Calcule à base des données cliniques
t2 <- 1:153
SIR_décembre <- data.frame(ode(
  y = init2, times = t2,
  func = SIR, parms = Opt_par
))

#Changement de base de donnnée
SIR_décembre_infectieux <- SIR_décembre
SIR_décembre_infectieux$I <- SIR_décembre$I*0.33

# Tracer le graphique avec ggplot clinique
SIR_décembre_infectieux |> 
  ggplot(aes(x = t2)) +
  geom_line(aes(y = I), colour = "#00561b") +  
  labs(y = "Nombre de cas", title = "COVID-19 Estimé") +
  scale_colour_manual(name = "", values = c("#00561b" = "#00561b"), labels = c("Estimé")) +
  theme_minimal()

juillet <- as.data.frame(SIR_juillet_infectieux[153:364,c(1,3)])
décembre <- as.data.frame(SIR_décembre_infectieux[,c(1,3)])
Prev_SIR_2023 <- rbind(juillet, décembre)

#Résultat mensuel#
jours_par_mois <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
mois <- rep(1:12, jours_par_mois)
Prev_SIR_2023$mois <- factor(mois, levels = unique(mois))
Prev_SIR_2023 <- Prev_SIR_2023 %>%
  group_by(mois) %>%
  summarise(
    I = round(mean(I), 0)
  )

#visualisation
mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", 
          "Août", "Septembre", "Octobre", "Novembre", "Décembre")
ggplot(Prev_SIR_2023, aes(x = 1:length(I), y = I)) +
  geom_line(aes(color = "1"),size = 1) +
  geom_point(color = "#381a3c", size = 3) +
  labs(title = "", x = "Temps (en jours)", y = "Nombre de cas") +   
  theme_minimal() + 
  scale_x_continuous(breaks = 1:12, labels = mois) +
  scale_colour_manual(name = "Données prévisionnel par la méthode SIR de l'année 2023",
                      values = c("1" = "#00561b"), 
                      labels = c("Personnes infectés, infectieuses et symptomatique", "Personne uniquement infecté"))


#Information plus de l'évolution de l'épidémies
#R0
R0_SIR <- ((0.5141/0.485)+(0.527/0.485))/2
Infecté_symptomatique <- (sum(juillet$I) + sum(décembre$I))
Retiré <- (max(SIR_juillet$R) + max(SIR_décembre$R))/2









#Mise en place d'un nouveau modéle par compartiment
#Le modéle SEIRD
SEIRD <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N
    dE <- beta * S * I / N - E*zeta
    dI <- E*zeta - gamma * I - rho * I
    dR <- gamma * I
    dD <- rho * I
    return(list(c(dS, dE, dI, dR, dD)))
  })
}

#Initialisation des paramétres
init <- c(
  S = N - Infecté_tot[1],
  E = Infecté_tot[1],
  I = Infecté_tot[1],
  R = 0,
  D = 0
)
#Supposition qu'il y ai autaut d'exposé que d'infecté à t = 0

#Ajustement du modéle 
RSS2 <- function(parameters) {
  names(parameters) <- c("beta", "zeta", "gamma", "rho")
  out <- ode(y = init, times = Jour, func = SEIRD, parms = parameters)
  fit <- out[,4]
  sum((grippe_quotidiennes$donnees_quotidiennes - fit)^2)
}

#détermination des paramétres de maniére mathématiquement#
Opt <- optim(par = c(beta = 0.1, gamma = 0.1, zeta = 0.1, rho = 0.1), fn = RSS2, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1))
Opt_par <- setNames(Opt$par, c("beta", "gamma", "zeta", "rho"))
Opt_par

#détermination des paramétres de maniére mathématiquement#
Opt_par <- setNames(c(0.75,0.95,0.62,0.069), c("beta", "zeta","gamma","rho"))
t <- 1:365

Opt_par <- setNames(c(0.73,0.5,0.5626484,0.06251649), c("beta", "zeta","gamma","rho"))

0.61/(0.48+0.05)
0.75/1.150943
(9*0.6516396)/10
Opt_par <- setNames(c(0.58,0.5,0.465,0.065), c("beta", "zeta","gamma","rho")) #Bon nombre de cas
Opt_par <- setNames(c(0.61,0.5,0.465,0.065), c("beta", "zeta","gamma","rho")) #Bon pics épidémique
Opt_par <- setNames(c(0.75,0.5,0.601,0.073), c("beta", "zeta","gamma","rho")) #Bon pics épidémique & bon nombre de cas


#Calcule à base des données cliniques
SEIRD_juillet <- data.frame(ode(
  y = init, times = t,
  func = SEIRD, parms = Opt_par
))
which.max(SEIRD_juillet$E)
which.max(SEIRD_juillet$I)
which.max(SIR_juillet$I)
SEIRD_juillet[365,]
SEIRD_juillet[1,]
par(mfrow=c(1,2))
#Changement de base de donnnée
SEIRD_juillet_infectieux <- SEIRD_juillet
SEIRD_juillet_infectieux$I <- SEIRD_juillet$I*0.33
SEIRD_juillet_infectieux_154 <- SEIRD_juillet_infectieux[1:154,]

plot(SIR_juillet_infectieux$I)
plot(SEIRD_juillet_infectieux$I)


#visualisation graphique
SEIRD_juillet_infectieux_154 |> 
  ggplot(aes(x = 1:154)) +
  geom_line(aes(y = E, colour = "1")) +
  geom_vline(xintercept = which.max(SEIRD_juillet$E), colour = "#357ab7") +
  geom_line(aes(y=I, colour = "2")) +
  geom_vline(xintercept = which.max(SEIRD_juillet$I), colour = "#00561b") +
  labs(y = "Nombre de cas", x = "Temps (en jours)", title = "") +
  theme_minimal() +
  geom_point(data = grippe_quotidiennes, aes(x = 1:154, y = donnees_quotidiennes, shape = "3"), colour = "#CCCCFF") +
  geom_vline(xintercept = which.max(grippe_quotidiennes$donnees_quotidiennes), colour = "#CCCCFF") +
  scale_colour_manual(name = "Données prévisionnel de août 2022 à juillet 2023",
                      values = c("1" = "#357ab7", "2" = "#00561b"), 
                      labels = c("Personne uniquement infecté","Personnes infectés, infectieuses et symptomatique")) +
  scale_shape_manual(name = "Données historique de août 2022 à juillet 2023",
                     values = c("3" = 19),
                     labels = c("Personne infecté et symptomatique"))


#Visualisation entiere
ggplot(SEIRD_juillet_infectieux, aes(x = t)) +
  geom_line(aes(y = I, colour = "Personnes infectés et infectieuses et symptomatique")) +
  geom_line(aes(y = E, colour = "Personne uniquement infecté")) +  
  labs(y = "Nombre de cas",x = "Temps (en jours)", title = "") +
  scale_colour_manual(name = "",
                      values = c("Personnes infectés et infectieuses et symptomatique" = "#00561b", "Personne uniquement infecté" = "#357ab7"))+
  theme_minimal()

#Modélisation de la suite de l'année 2023
new_incidence <- (SEIRD_juillet$I[365])
new_incidence <- new_incidence + (new_incidence*12.928)/100
new_incidence
new_expose <- (SEIRD_juillet$E[365])
new_expose <- new_expose + (new_expose*12.928)/100
new_expose
#Nous augmentons de 2 points la nouvelle incidence et le R0 de la nouvelle vague
Opt_par <- setNames(c(0.77,0.5,0.601,0.073), c("beta", "zeta","gamma","rho"))
init2 <- c(
  S = N - new_incidence[1],
  E = new_expose[1],
  I = new_incidence[1],
  R = 0,
  D = 0
)
#Calcule à base des données cliniques
t2 <- 1:153
SEIRD_décembre <- data.frame(ode(
  y = init2, times = t2,
  func = SEIRD, parms = Opt_par
))

#Changement de base de donnnée
SEIRD_décembre_infectieux <- SEIRD_décembre
SEIRD_décembre_infectieux$I <- SEIRD_décembre$I*0.33

#Visualisation entiere
ggplot(SEIRD_décembre_infectieux, aes(x = t2)) +
  geom_line(aes(y = I, colour = "Personnes infectés et infectieuses et symptomatique")) +
  geom_line(aes(y = E, colour = "Personne uniquement infecté")) +  
  labs(y = "Nombre de cas",x = "Temps (en jours)", title = "") +
  scale_colour_manual(name = "",
                      values = c("Personnes infectés et infectieuses et symptomatique" = "#00561b", "Personne uniquement infecté" = "#357ab7"))+
  theme_minimal()

#Création base de données
juillet2 <- as.data.frame(SEIRD_juillet_infectieux[153:364,c(1,3,4)])
décembre2 <- as.data.frame(SEIRD_décembre_infectieux[,c(1,3,4)])
Prev_SEIRD_2023 <- rbind(juillet2, décembre2)

#Résultat mensuel#
jours_par_mois <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
mois <- rep(1:12, jours_par_mois)
Prev_SEIRD_2023$mois <- factor(mois, levels = unique(mois))
Prev_SEIRD_2023 <- Prev_SEIRD_2023 %>%
  group_by(mois) %>%
  summarise(
    I = round(mean(I), 0),
    E = round(mean(E),0)
  )

#visualisation
mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", 
          "Août", "Septembre", "Octobre", "Novembre", "Décembre")
ggplot(Prev_SEIRD_2023, aes(x = 1:length(I), y = I)) +
  geom_line(aes(color = "1"),size = 1) +
  geom_point(color = "#381a3c", size = 3) +
  labs(title = "", x = "Année 2023", y = "Nombre de cas") +   
  theme_minimal() + 
  scale_x_continuous(breaks = 1:12, labels = mois) +
  scale_colour_manual(name = "Données prévisionnel par la méthode SIR de l'année 2023",
                      values = c("1" = "#00561b"), 
                      labels = c("Personnes infectés, infectieuses et symptomatique", "Personne uniquement infecté"))


#Information plus de l'évolution de l'épidémies
#R0
R0_SEIRD <- ((0.75/(0.601 + 0.069))+(0.77/(0.601 + 0.073)))/2
Infecté_symptomatique2 <- (sum(juillet2$I) + sum(décembre2$I))
Contaminé2 <- (sum(juillet2$E) + sum(décembre2$E))
Décédes <- max(SEIRD_juillet$D) + max(SEIRD_décembre$D)
Rétablis <- max(SEIRD_juillet$R) + max(SEIRD_décembre$R)
Retiré <- Décédes + Rétablis



















#Mise en place d'un nouveau modéle par compartiment
SEIRDV <- function(times, state, parameters) {
  with(as.list(c(state, parameters)), {
    dS <- -beta * S * I / N - alpha * V / N
    dV <- alpha * V / N - beta * kappa * V * I / N
    dE <- beta * S * I / N + beta * kappa * V * I / N - E * zeta
    dI <- E*zeta - gamma * I - rho * I
    dR <- gamma * I
    dD <- rho * I
    return(list(c(dS, dV, dE, dI, dR, dD)))
  })
}

#Initialisation des paramétres
#Calcule du nombre de personne vacciné
Vacciné <- (N - Infecté_tot)*(46.9/100)
init3 <- c(
  S = N - Infecté_tot[1] - Vacciné[1],
  V = Vacciné[1],
  E = Infecté_tot[1],
  I = Infecté_tot[1],
  R = 0,
  D = 0
)
#Nous supposons autant d'infecté que de d'exposé à t = 0

#Ajustement du modéle 
Jour <- 1:365
RSS3 <- function(parameters) {
  names(parameters) <- c("beta", "zeta", "gamma", "rho", "alpha", "kappa")
  out <- ode(y = init3, times = Jour, func = SEIRDV, parms = parameters)
  fit <- out[, 5]
  sum((grippe_quotidiennes$donnees_quotidiennes - fit)^2)
}


#détermination des paramétres de maniére mathématiquement#
Opt <- optim(par = c(beta = 0.1, gamma = 0.1, zeta = 0.1, rho = 0.0001, alpha = 0.0001, kappa = 0.1)
             , fn = RSS3, method = "L-BFGS-B", lower = c(0.1,1/14,1/7,0.0001,0.0001,0.1), upper = c(1,1/7,1,0.001,0.001,0.3))
Opt_par <- setNames(Opt$par, c("beta", "gamma", "zeta", "rho", "alpha", "kappa"))
Opt_par

#détermination des paramétres de maniére mathématiquement#
Opt_par <- setNames(c(0.7428798928,0.5,0.0996263496,0.01,0.0001000291,0.4956515907), c("beta", "zeta","gamma","rho","alpha","kappa"))
Opt_par <- setNames(c(0.70,0.5,0.29,0.023,0.5,0.007), c("beta", "zeta","gamma","rho","alpha","kappa")) #Bon nombre de pics 




Opt_par <- setNames(c(0.75,0.5,0.32,0.03,0.5,0.002), c("beta", "zeta","gamma","rho","alpha","kappa")) #Bon nombre de cas 

#Calcule à base des données cliniques
SEIRDV_juillet <- data.frame(ode(
  y = init3, times = t,
  func = SEIRDV, parms = Opt_par

))
which.max(SEIRDV_juillet$I)
which.max(grippe_quotidiennes$donnees_quotidiennes)

SEIRDV_juillet[365,]
SEIRDV_juillet[1,]

par(mfrow=c(1,2))
#Changement de base de donnnée
SEIRDV_juillet_infectieux <- SEIRDV_juillet
SEIRDV_juillet_infectieux$I <- SEIRDV_juillet$I*0.33
SEIRDV_juillet_infectieux_154 <- SEIRDV_juillet_infectieux[1:154,]

plot(SIR_juillet_infectieux$I)
plot(SEIRDV_juillet_infectieux$I)

#visualisation graphique
SEIRDV_juillet_infectieux_154 |> 
  ggplot(aes(x = 1:154)) +
  geom_line(aes(y = E, colour = "1")) +
  geom_vline(xintercept = which.max(SEIRDV_juillet_infectieux_154$E), colour = "#357ab7") +
  geom_line(aes(y=I, colour = "2")) +
  geom_vline(xintercept = which.max(SEIRDV_juillet_infectieux_154$I), colour = "#00561b") +
  labs(y = "Nombre de cas", x = "Temps (en jours)", title = "") +
  theme_minimal() +
  geom_point(data = grippe_quotidiennes, aes(x = 1:154, y = donnees_quotidiennes, shape = "3"), colour = "#CCCCFF") +
  geom_vline(xintercept = which.max(grippe_quotidiennes$donnees_quotidiennes), colour = "#CCCCFF") +
  scale_colour_manual(name = "Données prévisionnel de août 2022 à juillet 2023",
                      values = c("1" = "#357ab7", "2" = "#00561b"), 
                      labels = c("Personne uniquement infecté","Personnes infectés, infectieuses et symptomatique")) +
  scale_shape_manual(name = "Données historique de août 2022 à juillet 2023",
                     values = c("3" = 19),
                     labels = c("Personne infecté et symptomatique"))

#Visualisation entiere
ggplot(SEIRDV_juillet_infectieux, aes(x = t)) +
  geom_line(aes(y = I, colour = "Personnes infectés et infectieuses et symptomatique")) +
  geom_line(aes(y = E, colour = "Personne uniquement infecté")) +  
  labs(y = "Nombre de cas",x = "Temps (en jours)", title = "") +
  scale_colour_manual(name = "",
                      values = c("Personnes infectés et infectieuses et symptomatique" = "#00561b", "Personne uniquement infecté" = "#357ab7"))+
  theme_minimal()

#Modélisation de la suite de l'année 2023
new_incidence2 <- (SEIRDV_juillet_infectieux$I[365])
new_incidence2 <- new_incidence2 + (new_incidence2*12.928)/100
new_incidence2
new_expose2 <- (SEIRDV_juillet_infectieux$E[365])
new_expose2 <- new_expose2 + (new_expose2*12.928)/100
new_expose2
new_vacciné <- SEIRDV_juillet_infectieux$V[365]
new_vacciné
#Nous augmentons de 2 points la nouvelle incidence et le R0 de la nouvelle vague
Opt_par <- setNames(c(0.77,0.5,0.32,0.03,0.5,0.002), c("beta", "zeta","gamma","rho","alpha","kappa"))
init3 <- c(
  S = N - new_incidence2[1] - new_vacciné[1],
  V = new_vacciné[1],
  E = new_expose2[1],
  I = new_incidence2[1],
  R = 0,
  D = 0
)
#Calcule à base des données cliniques
t3 <- 1:153
SEIRDV_décembre <- data.frame(ode(
  y = init3, times = t3,
  func = SEIRDV, parms = Opt_par
))

#Changement de base de donnnée
SEIRDV_décembre_infectieux <- SEIRDV_décembre
SEIRDV_décembre_infectieux$I <- SEIRDV_décembre$I*0.33

#Visualisation entiere
ggplot(SEIRDV_décembre_infectieux, aes(x = t2)) +
  geom_line(aes(y = I, colour = "Personnes infectés et infectieuses et symptomatique")) +
  geom_line(aes(y = E, colour = "Personne uniquement infecté")) +  
  labs(y = "Nombre de cas",x = "Temps (en jours)", title = "") +
  scale_colour_manual(name = "",
                      values = c("Personnes infectés et infectieuses et symptomatique" = "#00561b", "Personne uniquement infecté" = "#357ab7"))+
  theme_minimal()

#Création base de données
head(SEIRDV_juillet_infectieux)
head(SEIRDV_décembre_infectieux)
juillet3 <- as.data.frame(SEIRDV_juillet_infectieux[153:364,c(1,4,5)])
décembre3 <- as.data.frame(SEIRDV_décembre_infectieux[,c(1,4,5)])
Prev_SEIRDV_2023 <- rbind(juillet3, décembre3)

#Résultat mensuel#
jours_par_mois <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
mois <- rep(1:12, jours_par_mois)
Prev_SEIRDV_2023$mois <- factor(mois, levels = unique(mois))
Prev_SEIRDV_2023 <- Prev_SEIRDV_2023 %>%
  group_by(mois) %>%
  summarise(
    I = round(mean(I), 0),
    E = round(mean(E),0)
  )

#visualisation
mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", 
          "Août", "Septembre", "Octobre", "Novembre", "Décembre")
ggplot(Prev_SEIRDV_2023, aes(x = 1:length(I), y = I)) +
  geom_line(aes(color = "1"),size = 1) +
  geom_point(color = "#381a3c", size = 3) +
  labs(title = "", x = "Année 2023", y = "Nombre de cas") +   
  theme_minimal() + 
  scale_x_continuous(breaks = 1:12, labels = mois) +
  scale_colour_manual(name = "Données prévisionnel par la méthode SIR de l'année 2023",
                      values = c("1" = "#00561b"), 
                      labels = c("Personnes infectés, infectieuses et symptomatique", "Personne uniquement infecté"))


#Information plus de l'évolution de l'épidémies
#R0
R0_SEIRD <- ((0.75/(0.32 + 0.03))+(0.77/(0.32 + 0.03)))/2
Infecté_symptomatique3 <- (sum(juillet3$I) + sum(décembre3$I))
Contaminé3 <- (sum(juillet3$E) + sum(décembre3$E))
Décédes2 <- max(SEIRDV_juillet$D) + max(SEIRDV_décembre$D)
Rétablis2 <- max(SEIRDV_juillet$R) + max(SEIRDV_décembre$R)
Retiré2 <- Décédes2 + Rétablis2









#Exportation des résultats
#importation base de données#
autre <- read_excel("/Users/rododo/Desktop/BASE DE DONNEES USA/Database_prev.xlsx")
Grippe_TSDBD <- read_excel("~/Desktop/BASE DE DONNEES USA/Grippe_TSDBD.xlsx")
grippe <- Grippe_TSDBD[c(673:912),]
grippe_2023 <- grippe[c(229:240),]

#Bon typage des données SIR#
Prev_SIR_2023 <- Prev_SIR_2023[,2]
Prev_SIR_2023$source <- "SIR_prevision"
Prev_SIR_2023$Date <- grippe_2023$Date
names(Prev_SIR_2023) <- c("Incidence", "source", "Date")
str(Prev_SIR_2023)

#Bon typage des données SEIRD#
Prev_SEIRD_2023 <- Prev_SEIRD_2023[,2]
Prev_SEIRD_2023$source <- "SEIRD_prevision"
Prev_SEIRD_2023$Date <- grippe_2023$Date
names(Prev_SEIRD_2023) <- c("Incidence", "source", "Date")
str(Prev_SEIRD_2023)

#Bon typage des données SEIRDV#
Prev_SEIRDV_2023 <- Prev_SEIRDV_2023[,2]
Prev_SEIRDV_2023$source <- "SEIRDV_prevision"
Prev_SEIRDV_2023$Date <- grippe_2023$Date
names(Prev_SEIRDV_2023) <- c("Incidence", "source", "Date")
str(Prev_SEIRDV_2023)

#Concaténation
Database_prev <- rbind(autre, Prev_SIR_2023, Prev_SEIRD_2023, Prev_SEIRDV_2023)
write_xlsx(Database_prev,"Database_prev.xlsx")

R0_SEIRD

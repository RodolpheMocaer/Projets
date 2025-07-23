#library 
library(readxl)
library(tseries)
library(seastests)
library(seasonal)

#importation
Grippe_TSDBD <- read_excel("~/Desktop/BASE DE DONNEES USA/Grippe_TSDBD.xlsx")
#recadrage de la base de données 2004(673) - 2023(912)
grippe <- Grippe_TSDBD[c(673:912),]
#séparation des données à prédire (année 2023)
grippe_2023 <- grippe[c(229:240),]
grippe_donnée <- grippe[-c(229:240),]

#univariées
#test série temporelle
yt <- grippe_donnée$ILITOTAL
#stationnarité
adf_test <- adf.test(yt)
print(adf_test)
#La série est donc stationnaire

#saisonnalité
yy <- ts(data = yt, start=c(2004,01),frequency=12)
# Friedman test
ft <- fried(yy)
show(ft)
# Kruskal-Wallis test
kwt <- kw(yy)
show(kwt)
# QS test
qst <- qs(yy)
show(qst)
# Webel-Ollech test
# Webel-Ollech test - new version of seastests (2021-09)
wot <- combined_test(yy)
show(wot)
#Toutes les P-value sont inférieurs à 1% ainsi, nous rejetons l'hypothése 
#d'absence de saisonnalité

#Spé additive ou multiplicative
seasX <- seas(yy)
out(seasX)
#une préférence pour une non transformation




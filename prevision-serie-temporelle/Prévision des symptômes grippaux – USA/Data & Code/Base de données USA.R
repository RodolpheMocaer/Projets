#Construction Base de données USA#
#library
library(readr)
library(dplyr)
library(stringr)
library(zoo)
library(readxl)
library(writexl)

#Grippe - Y
#importation
Y <- read_csv("~/Desktop/BASE DE DONNEES USA/Grippe/ILINet.csv", skip = 1, col_types = cols())
Y <- Y[,c(3,4,5,13)]
Y <- Y |> 
  mutate(WEEK = str_pad(WEEK, width = 2, pad = "0"))

Y <- read_csv("~/Desktop/BASE DE DONNEES USA/Grippe/ILINet.csv", skip = 1, col_types = cols())
Y2 <- Y[,c(3,4,5,13,15)]
Y2 <- Y2 |> 
  mutate(WEEK = str_pad(WEEK, width = 2, pad = "0"))
write_xlsx(Y2,"Grippe_hebdo_SIR.xlsx")


#mensualisation de la colonnes date de la base de données
Y$Date <- paste0(Y$YEAR, Y$WEEK)
Y$Date <- as.Date(paste0(Y$Date, "1"), format = "%Y%W%u")
Y$Date <- format(Y$Date, "%Y%m")
Y$Date <- na.locf(Y$Date)

#Bon typage des données
Y$Date <- paste0(substr(Y$Date, 1, 4), "-", substr(Y$Date, 5, 6))
Y$Date <- as.Date(paste0(Y$Date, "-01"), format = "%Y-%m-%d")
Y$Date <- as.POSIXct(Y$Date)
str(Y)

#Moyenne par mois
Y <- Y[,c(3:5)]
Y <- aggregate(. ~ Date, data = Y, FUN = mean, na.rm = TRUE)


#Grippe - Test - Total
#importation des bases de données
Grippe1 <- read_csv("~/Desktop/BASE DE DONNEES USA/Grippe/WHO_NREVSS_Public_Health_Labs.csv", skip = 1, col_types = cols())
Grippe2 <- read_csv("~/Desktop/BASE DE DONNEES USA/Grippe/WHO_NREVSS_Combined_prior_to_2015_16.csv", skip = 1, col_types = cols())
Grippe <- bind_rows(Grippe2, Grippe1)
somme_colonnes <- rowSums(Grippe[, 7:15], na.rm = TRUE)
Grippe$'%positif_test' <- (somme_colonnes/Grippe$`TOTAL SPECIMENS`)*100
Grippe <- Grippe[,c(3,4,16)]
Grippe <- Grippe |> 
  mutate(WEEK = str_pad(WEEK, width = 2, pad = "0"))


#mensualisation de la colonnes date de la base de données
Grippe$Date <- paste0(Grippe$YEAR, Grippe$WEEK)
Grippe$Date <- as.Date(paste0(Grippe$Date, "1"), format = "%Y%W%u")
Grippe$Date <- format(Grippe$Date, "%Y%m")
Grippe$Date <- na.locf(Grippe$Date)

#Bon typage des données
Grippe$Date <- paste0(substr(Grippe$Date, 1, 4), "-", substr(Grippe$Date, 5, 6))
Grippe$Date <- as.Date(paste0(Grippe$Date, "-01"), format = "%Y-%m-%d")
Grippe$Date <- as.POSIXct(Grippe$Date)
str(Grippe)

#Moyenne par mois
Grippe <- Grippe[,c(3:4)]
Grippe <- aggregate(. ~ Date, data = Grippe, FUN = mean, na.rm = TRUE)
#Attention séries non ajustée

#Variables indépendante
#Demographie
#importation
Pop_plus_16 <- read_excel("~/Desktop/BASE DE DONNEES USA/Demographie/CNP16OV.xls",skip = 10)
Pop_tot <- read_excel("~/Desktop/BASE DE DONNEES USA/Demographie/POPTHM.xls",skip = 10)
Pop_plus_55 <- read_excel("~/Desktop/BASE DE DONNEES USA/Demographie/LNU00024230.xls",skip = 10)

#Concaténation des bases
Pop <- merge(Pop_plus_16, Pop_tot, by = "observation_date", all = TRUE)
Pop <- merge(Pop, Pop_plus_55, by = "observation_date", all = TRUE)
colnames(Pop)

#Supprésion des NA
Pop <- na.omit(Pop)
Pop$Pop_moins_16_ans <- Pop$POPTHM - Pop$CNP16OV

#Renommage des colonnes 
Pop <- Pop |> 
  rename( Date = observation_date) |> 
  rename(Pop_globale = "POPTHM") |> 
  rename(Pop_plus_55_ans = "LNU00024230")
Pop <- Pop[,-c(2)]
str(Pop)
#Attention séries non ajustée

#Indice de prix
Medical_care_IP <- read_excel("~/Desktop/BASE DE DONNEES USA/Indice de prix/CPIEMEDCARE.xls",skip = 10)
Transportation_IP <- read_excel("~/Desktop/BASE DE DONNEES USA/Indice de prix/CPIETRANS.xls",skip = 10)
Median_IP <- read_excel("~/Desktop/BASE DE DONNEES USA/Indice de prix/MEDCPIM158SFRBCLE.xls",skip = 10)

#Bon typage des données
Medical_care_IP$observation_date <- as.POSIXct(Medical_care_IP$observation_date)
Transportation_IP$observation_date <- as.POSIXct(Transportation_IP$observation_date)
Median_IP$observation_date <- as.POSIXct(Median_IP$observation_date)

#Concaténation
IP <- merge(Medical_care_IP, Transportation_IP, by = "observation_date", all = TRUE)
IP <- merge(IP, Median_IP, by = "observation_date", all = TRUE)
colnames(IP)

#Renommage des colonnes 
IP <- IP |> 
  rename( Date = observation_date) |> 
  rename(Medical_care = "CPIEMEDCARE") |> 
  rename(Transportation = "CPIETRANS") |> 
  rename(Médian = "MEDCPIM158SFRBCLE")
IP <- na.omit(IP)
#Série ajustée

#Météorologie
#Importation
Météo <- read_csv("~/Desktop/BASE DE DONNEES USA/Température/USW00013895.csv")

#Selection des colonnes
Météo <- Météo[, c('DATE','PRCP', 'TMAX', 'TMIN')]
Météo$Mean <- ((Météo$TMAX + Météo$TMIN)/2)/10
Météo <- Météo[,c(1,2,5)]

#Selection des lignes
Météo <- Météo[c(0:27862),]
Météo <- na.omit(Météo)

#Moyenne par mois
Météo$DATE <- format(Météo$DATE, "%Y-%m")
Météo <- aggregate(. ~ DATE, data = Météo, FUN = mean, na.rm = TRUE)

#Bon typage des données
Météo$DATE <- as.Date(paste0(Météo$DATE, "-01"), format = "%Y-%m-%d")
Météo$Date <- as.POSIXct(Météo$DATE)
Météo <- Météo[,-1]
str(Météo)
#Attention séries non ajustée

#Transport
#Importation
Transport_avion <- read_excel("~/Desktop/BASE DE DONNEES USA/Transport /AIRRPMTSID11.xls",skip = 10)
Transport_train <- read_excel("~/Desktop/BASE DE DONNEES USA/Transport /RAILPMD11.xls",skip = 10)
Transport_voitures_km <- read_excel("~/Desktop/BASE DE DONNEES USA/Transport /TRANSITD11.xls",skip = 10)
Transport_public_transport <- read_excel("~/Desktop/BASE DE DONNEES USA/Transport /TRFVOLUSM227SFWA.xls",skip = 10)

#Concaténation
Transport <- merge(Transport_avion, Transport_train, by = "observation_date", all = TRUE)
Transport <- merge(Transport, Transport_voitures_km, by = "observation_date", all = TRUE)
Transport <- merge(Transport, Transport_public_transport, by = "observation_date", all = TRUE)
colnames(Transport)

#Renommage des colonnes
Transport <- Transport |> 
  rename(Date = observation_date) |> 
  rename(Avion = "AIRRPMTSID11") |> 
  rename(Train = "RAILPMD11") |> 
  rename(Voiture = "TRANSITD11") |> 
  rename(Commun = "TRFVOLUSM227SFWA")
Transport <- na.omit(Transport)

#Bon typage des données
Transport$Date <- as.POSIXct(Transport$Date)
str(Transport)
#Série ajustée


#Economie
#Importation des données
BBKCI <- read_excel("~/Desktop/BASE DE DONNEES USA/Economie/BBKMCOIX.xls", skip = 10)
BBKGDP <- read_excel("~/Desktop/BASE DE DONNEES USA/Economie/BBKMGDP.xls", skip = 10)
BBKF <- read_excel("~/Desktop/BASE DE DONNEES USA/Economie/BBKMLEIX.xls", skip = 10)

#Concaténation
Economic <- merge(BBKCI, BBKGDP, by = "observation_date", all = TRUE)
Economic <- merge(Economic, BBKF, by = "observation_date", all = TRUE)
colnames(Economic)

#Renommage des colonnes
Economic <- Economic |> 
  rename(Date = observation_date) |> 
  rename(BBKCI = "BBKMCOIX") |> 
  rename(BBKGDP = "BBKMGDP") |> 
  rename(BBKF = "BBKMLEIX")
Economic <- na.omit(Economic)

#Bon typage des données
Economic$Date <- as.POSIXct(Economic$Date)
str(Economic)
#Série ajustée

#Google trends
#Symptomes de la grippe
#importation
Cough <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends/multiTimeline.csv",skip = 1)
Fever <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends/multiTimeline-2.csv",skip = 1)
Sore_throat <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends/multiTimeline-3.csv",skip = 1)
Aches <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends/multiTimeline-4.csv",skip = 1)
Fatigue <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends/multiTimeline-5.csv",skip = 1)
Headache <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends/multiTimeline-6.csv",skip = 1)
Runny_nose <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends/multiTimeline-7.csv",skip = 1)

#Concaténation
Symptomes <- merge(Cough, Fever, by = "Mois", all = TRUE)
Symptomes <- merge(Symptomes, Sore_throat, by = "Mois", all = TRUE)
Symptomes <- merge(Symptomes, Aches, by = "Mois", all = TRUE)
Symptomes <- merge(Symptomes, Fatigue, by = "Mois", all = TRUE)
Symptomes <- merge(Symptomes, Headache, by = "Mois", all = TRUE)
Symptomes <- merge(Symptomes, Runny_nose, by = "Mois", all = TRUE)
colnames(Symptomes)

#Renommage des colonnes
Symptomes <- Symptomes |> 
  rename(Date = Mois) |> 
  rename(Cough = "Cough: (États-Unis)") |> 
  rename(Fever = "Fever: (États-Unis)") |> 
  rename(Sore_throat = "Sore throat: (États-Unis)") |> 
  rename(Aches = "Aches: (États-Unis)") |> 
  rename(Fatigue = "Fatigue: (États-Unis)") |> 
  rename(Headache = "Headache: (États-Unis)") |> 
  rename(Runny_nose = "runny nose: (États-Unis)")

#Bon typage des données
Symptomes$Date <- as.Date(paste0(Symptomes$Date, "-01"), format = "%Y-%m-%d")
Symptomes$Date <- as.POSIXct(Symptomes$Date)
str(Symptomes)

#ACP#
library(FactoMineR)
Symptomes_standar <- scale(Symptomes[,c(2:8)])
pca_resulta <- PCA(Symptomes_standar, graph = FALSE)
summary(pca_resulta)

#Visualisation propriété ACP
library(corrplot)
library(RColorBrewer)
coordo<-round(pca_resulta$var$coord,2)
corrplot(coordo, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400",col=brewer.pal(n=9, name="RdYlBu"), addCoef.col="black")
contribution<-round(pca_resulta$var$contrib,2)
corrplot(contribution, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#003300", col=brewer.pal(n=9,name="OrRd"), addCoef.col="black")
cosinus<-round(pca_resulta$var$cos2,2)
corrplot(cosinus, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="PiYG"), addCoef.col="black")

#Histogramme
library(factoextra)
fviz_eig(pca_resulta, addlabels = TRUE, ylim = c(0, 50), main = "")

#Création de la vraiables latentes
coefficients_premiere_acp <- pca_resulta$var$coord[, 1]
variables_originales <- Symptomes[, c(2:8)]
Symptomes$Variable_latente_symptomes <- rowSums(variables_originales * coefficients_premiere_acp)

#Maladie concomitante
#importation
Tuberculosis <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 2/multiTimeline-8.csv",skip = 1)
Common_cold <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 2/multiTimeline-9.csv",skip = 1)
Sinusitis <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 2/multiTimeline-10.csv",skip = 1)
Bronchiolitis <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 2/multiTimeline-11.csv",skip = 1)
Viral_Gastroenteritis <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 2/multiTimeline-12.csv",skip = 1)
Covid_19 <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 2/multiTimeline-13.csv",skip = 1)
Bronchitis <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 2/multiTimeline-14.csv",skip = 1)
Pneumonia <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 2/multiTimeline-15.csv",skip = 1)

#Concaténation
Maladies_conco <- merge(Tuberculosis, Common_cold, by = "Mois", all = TRUE)
Maladies_conco <- merge(Maladies_conco, Sinusitis, by = "Mois", all = TRUE)
Maladies_conco <- merge(Maladies_conco, Bronchiolitis, by = "Mois", all = TRUE)
Maladies_conco <- merge(Maladies_conco, Viral_Gastroenteritis, by = "Mois", all = TRUE)
Maladies_conco <- merge(Maladies_conco, Covid_19, by = "Mois", all = TRUE)
Maladies_conco <- merge(Maladies_conco, Bronchitis, by = "Mois", all = TRUE)
Maladies_conco <- merge(Maladies_conco, Pneumonia, by = "Mois", all = TRUE)
colnames(Maladies_conco)

#Renommage des colonnes
Maladies_conco <- Maladies_conco |> 
  rename(Date = Mois) |> 
  rename(Tuberculosis = "Tuberculosis: (États-Unis)") |> 
  rename(Common_cold = "Common cold: (États-Unis)") |> 
  rename(Sinusitis = "Sinusitis: (États-Unis)") |> 
  rename(Bronchiolitis = "Bronchiolitis: (États-Unis)") |> 
  rename(Viral_Gastroenteritis = "Viral Gastroenteritis: (États-Unis)") |> 
  rename(Covid_19 = "Covid-19: (États-Unis)") |> 
  rename(Bronchitis = "Bronchitis: (États-Unis)") |> 
  rename(Pneumonia = "Pneumonia: (États-Unis)")

#Bon typage des données
Maladies_conco$Date <- as.Date(paste0(Maladies_conco$Date, "-01"), format = "%Y-%m-%d")
Maladies_conco$Date <- as.POSIXct(Maladies_conco$Date)
Maladies_conco$Covid_19 <- as.numeric(Maladies_conco$Covid_19)
Maladies_conco$Covid_19[is.na(Maladies_conco$Covid_19)] <- round(runif(sum(is.na(Maladies_conco$Covid_19))),2)
str(Maladies_conco)

#ACP#
library(FactoMineR)
Maladies_conco_standar <- scale(Maladies_conco[,c(2:9)])
pca_resulta2 <- PCA(Maladies_conco_standar, graph = FALSE)
summary(pca_resulta2)

#Visualisation propriété ACP
coordo<-round(pca_resulta2$var$coord,2)
corrplot(coordo, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400",col=brewer.pal(n=9, name="RdYlBu"), addCoef.col="black")
contribution<-round(pca_resulta2$var$contrib,2)
corrplot(contribution, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#003300", col=brewer.pal(n=9,name="OrRd"), addCoef.col="black")
cosinus<-round(pca_resulta2$var$cos2,2)
corrplot(cosinus, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="PiYG"), addCoef.col="black")

#Histogramme
fviz_eig(pca_resulta2, addlabels = TRUE, ylim = c(0, 50), main = "")

#Création de la vraiables latentes 1
coefficients_premiere_acp1 <- pca_resulta2$var$coord[, 1]
variables_originales1 <- Maladies_conco[, -c(1,2,7)]
Maladies_conco$Variable_latente_Maladies_conco1 <- rowSums(variables_originales1 * coefficients_premiere_acp1)

#Création de la vraiables latentes 2
coefficients_premiere_acp2 <- pca_resulta2$var$coord[, 2]
variables_originales2 <- Maladies_conco[, c(3,7)]
Maladies_conco$Variable_latente_Maladies_conco2 <- rowSums(variables_originales2 * coefficients_premiere_acp2)

#Création de la vraiables latentes 3 (On sait jamais)
coefficients_premiere_acp3 <- pca_resulta2$var$coord[, 3]
variables_originales3 <- Maladies_conco[, c(2,7)]
Maladies_conco$Variable_latente_Maladies_conco3 <- rowSums(variables_originales3 * coefficients_premiere_acp3)

#Zoonose
#importation
Avian_Cat <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 3/multiTimeline-16.csv",skip = 1)
Swine <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 3/multiTimeline-17.csv",skip = 1)
High_Avian <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 3/multiTimeline-18.csv",skip = 1)
Equine_Seal <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 3/multiTimeline-19.csv",skip = 1)
Canine <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends 3/multiTimeline-20.csv",skip = 1)

#Concaténation
Zoonose <- merge(Avian_Cat, Swine, by = "Mois", all = TRUE)
Zoonose <- merge(Zoonose, High_Avian, by = "Mois", all = TRUE)
Zoonose <- merge(Zoonose, Equine_Seal, by = "Mois", all = TRUE)
Zoonose <- merge(Zoonose, Canine, by = "Mois", all = TRUE)
colnames(Zoonose)

#Renommage des colonnes
Zoonose <- Zoonose |> 
  rename(Date = Mois) |> 
  rename(Avian_Cat = "H5N1: (États-Unis)") |> 
  rename(Swine = "H1N1: (États-Unis)") |> 
  rename(High_Avian = "H5N8: (États-Unis)") |> 
  rename(Equine_Seal = "H3N8: (États-Unis)") |> 
  rename(Canine = "H3N2: (États-Unis)")

#Bon typage des données
Zoonose$Date <- as.Date(paste0(Zoonose$Date, "-01"), format = "%Y-%m-%d")
Zoonose$Date <- as.POSIXct(Zoonose$Date)
Zoonose$Avian_Cat <- as.numeric(Zoonose$Avian_Cat)
Zoonose$Avian_Cat[is.na(Zoonose$Avian_Cat)] <- round(runif(sum(is.na(Zoonose$Avian_Cat))),2)
Zoonose$Swine <- as.numeric(Zoonose$Swine)
Zoonose$Swine[is.na(Zoonose$Swine)] <-round(runif(sum(is.na(Zoonose$Swine))),2)
str(Zoonose)

#ACP#
Zoonose_standar <- scale(Zoonose[,c(2:6)])
pca_resulta3 <- PCA(Zoonose_standar, graph = FALSE)
summary(pca_resulta3)

#Visualisation propriété ACP
coordo<-round(pca_resulta3$var$coord,2)
corrplot(coordo, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400",col=brewer.pal(n=9, name="RdYlBu"), addCoef.col="black")
contribution<-round(pca_resulta3$var$contrib,2)
corrplot(contribution, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#003300", col=brewer.pal(n=9,name="OrRd"), addCoef.col="black")
cosinus<-round(pca_resulta3$var$cos2,2)
corrplot(cosinus, is.corr=FALSE, method="circle", tl.srt=45, tl.col="#004400", col=brewer.pal(n=9, name="PiYG"), addCoef.col="black")

#Histogramme
fviz_eig(pca_resulta3, addlabels = TRUE, ylim = c(0, 50), main = "")

#Création de la vraiables latentes 1
coefficients_premiere_acp4 <- pca_resulta2$var$coord[, 1]
variables_originales4 <- Zoonose[, c(3,5)]
Zoonose$Variable_latente_Zoonose1 <- rowSums(variables_originales4 * coefficients_premiere_acp4)

#Création de la vraiables latentes 2
coefficients_premiere_acp5 <- pca_resulta2$var$coord[, 2]
variables_originales5 <- Zoonose[, c(2,6)]
Zoonose$Variable_latente_Zoonose2 <- rowSums(variables_originales5 * coefficients_premiere_acp5)

#Google trends hors variables latentes
#importation 
Caté_santé <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends hors variable latentes /Catégorie_Influenza_rhume.csv",skip = 1)
Terme_santé <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends hors variable latentes /Terme_Flu_Influenza.csv",skip = 1)
Sympt_santé <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends hors variable latentes /Symptôme.csv",skip = 1)
Zoonose_santé <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends hors variable latentes /Zoonose.csv",skip = 1)
Maladie_conco_santé <- read_csv("~/Desktop/BASE DE DONNEES USA/Google trends hors variable latentes /Maladie_commune.csv",skip = 1)

#Concaténation
Google_trends <- merge(Caté_santé, Terme_santé, by = "Mois", all = TRUE)
Google_trends <- merge(Google_trends, Sympt_santé, by = "Mois", all = TRUE)
Google_trends <- merge(Google_trends, Zoonose_santé, by = "Mois", all = TRUE)
Google_trends <- merge(Google_trends, Maladie_conco_santé, by = "Mois", all = TRUE)
colnames(Google_trends)

#Renommage des colonnes
Google_trends <- Google_trends |> 
  rename(Date = Mois) |> 
  rename(Caté_santé = "Geo: États-Unis") |> 
  rename(Terme_santé = "flu + influenza: (États-Unis)") |> 
  rename(Sympt_santé = "fever + cough + aches + headaches + sore throat + runny nose: (États-Unis)") |> 
  rename(Zoonose_santé = "h5n1 + h1n1 + h5n8 + h3n2: (États-Unis)") |> 
  rename(Maladie_conco_santé = "common cold + pneumonia + bronchitis + bronchiolitis + viral gastroenteritis + sinusitis: (États-Unis)")

#Bon typage des données
Google_trends$Date <- as.Date(paste0(Google_trends$Date, "-01"), format = "%Y-%m-%d")
Google_trends$Date <- as.POSIXct(Google_trends$Date)
Google_trends$Zoonose_santé <- as.numeric(Google_trends$Zoonose_santé)
Google_trends$Zoonose_santé[is.na(Google_trends$Zoonose_santé)] <- round(runif(sum(is.na(Google_trends$Zoonose_santé))),2)
str(Google_trends)



#Création base de données finale sans variables latentes
Grippe_TSDBD <- merge(Y, Grippe, by = "Date", all = TRUE)
Grippe_TSDBD <- merge(Grippe_TSDBD, Economic, by = "Date", all = TRUE)
Grippe_TSDBD <- merge(Grippe_TSDBD, IP, by = "Date", all = TRUE)
Grippe_TSDBD <- merge(Grippe_TSDBD, Météo, by = "Date", all = TRUE)
Grippe_TSDBD <- merge(Grippe_TSDBD, Pop, by = "Date", all = TRUE)
Grippe_TSDBD <- merge(Grippe_TSDBD, Transport, by = "Date", all = TRUE)
Grippe_TSDBD <- merge(Grippe_TSDBD, Google_trends, by = "Date", all = TRUE)

write_xlsx(Grippe_TSDBD,"Grippe_TSDBD.xlsx")







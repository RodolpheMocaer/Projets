#library
library(readxl)
library(knitr)
library(corrplot)
library(ggplot2)
library(tseries)
library(e1071)
library(dplyr)
library(extraDistr)

#importation
Grippe_TSDBD <- read_excel("~/Desktop/BASE DE DONNEES USA/Grippe_TSDBD.xlsx")
grippe <- Grippe_TSDBD[c(673:912),-c(2,4)]
grippe_2023 <- grippe[c(229:240),]
grippe_donnée <- grippe[-c(229:240),]
grippe2 <- Grippe_TSDBD[c(672:900),-c(2,4)]

#Analyse de Y au préalable
Yt <- grippe_donnée$ILITOTAL
#Visualisation de notre série Brute
# Création du graphique
ggplot(data = grippe_donnée, aes(x = 1:length(ILITOTAL), y = ILITOTAL)) +
  geom_line(color = "#00561b", size = 1, linetype = "solid") +
  geom_point(color = "#381a3c", size = 3, shape = 20) +
  labs(x = "Temps", y = "Incidence de la grippe", 
       title = "Évolution de l'incidence de la grippe aux USA entre 2004 et 2022") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_text(size = 14),  
        plot.title = element_text(size = 16)) +   
  scale_x_continuous(breaks = seq(from = 1, to = 228, by = 12), 
                     labels = c("2004", "2005", "2006", "2007", "2008",
                                "2009","2010","2011","2012","2013",
                                "2014","2015","2016","2017","2018","2019","2020",
                                "2021","2022"))
#Propriété statistique
#Stationarité
adf_y <- adf.test(Yt) #validé
table_results <- data.frame(
  Statistique = c("Statistique du test", "Retars", "P valeur"),
  Valeur = c(adf_y$statistic,adf_y$parameter,adf_y$p.value)
)
kable(table_results, align = "c", caption = "Résultats du test ADF pour la variable y")

#distribution#
kurtosis_y <- kurtosis(Yt)
skewness_y <- skewness(Yt)
#Normalité
shapiro_y <- shapiro.test(Yt) #non validé
shapiro_y_1 <- shapiro.test(1/Yt) #non validé
shapiro_logy <- shapiro.test(log(Yt)) #validé au seuil de 5%
shapiro_sqrty <- shapiro.test(sqrt(Yt)) #non validé
table_results <- data.frame(
  Statistique = c("Kurtosis", "Skewness", "Shapiro-Wilk stat value Yt","Shapiro-Wilk p-value Yt"
                  , "Shapiro-Wilk stat value 1/Yt","Shapiro-Wilk p-value 1/Yt", "Shapiro-Wilk stat value log(Yt)","Shapiro-Wilk p-value log(Yt)"
                  , "Shapiro-Wilk stat value √Yt","Shapiro-Wilk p-value √Yt"),
  Valeur = c(kurtosis_y, skewness_y,shapiro_y$statistic ,shapiro_y$p.value,shapiro_y_1$statistic,shapiro_y_1$p.value,
             shapiro_logy$statistic,shapiro_logy$p.value,shapiro_sqrty$statistic,shapiro_sqrty$p.value)
)
kable(table_results, align = "c", caption = "Résultats des statistiques de kurtosis, skewness et test de normalité Shapiro-Wilk pour la variable y")

#Homogénéité
#validé car nous utilisons une variables donc forcément homogéne
#Stabilité spatial et temporelle
#validé car le même organise qui prends les données 


#Boxplot de Yt
# Création du graphique boxplot
boxplot(Yt, 
        col = "#00561b",
        border = "#00561b",
        main = "",
        xlab = "Incidence de la grippe",
        ylab = "Incidence de la grippe de l'ensemble de 2004 à 2022",
        staplewex = 2, 
        whiskcol = "#86a886",
        staplecol = "#86a886",
        outcol = "#381a3c",
        cex.axis = 1.5,
        cex.lab = 1.5) 

#Statistique descriptives
summary_y <- round(summary(Yt),2)
sd_y <- round(sd(Yt),2)
var_y <- round(var(Yt),2)
table_summary <- data.frame(
  Statistique = c("Minimum", "1er quartile", "Médiane", "Moyenne", "3ème quartile", "Maximum", "Écart-type", "Variance"),
  Valeur = c(paste(summary_y[1], collapse = " - "), paste(summary_y[2], collapse = " - "), paste(summary_y[3], collapse = " - "),
             paste(summary_y[4], collapse = " - "), paste(summary_y[5], collapse = " - "), paste(summary_y[6], collapse = " - "),
             sd_y, var_y)
)
kable(table_summary, align = "c", caption = "Résumé statistique de la variable y")

#Analyse de nos X au préalable
#Stationarité
adf_results <- list(
  BBKCI = adf.test(grippe_donnée$BBKCI),
  BBKGDP = adf.test(grippe_donnée$BBKGDP),
  BBKF = adf.test(grippe_donnée$BBKF),
  Medical_care = adf.test(grippe_donnée$Medical_care),
  Transportation = adf.test(grippe_donnée$Transportation), #x
  Médian = adf.test(grippe_donnée$Médian), #x
  PRCP = adf.test(grippe_donnée$PRCP),
  Mean = adf.test(grippe_donnée$Mean),
  Pop_globale = adf.test(grippe_donnée$Pop_globale), #x
  Pop_plus_55_ans = adf.test(grippe_donnée$Pop_plus_55_ans), #x
  Pop_moins_16_ans = adf.test(grippe_donnée$Pop_moins_16_ans), #x
  Avion = adf.test(grippe_donnée$Avion), #seuil de 10% x
  Train = adf.test(grippe_donnée$Train), #x
  Voiture = adf.test(grippe_donnée$Voiture), #x
  Commun = adf.test(grippe_donnée$Commun), #seuil de 10% x
  Caté_santé = adf.test(grippe_donnée$Caté_santé),
  Terme_santé = adf.test(grippe_donnée$Terme_santé),
  Sympt_santé = adf.test(grippe_donnée$Sympt_santé),
  Zoonose_santé = adf.test(grippe_donnée$Zoonose_santé),
  Maladie_conco_santé = adf.test(grippe_donnée$Maladie_conco_santé)
)
p_values <- sapply(adf_results, function(x) x$p.value)
adf_df <- data.frame(
  P_Value = p_values,
  Val_stat = sapply(adf_results,function(x) x$statistic)
  
)
kable(adf_df)
colnames(grippe2)
#Différenciation des séries non stationnaire
Série_non_stat <- data.matrix(grippe2[,c(7,8,11,12,13,14,15,16,17)])
dSérie_non_stat = diff(Série_non_stat)
dSérie_non_stat = data.frame(dSérie_non_stat)
str(dSérie_non_stat)

#test ADF
adf_results2 <- list(
  Transportation = adf.test(dSérie_non_stat$Transportation),
  Médian = adf.test(dSérie_non_stat$Médian),
  Pop_globale = adf.test(dSérie_non_stat$Pop_globale), #x au seuil de 10%
  Pop_plus_55_ans = adf.test(dSérie_non_stat$Pop_plus_55_ans),
  Pop_moins_16_ans = adf.test(dSérie_non_stat$Pop_moins_16_ans),
  Avion = adf.test(dSérie_non_stat$Avion),
  Train = adf.test(dSérie_non_stat$Train),
  Voiture = adf.test(dSérie_non_stat$Voiture),
  Commun = adf.test(dSérie_non_stat$Commun)
)
p_values <- sapply(adf_results2, function(x) x$p.value)
adf_df2 <- data.frame(
  P_Value = p_values,
  Val_stat = sapply(adf_results2,function(x) x$statistic)
  
)
kable(adf_df2)

#Nous décidons de retiré pop_global et de ne pas faire une deuxiéme différenciation.
dSérie_non_stat_sans_popg <- dSérie_non_stat[,-3]

#Data base stationnaire
Data_stationnaire <- bind_cols(grippe_donnée[,-c(7,8,11,12,13,14,15,16,17)], dSérie_non_stat_sans_popg) #Sans popG
Data_stationnaire2 <- bind_cols(grippe_donnée[,-c(7,8,11,12,13,14,15,16,17)], dSérie_non_stat) #Avec popG
Data <- Data_stationnaire[,-c(1,2)]
str(Data)
summary(Data)
#Statistique descriptives
compute_stats <- function(col) {
  c(min = min(col),
    médiane = median(col),
    moyenne = mean(col),
    max = max(col),
    variance = var(col))
}
stats <- t(apply(Data, 2, compute_stats))
table_summary <- data.frame(Valeur = round(stats[, ],0))
print(table_summary)

#Corrélation
#esenmble
Correlation = cor(Data_stationnaire2[,-1],use = "complete.obs",method="s" )
col <- colorRampPalette(c("#22780f","#fee7f0", "#ccccff"))
corrplot(Correlation, method = "color", col = col(200), 
         type = "upper", order = "FPC",
         tl.col = "black", tl.srt = 45, 
         diag = FALSE, 
         addCoef.col = "black", 
         number.cex = 0.7, 
         tl.cex = 0.8, 
         addrect = 4)

#Normalité de la distribution
g <- Yt
g_normalized <- (g - min(g)) / (max(g) - min(g))
hist_density <- density(g_normalized)
exp_density <- dexp(hist_density$x, rate = 1/mean(g_normalized))
norm_density <- dnorm(hist_density$x, mean = mean(g_normalized), sd = sd(g_normalized))
cauchy_density <- dcauchy(hist_density$x, location = mean(g_normalized), scale = sd(g_normalized))
weibull_density <- dweibull(hist_density$x, shape = 2, scale = 1)
log_normal_density <- dlnorm(hist_density$x, meanlog = mean(log(g_normalized)), sdlog = sd(log(g_normalized)))

palette <- c("#00561b", "#0000FF", "#FF0000", "#00FF00", "#FFFF00", "#FFA500", "#800080")

plot(hist_density, main = "Comparaison des distributions", xlab = "Valeurs", ylab = "Densité", col = palette[1], lwd = 2, ylim = c(0, max(hist_density$y, exp_density)), xlim = c(0:1))
lines(hist_density$x, exp_density, col = palette[2], lwd = 2, lty = "dashed")
lines(hist_density$x, norm_density, col = palette[3], lwd = 2, lty = "dashed")
lines(hist_density$x, cauchy_density, col = palette[4], lwd = 2, lty = "dashed")
lines(hist_density$x, weibull_density, col = palette[5], lwd = 2, lty = "dashed")
lines(hist_density$x, log_normal_density, col = palette[6], lwd = 2, lty = "dashed")

lines(hist_density, col = palette[1], lwd = 4)

legend("topright", legend = c("Yt Normalisé","Distribution Expo","Distribution Normal"," Distribution Cauchy"," Distribution Weibull", "Distribution Log-normal", "Distribution Laplace"),
       col = palette, lty = c(1, rep(2, 6)),cex = 0.6,text.width = 0.09)







#Prévisions univariées
#library
library(RJDemetra)
library(seastests)
library(seasonal)
library(seastests)
library(forecast)
library(writexl)
library(smooth)
library(tidyr)

#Graphique de comparaison de la saisonnalité par année, package forecast
yy2 <- ts(data = grippe_donnée$ILITOTAL, start=c(2004,01),end=c(2022,12),frequency=12)
ggtsdisplay(yy2, plot.type="histogram")
ggseasonplot(yy2, col=rainbow(12), year.labels=TRUE)

#Détection de la saisonnalité
sea <- seasdum(yy2)
# Friedman test
ft <- fried(yy2)
# Kruskal-Wallis test
kwt <- kw(yy2)
# Webel-Ollech test - new version of seastests (2021-09)
wot <- combined_test(yy2)
#Toutes les P-value sont inférieurs à 1% ainsi, nous rejetons l'hypothése 
#d'absence de saisonnalité
table_results <- data.frame(
  Test = c("Seasonnal - Dummies", "Friedman", "Kruskal - Wallis", "QS - test",
                  "QS - test robuste", "Koenker - Basset"),
  "Valeur Statistique" = c(sea$stat,ft$stat,kwt$stat,wot$stat[1],wot$stat[2],wot$stat[3]),
  "P-value" = c(sea$Pval,ft$Pval,kwt$Pval,wot$Pval[1],wot$Pval[2],wot$Pval[3])
)
kable(table_results, align = "c", caption = "Test de saisonnalité")

#Prevision série temporelle
#Spé additive ou multiplicative
seasX <- seas(yy2)
out(seasX)

#Désaisonnalisation de la série IPC
myspec <- x13_spec("RSA5c") 
mysax13 <- x13(yy2, myspec)
mysax13
s_transform(mysax13) #Pas besoin de transformation / schéma de décompositions additives
summary(mysax13$regarima)
y <- mysax13$final$series[,1] #série réelle
sa <-mysax13$final$series[,2] #série ajustée 
t <- mysax13$final$series[,3] #trends
s <- mysax13$final$series[,4] #Cycle saisonnier
i <- mysax13$final$series[,5] #irrégularité
#Visualisation de la désaisonalisation et de la décomposition
#Evolution de l'incidence
par(mfrow=c(1,3))
#Yt#
plot(y, type = "o", col = "#00561b", pch = 20, xlab = "Temps", ylab = "Incidence de la grippe",
     main = "Évolution",
     cex.axis = 1.5, cex.lab = 1.5)
#Série ajustée#
plot(sa, type = "o", col = "#5f8c61", pch = 20, xlab = "Temps", ylab = "Incidence de la grippe",
     main = "Évolution ajustée",
     cex.axis = 1.5, cex.lab = 1.5)
#Trends#
plot(t, type = "o", col = "#adc4ad", pch = 20, xlab = "Temps", ylab = "Incidence de la grippe",
     main = "Trends",
     cex.axis = 1.5, cex.lab = 1.5)
par(mfrow=c(1,2))
#Yt#
plot(s, type = "l", col = "#357ab7", xlab = "Temps", ylab = "Incidence de la grippe",
     main = "Cycle saisonnier",
     cex.axis = 1.5, cex.lab = 1.5)
#Série ajustée#
plot(i, type = "l", col = "#a3babd", xlab = "Temps", ylab = "Incidence de la grippe",
     main = "Irrégularité",
     cex.axis = 1.5, cex.lab = 1.5)
par(mfrow=c(1,1))

#Graphique de comparaison de la série ajustée et de la série non ajustée
Y_data <- as.data.frame(y)
Sa_data <- as.data.frame(sa)

ggplot() +
  geom_line(data = Y_data, aes(x = 1:length(y), y = x, color = "1"), linetype = "solid") +
  geom_line(data = Sa_data, aes(x = 1:length(sa), y = x, color = "2"), linetype = "solid") +
  labs(x = "Evolution", y = "Incidences des Symptômes grippaux", title = "") + 
  scale_color_manual(name = "Données avec et sans désaisonnalisation",
                     values = c("1" = "#357ab7",
                                "2" = "#dc147c"), 
                     labels = c("Sans", "Avec"))+
  theme_minimal()

#Les prévisions avec la série ajustée
#mise à jour de nos données
str(sa) #Bon typage

#prévision naïve
forecast <- naive(sa,h=12)
plot(naive(sa,h=12))
prevision_naïve <-as.data.frame(forecast$mean)

#prévision X13 - ARIMA - SEATS
myregx13 <- regarima_x13(sa, spec ="RG5c")
s_transform(myregx13)
prevision_23_X13 <-as.data.frame(myregx13$forecast[1:12])

#STL
fitstl = stlm(sa)
prevstl <- forecast(fitstl,12)
plot(prevstl)
prevision_STL <-as.data.frame(prevstl$mean)

#STS
fitsts = StructTS(sa)
prevsts <- forecast(fitsts,12)
plot(prevsts$mean)
prevision_STS <-as.data.frame(prevsts$mean)

#Bagged_model
fitbag <- baggedModel(sa)
prevbag <- forecast(fitbag,12)
plot(prevbag$mean)
prevision_bagged <-as.data.frame(prevbag$mean)

#Méthode de lissage
#Holt-Winters
fithw <- HoltWinters(sa)
prevhw <- forecast(fithw, h=12)
plot(fithw)
summary(fithw)
prevision_HO <-as.data.frame(prevhw$mean)

#ETS
fitets <- ets(sa)
prevets <- forecast(fitets,12)
plot(prevets)
prevision_ETS <-as.data.frame(prevets$mean)

#TBATS
fittbats = tbats(sa)
prevtbats <- forecast(fittbats,12)
plot(prevtbats)
prevision_BATS <-as.data.frame(prevtbats$mean)

#ADAM - 1
fitadam1 <- auto.adam(sa, model="ZZZ", lags=c(1,12), select=TRUE)
prevadam1 <- forecast(fitadam1, h=12, level = 0.90)
plot(prevadam1)
prevision_ADAM1 <-as.data.frame(prevadam1$mean)

#ADAM - SARIMA
fitadam3 <- auto.adam(sa, model="ZZN", lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
prevadam3 <- forecast(fitadam3, h=12, level = 0.90)
plot(prevadam3)
prevision_ADAM_SARIMA <-as.data.frame(prevadam3$mean)

#SSARIMA
fitssarima <- auto.ssarima(sa, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
prevssarima <- forecast(fitssarima, h=12, level = 0.90)
plot(prevssarima)
prevision_SSARIMA <-as.data.frame(prevssarima$mean)

#CES
fitces2 <- auto.ces(sa, models=c("n","s","p","f"))
prevces <- forecast(fitces2, h=12, level = 0.90)
plot(prevces)
prevision_CES <-as.data.frame(prevces$mean)

#SARIMA
myspec <- regarima_spec_tramoseats("TRfull")
myregts <- regarima(sa, myspec)
prevision_SARIMA <-as.data.frame(myregts$forecast[1:12])

#Concaténation des prévisions 
PREV <- cbind(prevision_23_X13,prevision_ADAM_SARIMA,prevision_ADAM1,
              prevision_bagged,prevision_BATS,prevision_CES,prevision_ETS,
              prevision_HO,prevision_naïve,prevision_SARIMA,prevision_SSARIMA,prevision_STL,
              prevision_STS)
#Renommage des colonnes
names(PREV) <- c("X13", "LE_ADAM_SARIMA", "LE_ADAM1", "BAGGED", "LE_BATS", "LE_CES", 
                 "LE_ETS", "LE_HO", "NAIVE", "LE_SARIMA", "LE_SSARIMA", "STL", "STS")
#Reorganisation de la base de données
Prévision_long_autres <- gather(PREV[,c(1,4,9,12,13)], key = "Methodes", value = "Prévision")
Prévision_long_MLE <- gather(PREV[,-c(1,4,9,12,13)], key = "Methodes", value = "Prévision")

#Ajout du temps
mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", 
          "Août", "Septembre", "Octobre", "Novembre", "Décembre")
times <- rep(1:12, times = length(colnames(PREV[,c(1,4,9,12,13)])))
Prévision_long_autres <- Prévision_long_autres  |> 
  mutate(Times = times)

times2 <- rep(1:12, times = length(colnames(PREV[,-c(1,4,9,12,13)])))
Prévision_long_MLE <- Prévision_long_MLE  |> 
  mutate(Times = times2)

#Visualisation des diférentes prévisions
#Les 1er
ggplot(Prévision_long_autres, aes(x = Times, y = Prévision, color = Methodes)) +
  geom_line() +
  labs(title = "Prévisions de différents modèles",
       x = "Année 2023", y = "Prévisions de l'incidence des symptômes grippaux") +
  theme_minimal() + 
  scale_x_continuous(breaks = 1:12, labels = mois)

#les méthodes de lissage exponetielle
ggplot(Prévision_long_MLE, aes(x = Times, y = Prévision, color = Methodes)) +
  geom_line() +
  labs(title = "Prévisions de différents modèles de lissage exponentiel",
       x = "Année 2023", y = "Prévisions de l'incidence des symptômes grippaux") +
  theme_minimal() + 
  scale_x_continuous(breaks = 1:12, labels = mois)









#Les prévisions avec la série non ajustée car résultats des prévisions louche
#mise à jour de nos données
str(y) #Bon typage

#prévision naïve
forecast <- naive(y,h=12)
plot(naive(y,h=12))
summary(forecast)
prevision_naïve2 <-as.data.frame(forecast$mean)

#prévision X13 - ARIMA - SEATS
myregx13 <- regarima_x13(y, spec ="RG5c")
s_transform(myregx13)
plot(myregx13)
summary(myregx13)
prevision_23_X13_2 <-as.data.frame(myregx13$forecast[1:12])

#STL
fitstl = stlm(y)
prevstl <- forecast(fitstl,12)
plot(prevstl)
summary(prevstl)
prevision_STL2 <-as.data.frame(prevstl$mean)

#STS
fitsts = StructTS(y)
prevsts <- forecast(fitsts,12)
plot(prevsts)
summary(prevsts)
prevision_STS_2 <-as.data.frame(prevsts$mean)

#Bagged_model
fitbag <- baggedModel(y)
prevbag <- forecast(fitbag,12)
plot(prevbag)
summary(prevbag)
prevision_bagged2 <-as.data.frame(prevbag$mean)

#Méthode de lissage
#Holt-Winters
fithw <- HoltWinters(y)
prevhw <- forecast(fithw, h=12)
plot(prevhw)
summary(prevhw)
prevision_HO2 <-as.data.frame(prevhw$mean)

#ETS
fitets <- ets(y)
prevets <- forecast(fitets,12)
plot(prevets)
summary(prevets)
prevision_ETS2 <-as.data.frame(prevets$mean)

#TBATS
fittbats = tbats(y)
prevtbats <- forecast(fittbats,12)
plot(prevtbats)
summary(prevtbats)
prevision_BATS2 <-as.data.frame(prevtbats$mean)

#ADAM - 1 x
fitadam1 <- auto.adam(y, model="ZZZ", lags=c(1,12), select=TRUE)
prevadam1 <- forecast(fitadam1, h=12, level = 0.90)
plot(prevadam1)
summary(prevadam1)
prevision_ADAM1_2 <-as.data.frame(prevadam1$mean)

#ADAM - SARIMA 
fitadam3 <- auto.adam(y, model="ZZN", lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
prevadam3 <- forecast(fitadam3, h=12, level = 0.90)
plot(prevadam3)
summary(fitadam3)
prevision_ADAM_SARIMA2 <-as.data.frame(prevadam3$mean)

#SSARIMA 
fitssarima <- auto.ssarima(y, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
prevssarima <- forecast(fitssarima, h=12, level = 0.90)
plot(prevssarima)
summary(fitssarima)
prevision_SSARIMA2 <-as.data.frame(prevssarima$mean)

#CES
fitces2 <- auto.ces(y, models=c("n","s","p","f"))
prevces <- forecast(fitces2, h=12, level = 0.90)
plot(prevces)
summary(fitces2)
prevision_CES2 <-as.data.frame(prevces$mean)

#SARIMA 
myspec <- regarima_spec_tramoseats("TRfull")
myregts <- regarima(y, myspec)
summary(myregts)
prevision_SARIMA2 <-as.data.frame(myregts$forecast[1:12])

#Concaténation des prévisions 
PREV <- cbind(prevision_23_X13_2,prevision_ADAM_SARIMA2,prevision_ADAM1_2,
              prevision_bagged2,prevision_BATS2,prevision_CES2,prevision_ETS2,
              prevision_HO2,prevision_naïve2,prevision_SARIMA2,prevision_SSARIMA2,prevision_STL2,
              prevision_STS_2)
#Renommage des colonnes
names(PREV) <- c("X13", "LE_ADAM_SARIMA", "LE_ADAM1", "BAGGED", "LE_BATS", "LE_CES", 
                 "LE_ETS", "LE_HO", "NAIVE", "LE_SARIMA", "LE_SSARIMA", "STL", "STS")
#Reorganisation de la base de données
Prévision_long_autres2 <- gather(PREV[,c(1,4,9,12,13)], key = "Methodes", value = "Prévision")
Prévision_long_MLE2 <- gather(PREV[,-c(1,4,9,12,13)], key = "Methodes", value = "Prévision")

#Ajout du temps
mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", 
          "Août", "Septembre", "Octobre", "Novembre", "Décembre")
times <- rep(1:12, times = length(colnames(PREV[,c(1,4,9,12,13)])))
Prévision_long_autres2 <- Prévision_long_autres2  |> 
  mutate(Times = times)

times2 <- rep(1:12, times = length(colnames(PREV[,-c(1,4,9,12,13)])))
Prévision_long_MLE2 <- Prévision_long_MLE2  |> 
  mutate(Times = times2)

#Visualisation des diférentes prévisions
#Les 1er
ggplot(Prévision_long_autres2, aes(x = Times, y = Prévision, color = Methodes)) +
  geom_line() +
  labs(title = "Prévisions de différents modèles",
       x = "Année 2023", y = "Prévisions de l'incidence des symptômes grippaux") +
  theme_minimal() + 
  scale_x_continuous(breaks = 1:12, labels = mois)

#les méthodes de lissage exponetielle
ggplot(Prévision_long_MLE2, aes(x = Times, y = Prévision, color = Methodes)) +
  geom_line() +
  labs(title = "Prévisions de différents modèles de lissage exponentiel",
       x = "Année 2023", y = "Prévisions de l'incidence des symptômes grippaux") +
  theme_minimal() + 
  scale_x_continuous(breaks = 1:12, labels = mois)
#résultats beaucoup plus cohérent, notre modéle nécessite pas de désaisonnalisation









#Série multivariées
#library
library(MASS)
library(BeSS)
library(lgarch)
library(tidyverse)
library(gets)
library(leaps)
library(car)
library(forecast)
library(seasonal)
library(writexl)

#Sélections des variables uniquement stationnaire
#Sélectione des variables uniquement saisonniére % pas obligés
colnames(Data_stationnaire) #avec Y et avec le mois
df_wm <- Data_stationnaire[,-1]
colnames(df_wm) #avec Y et sans mois

#Choix des variables par plusieurs méthodes
#Par le package MASS
fit <- lm(ILITOTAL ~ BBKCI + BBKGDP + BBKF + Medical_care + PRCP + Mean + Caté_santé + Terme_santé + 
            Sympt_santé + Zoonose_santé + Maladie_conco_santé + Transportation + Médian + 
            Pop_plus_55_ans + Pop_moins_16_ans + Avion + Train + Voiture + Commun, data = df_wm)
#1er approche
step1 <- stepAIC(fit, direction="both")
summary(step1)
# BBKCI / BBKGDP / BBKF / Medical_care / Mean / Terme_santé / Sympt_santé / Zoonose_santé / Maladie_conco_santé
# / Pop_moins_16_ans / Pop_plus_55_ans #11 + intercepts#

#2eme approche
step2 <- stepAIC(fit, direction="forward")
summary(step2)
# BBKCI / BBKGDP / BBKF / Medical_care / PRCP / Mean / Caté_santé / Terme_santé / Sympt_santé 
# Zoonose_santé / Maladie_conco_santé / Transportation / Médian / Pop_plus_de_55_ans
# Pop_moins_16_ans / Avion / Train / Voiture / Communn #18 + intercepts#

#Aucune sélection
#3eme approche
step3 <- stepAIC(fit, direction="backward")
summary(step3)
# BBKCI / BBKGDP / BBKF / Medical_care / Mean / Terme_santé / Sympt_santé / Zoonose_santé / Maladie_conco_santé
# Pop_moins_16_ans / Pop_plus_55_ans #11 + intercepts#

#D'aprés ces méthodes nous décidons de prendre considération des choix step1 et step2
Fit1 <- step3$model #Step3 = Step1

#Par le package BeSS
#réglage des variables
Yt
x <- df_wm[,-1]
fitbess <- bess(x, Yt, family = "gaussian")
bestmodel <- fitbess$bestmodel
summary(bestmodel)
FitbeSS <- fitbess$bestmodel$model
fitbess$bestmodel$coefficients
# BBKCI / BBKGDP / BBKF / Medical_care / Mean / Caté_santé / Terme_santé / Sympt_santé 
# Zoonose_santé / Maladie_conco_santé /Transportation / Médian / Pop_plus_de_55_ans
# Pop_moins_16_ans / Voiture #15#

#Par le package leaps
#le choix par sous ensemble
#Nous reprenons le choix de step3
leaps <- regsubsets(ILITOTAL ~ BBKCI + BBKGDP + BBKF + Medical_care + Mean + Caté_santé + Terme_santé + 
                      Sympt_santé + Zoonose_santé + Maladie_conco_santé + Transportation + Médian +
                      Pop_plus_55_ans + Pop_moins_16_ans + Voiture, 
                    data = df_wm, nbest=1, nvmax=15, method=c("exhaustive"))

summary <- summary(leaps)
#Choix du meilleur modéle
res.sum <- summary(leaps)
resultats <- data.frame(
  Adj.R2 = (res.sum$adjr2),
  CP = (res.sum$cp),
  BIC = (res.sum$bic)
)
resultats_classement <- data.frame(
  Adj.R2 = (res.sum$adjr2),
  CP = (res.sum$cp),
  BIC = (res.sum$bic),
  Adj.R2 = rank(desc(resultats$Adj.R2)),
  CP = rank(resultats$CP),
  BIC = rank(resultats$BIC)
)
resultats_classement$Classement_G <- rank((rowMeans(resultats_classement[,4:6])))
resultats_classement$Classement_G <- floor(resultats_classement$Classement_G)
names(resultats_classement) <- c("Adj_R^2","CP","BIC","Classement_R^2","Classement_CP","Classement_BIC","Classement_G")

#Visualisation
ggplot(data = resultats_classement) +
  geom_point(aes(x = index(resultats_classement), y = `Classement_R^2`, colour = "1"), shape = 8, size = 4, position = position_jitter(width = 0.1)) +  
  geom_point(aes(x = index(resultats_classement), y = Classement_CP, colour = "2"), shape = 8, size = 4, position = position_jitter(width = 0.1)) +  
  geom_point(aes(x = index(resultats_classement), y = Classement_BIC, colour = "3"), shape = 8, size = 4, position = position_jitter(width = 0.1)) + 
  geom_point(aes(x = index(resultats_classement), y = Classement_G, shape = "4"), col = "#00561b", size = 4) + 
  labs(x = "Modèles à X variables", y = "Classement", 
       title = "Classement des différents modèles") +  
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_x_continuous(breaks = seq(min(index(resultats_classement)), max(index(resultats_classement)), by = 1)) + 
  scale_y_continuous(breaks = seq(min(index(resultats_classement)), max(index(resultats_classement)), by = 1)) +
  scale_colour_manual(name = "Indicateur statistique",
                      values = c("1" = "#dc147c", "2" = "#9e0e40", "3" = "#357ab7"), 
                      labels = c("R^2","CP","BIC")) +
  scale_shape_manual(name = "Classement général des modéles",
                     values = c("4" = 16),
                     labels = c("Place"))
  

#Le 8 modéles est le meilleur#
#Choix du meilleur modéles à partir de Step
line_11 <- summary$outmat[11, ]
print(line_11)
#Sélection meilleure modéles# 
Fit1 <- df_wm[,c(2,3,4,5,7,9,10,11,12,15,16)]

#Prévisions des modéles
#Marche aléatoire / naïve
forecast <- rwf(Yt,h=12)
plot(forecast)
plot(rwf(Yt,h=22))

#Estimation lm()
# Séparation de la base de données
trainingbase <- df_wm[1:228, ]
testingbase <- df_wm[217:228, ]

# Ajustement du modèle, choisie par leaps et BeSS
modelf <- lm(ILITOTAL ~ BBKCI + BBKGDP + BBKF + Medical_care + Mean + Zoonose_santé + Terme_santé +
               Sympt_santé + Maladie_conco_santé + Pop_moins_16_ans + Pop_plus_55_ans, data = trainingbase)
summary(modelf)
# Ajustement du modèle, ensemble des variables
modelf2 <- lm(ILITOTAL ~ BBKCI + BBKGDP + BBKF + Medical_care + PRCP + Mean + Caté_santé + Terme_santé + 
                Sympt_santé + Zoonose_santé + Maladie_conco_santé + Transportation + Médian + 
                Pop_plus_55_ans + Pop_moins_16_ans + Avion + Train + Voiture + Commun ,data = df_wm)
summary(modelf)
summary(modelf2)

# Prédiction avec modéle 1
forecast1 <- predict(modelf, newdata = testingbase)
Prev_lm1 <- as.data.frame(forecast1)
plot(Prev_lm1$forecast1, type = "l")

# Prédiction avec modéle 2
forecast2 <- predict(modelf2, newdata = testingbase)
Prev_lm2 <- as.data.frame(forecast2)
plot(Prev_lm2$forecast2, type = "l")

#Estimation ARMAX()
#séparation base de données
head(Fit1)
base_armax <- ts(Fit1)
colnames(Fit1)
#Estimation de ARMAX
#ARMAX sans auto_arima
modelx1=Arima(Yt,order=c(1,0,0), xreg = base_armax, seasonal=FALSE, include.mean=FALSE)
modelx1$var.coef
j <- ncol(modelx1$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx1$coef[i]/sqrt(modelx1$var.coef[i,i])
}
tstat
#Le meilleure modéle est le AR(1)
#Nous enelevons les valeurs absolue qui sont inférieur à 1,64
#Nouvelle base de données sans 1 / 2 / 3 / 5

base_armax2 <- ts(Fit1[,- c(1,2,3,5)])
modelx2=Arima(Yt,order=c(1,0,0), xreg = base_armax2, seasonal=FALSE, include.mean=FALSE)
j <- ncol(modelx2$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx2$coef[i]/sqrt(modelx2$var.coef[i,i])
}
tstat
#Nous gardons ce modéle
#Prédiction avec le deuxiéme modéle
f1= predict(modelx2, newxreg = base_armax2[217:228,], n.ahead = 12)
Prev_ARMAX1 <- as.data.frame(f1$pred)
summary(modelx2)

#Estimation ARMAX() / auto.arima()
#ARMAX avec auto.arima
modelx3=auto.arima(Yt, xreg = base_armax, seasonal=TRUE, stationary=TRUE)
j <- ncol(modelx3$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx3$coef[i]/sqrt(modelx3$var.coef[i,i])
}
tstat
#Nous enelevons les valeurs absolue qui sont inférieur à 1,64
#Nouvelle base de données sans 1 / 2 / 3 / 5
modelx4=auto.arima(Yt, xreg = base_armax2, seasonal=TRUE, stationary=TRUE)
j <- ncol(modelx4$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx4$coef[i]/sqrt(modelx4$var.coef[i,i])
}
tstat
#Nous enelevons les valeurs absolue qui sont inférieur à 1,64
#Nouvelle base de données sans 1
base_armax3 <- ts(Fit1[,- c(1,2,3,4,5)])
modelx5=auto.arima(Yt, xreg = base_armax3, seasonal=TRUE, stationary=TRUE)
j <- ncol(modelx4$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx4$coef[i]/sqrt(modelx4$var.coef[i,i])
}
tstat
#Nous gardons ce modéle
#Prédiction avec le deuxiéme modéle
f2= predict(modelx5, newxreg = base_armax3[217:228,], n.ahead = 12)
Prev_ARMAX2 <- as.data.frame(f2$pred)
summary(modelx5)
#Estimation ARX() / auto.arima()
#ARX avec auto.arima
modelx6=auto.arima(Yt, max.q = 0, xreg = base_armax, seasonal=FALSE, stationary=TRUE)
j <- ncol(modelx5$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx5$coef[i]/sqrt(modelx5$var.coef[i,i])
}
tstat
#Nous gardons ce modéles
#Prédiction avec le deuxiéme modéle
f3= predict(modelx6, newxreg = base_armax[217:228,], n.ahead = 12)
Prev_ARX <- as.data.frame(f3$pred)
summary(modelx6)

#Concaténation des data.frame de prévision
Prévision_mutli <- cbind(Prev_lm1, Prev_lm2,Prev_ARMAX1,Prev_ARMAX2,Prev_ARX)
names(Prévision_mutli) <- c("lm1", "lm2", "ARMAX1", "ARMAX2", "ARX")

#Reorganisation de la base de données
Prévision_mutli_bonne <- gather(Prévision_mutli, key = "Methodes", value = "Prévision")
#Ajout du temps
mois <- c("Janvier", "Février", "Mars", "Avril", "Mai", "Juin", "Juillet", 
          "Août", "Septembre", "Octobre", "Novembre", "Décembre")
times <- rep(1:12, times = length(colnames(Prévision_mutli)))
Prévision_mutli_bonne <- Prévision_mutli_bonne  |> 
  mutate(Times = times)

#Visualisation des diférentes prévisions
ggplot(Prévision_mutli_bonne, aes(x = Times, y = Prévision, color = Methodes)) +
  geom_line() +
  labs(title = "Prévisions de modéles multvariées",
       x = "Année 2023", y = "Prévisions de l'incidence des symptômes grippaux") +
  theme_minimal() + 
  scale_x_continuous(breaks = 1:12, labels = mois)







#Exportation des prévisions
#importation
autre <- read_excel("/Users/rododo/Desktop/BASE DE DONNEES USA/Database_prev.xlsx")
autre2 <- autre[-c(229:288),]
#concaténation des bases long et réorganisation de la base
Prévision_long <- rbind(Prévision_long_autres2[,1:2], Prévision_long_MLE2[,1:2])
Prévision_long$Date <- grippe_2023$Date
names(Prévision_long) <- c("source", "Incidence", "Date")
str(Prévision_long)
#concaténation base de données multivariées
Prévision_mutli_bonne2 <- Prévision_mutli_bonne[,1:2]
Prévision_mutli_bonne2$Date <- grippe_2023$Date
names(Prévision_mutli_bonne2) <- c("source", "Incidence", "Date")
str(Prévision_mutli_bonne2)

#concaténation finale
Database_prev2 <- rbind(autre2, Prévision_mutli_bonne2)
write_xlsx(Database_prev2,"Database_prev.xlsx")








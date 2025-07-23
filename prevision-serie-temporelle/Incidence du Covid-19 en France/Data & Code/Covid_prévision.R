#library#
library(ggplot2)
library(gridExtra)
library(tseries)
library(moments)
library(knitr)
library(nortest)
library(VGAM)
library(forecast)
library(stlplus)
library(stR)
library(smooth)
library(imputeTS)
library(dsa)
library(tidyverse)
library(lgarch)		
library(gets)
library(seasonal)
library(forecast)
library(dplyr)
library(tidyr)
library(BeSS)

#Importation de la base de données#
covid <- read.csv("/Users/rododo/Desktop/ECAP/Technique de prévision/Covid/memoirefi.csv")

#Bon typage des données#
covid$date <- as.Date(covid$date)
col_num = c(2:9)
covid[, col_num] <- lapply(covid[, col_num], function(x) as.numeric(gsub(",", ".", x)))
str(covid)

#Creation d'une var unique Y#
y <- covid$tx_incid

#Graphiques
plot_tx_incid = ggplot(covid, aes(x = date, y = tx_incid, color = tx_incid )) +
  geom_line() +
  theme_classic() + ggtitle("Taux d'incidence du COVID19") +
  xlab("Date") + ylab("Taux d'incidence") +
  scale_color_gradient(low = "blue", high = "red", name = "Tx incidence")

# Ajoutons des lignes verticales et annotations pour des événements marquants
plot_tx_incid = plot_tx_incid +
  geom_vline(xintercept = as.Date("2021-12-27"), linetype="dashed", color = "red") + # pass sanitaire
  geom_vline(xintercept = as.Date("2021-12-01"), linetype="dashed", color = "blue") + # 5eme vague 
  geom_vline(xintercept = as.Date("2021-01-04"), linetype="dashed", color = "purple") + # début vax 
  geom_vline(xintercept = as.Date("2022-02-01"), linetype="dashed", color = "purple") + # levée restrictions 
  geom_vline(xintercept = as.Date("2021-04-03"), linetype="dashed", color = "black") + # début 3ème confinement  
  geom_vline(xintercept = as.Date("2021-05-03"), linetype="dashed", color = "black") + # fin 3ème confinement  
  annotate("text", x = as.Date("2021-12-27"), y = max(covid$tx_incid, na.rm = TRUE), label = "Pass sanitaire", vjust = -0.5) +
  annotate("text", x = as.Date("2021-12-01"), y = max(covid$tx_incid, na.rm = TRUE), label = "5ème vague", vjust = 2, hjust=1.01)+
  annotate("text", x = as.Date("2021-01-04"), y = max(covid$tx_incid, na.rm = TRUE), label = "Début vaccination", hjust=0.1)+
  annotate("text", x = as.Date("2022-02-01"), y = max(covid$tx_incid, na.rm = TRUE), label = "Levée des restrictions", vjust = 0.9, hjust=-0.001)+
  annotate("text", x = as.Date("2021-04-20"), y = max(covid$tx_incid, na.rm = TRUE), label = "3ème confinement", vjust = 2)
plot_tx_incid  


#Vérification de la stationnarité de la série#
#Stationnarisation
#ADF#
adf_test <- adf.test(y)
print(adf_test)
#stationnaire

#Statistique descriptives#
summary_y <- summary(y)
sd_y <- sd(y)
var_y <- var(y)
table_summary <- data.frame(
  Statistique = c("Minimum", "1er quartile", "Médiane", "Moyenne", "3ème quartile", "Maximum", "Écart-type", "Variance"),
  Valeur = c(paste(summary_y[1], collapse = " - "), paste(summary_y[2], collapse = " - "), paste(summary_y[3], collapse = " - "),
             paste(summary_y[4], collapse = " - "), paste(summary_y[5], collapse = " - "), paste(summary_y[6], collapse = " - "),
             sd_y, var_y)
)
kable(table_summary, align = "c", caption = "Résumé statistique de la variable y")
#représentation de la série#
boxplot(y, xlab = "Taux l'incidence du covid", 
        main = "Taux de l'incidence du covid", 
        col = '#b4a7d6', cex.main=0.85)
#distribution#
kurtosis_y <- kurtosis(y)
skewness_y <- skewness(y)
#normalité#
shapiro.test(y)
shapiro_test <- shapiro.test(y)

table_results <- data.frame(
  Statistique = c("Kurtosis", "Skewness", "Shapiro-Wilk stat value","Shapiro-Wilk p-value"),
  Valeur = c(kurtosis_y, skewness_y,shapiro_test$statistic ,shapiro_test$p.value)
)
kable(table_results, align = "c", caption = "Résultats des statistiques de kurtosis, skewness et test de normalité Shapiro-Wilk pour la variable y")

#Visualisation des données et de la distribution#
g <- y
g_normalized <- (g - min(g)) / (max(g) - min(g))
hist_density <- density(g_normalized)
exp_density <- dexp(hist_density$x, rate = 1/mean(g_normalized))
norm_density <- dnorm(hist_density$x, mean = mean(g_normalized), sd = sd(g_normalized))
cauchy_density <- dcauchy(hist_density$x, location = mean(g_normalized), scale = sd(g_normalized))
weibull_density <- dweibull(hist_density$x, shape = 2, scale = 1)
log_normal_density <- dlnorm(hist_density$x, meanlog = mean(log(g_normalized)), sdlog = sd(log(g_normalized)))
laplace_density <- dlaplace(hist_density$x, location = mean(g_normalized), scale = sd(g_normalized))

palette <- c("#b4a7d6", "#0000FF", "#FF0000", "#00FF00", "#FFFF00", "#FFA500", "#800080")

plot(hist_density, main = "Comparaison des distributions", xlab = "Valeurs", ylab = "Densité", col = palette[1], lwd = 2, ylim = c(0, max(hist_density$y, exp_density)), xlim = c(0:1))
lines(hist_density$x, exp_density, col = palette[2], lwd = 2, lty = "dashed")
lines(hist_density$x, norm_density, col = palette[3], lwd = 2, lty = "dashed")
lines(hist_density$x, cauchy_density, col = palette[4], lwd = 2, lty = "dashed")
lines(hist_density$x, weibull_density, col = palette[5], lwd = 2, lty = "dashed")
lines(hist_density$x, log_normal_density, col = palette[6], lwd = 2, lty = "dashed")
lines(hist_density$x, laplace_density,col = palette[7], lwd = 2, lty = "dashed")

lines(hist_density, col = palette[1], lwd = 4)

legend("topright", legend = c("Yt Normalisé","Distribution Expo","Distribution Normal"," Distribution Cauchy"," Distribution Weibull", "Distribution Log-normal", "Distribution Laplace"),
       col = palette, lty = c(1, rep(2, 6)),cex = 0.6,text.width = 0.09)

#présentation graphique des X#
#incidence hospitalisation#
plot_incid_hosp <- ggplot(covid, aes(x = date, y = incid_hosp, color = incid_hosp)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() + 
  ggtitle("Nombre de nouveaux patients hospitalisés au cours des dernières 24h") +
  xlab("Date") + 
  ylab("Incidence de nouveaux patients hospitalisés sur 24h") 
plot_incid_hosp


#incidence réanimation#
plot_incid_rea <- ggplot(covid, aes(x = date, y = incid_rea, color = incid_rea)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() + 
  ggtitle("Nombre de nouveaux patients admis en réanimation au cours des dernières 24h") +
  xlab("Date") + 
  ylab("Incidence de nouveaux patients hospitalisés sur 24h") 
plot_incid_rea


#incidence retour à domicile#
plot_incid_rad <- ggplot(covid, aes(x = date, y = incid_rad, color = incid_rad)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() + 
  ggtitle("Nombre de nouveaux patients hospitalisés au cours des dernières 24h") +
  xlab("Date") + 
  ylab("Incidence de nouveaux patients hospitalisés sur 24h") 
plot_incid_rad

#Décés à l'hopital#
plot_incid_dchosp <- ggplot(covid, aes(x = date, y = incid_dchosp, color = incid_dchosp)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() + 
  ggtitle("Nombre de nouveaux patients hospitalisés au cours des dernières 24h") +
  xlab("Date") + 
  ylab("Incidence de nouveaux patients hospitalisés sur 24h") 
plot_incid_dchosp

#Nombre de patient ayant reçu la premiére dose#
plot_dose1 <- ggplot(covid, aes(x = date, y = dose1, color = dose1)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() + 
  ggtitle("Nombre de nouveaux patients hospitalisés au cours des dernières 24h") +
  xlab("Date") + 
  ylab("Incidence de nouveaux patients hospitalisés sur 24h") 
plot_dose1

#Nombre de patient ayant reçu la deuxiéme dose#
plot_dose2 <- ggplot(covid, aes(x = date, y = dose2, color = dose2)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() + 
  ggtitle("Nombre de nouveaux patients hospitalisés au cours des dernières 24h") +
  xlab("Date") + 
  ylab("Incidence de nouveaux patients hospitalisés sur 24h") 
plot_dose2

#Nombre de patient ayant reçu la troisiéme dose#
plot_dose3 <- ggplot(covid, aes(x = date, y = dose3, color = dose3)) +
  geom_line() +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() + 
  ggtitle("Nombre de nouveaux patients hospitalisés au cours des dernières 24h") +
  xlab("Date") + 
  ylab("Incidence de nouveaux patients hospitalisés sur 24h") 
plot_dose3

#Visualisation de la vaccination#
covid_long <- tidyr::pivot_longer(covid, cols = starts_with("dose"), names_to = "Dose", values_to = "Nombre")

ggplot(covid_long, aes(x = date, y = Nombre, color = Dose)) +
  geom_line() +
  theme_classic() +
  ggtitle("Vaccination de la population par dose") +
  xlab("Date") + ylab("Nombre de patients vaccinés") +
  scale_color_manual(values = c("red", "blue", "green"))

#Spération base de donnée#
Base_décembre = covid[c(700:730),]
covid = covid[-c(700:730),]


#Test ADF de nos séries brute#
dlbase <- covid
adf_results <- list(
  incid_hosp = adf.test(dlbase$incid_hosp),
  incid_rea = adf.test(dlbase$incid_rea),
  incid_rad = adf.test(dlbase$incid_rad),
  incid_dchosp = adf.test(dlbase$incid_dchosp),
  dose1 = adf.test(dlbase$dose1),
  dose2 = adf.test(dlbase$dose2),
  dose3 = adf.test(dlbase$dose3)
)
p_values <- sapply(adf_results, function(x) x$p.value)
adf_df <- data.frame(
  P_Value = p_values,
  Val_stat = sapply(adf_results,function(x) x$statistic)
  
)
kable(adf_df)

#Stationnarisation de nos séries différencier#
dX = data.matrix(covid[,3:9])
dX = diff(dX)
dX = data.frame(dX)

#Test ADF de nos séries différencié#
adf_results <- list(
  incid_hosp = adf.test(dX$incid_hosp),
  incid_rea = adf.test(dX$incid_rea),
  incid_rad = adf.test(dX$incid_rad),
  incid_dchosp = adf.test(dX$incid_dchosp),
  dose1 = adf.test(dX$dose1),
  dose2 = adf.test(dX$dose2),
  dose3 = adf.test(dX$dose3)
)

p_values <- sapply(adf_results, function(x) x$p.value)
adf_df <- data.frame(
  P_Value = p_values,
  Val_stat = sapply(adf_results,function(x) x$statistic)
  
)
kable(adf_df)
#Choix des variables utiles par la méthode BeSS#
#Importation de la base de données non différencier#
#Modèle initiale#
y <- covid$tx_incid
x <- cbind(covid$incid_hosp,covid$incid_rea,covid$incid_rad, covid$incid_dchosp, covid$dose1, covid$dose2, covid$dose3)
fitbess <- bess(x, y, family = "gaussian")
bestmodel <- fitbess$bestmodel
summary(bestmodel)

#Modéle ARX#
X = data.matrix(covid[,3:9])
dX = diff(X)

covid <- data.frame(covid) 
training_covid <- data.frame(covid)
training_dlbase <- data.frame(covid)

# ARX model with AR(1:5)
Model01 <- arx(training_covid$tx_incid, mc = TRUE, ar = 1:5, mxreg = X[, 1:7], vcov.type = "ordinary")
getsm <- getsm(Model01, arch.LjungB=NULL) 
getsm

# ARX with auto.arima
modelarx=auto.arima(covid$tx_incid, p=1, max.q = 0, xreg = X[, 1:6], seasonal=FALSE, stationary=TRUE)
modelarx
modelarx$coef
modelarx$var.coef
j <- ncol(modelarx$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelarx$coef[i]/sqrt(modelarx$var.coef[i,i])
}
tstat

# ARX with auto.arima modified
modelarx2=auto.arima(covid$tx_incid, max.q = 0, xreg = X[, c(1:4, 6)], seasonal=FALSE, stationary=TRUE)
modelarx2
modelarx2$coef
modelarx2$var.coef
j <- ncol(modelarx2$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelarx2$coef[i]/sqrt(modelarx2$var.coef[i,i])
}
tstat

# ARMAX(1,0,0) without auto.arima
modelx1=Arima(covid$tx_incid,order=c(1,0,0), xreg = X[, 1:6], seasonal=FALSE, include.mean=FALSE)
modelx1
modelx1$coef
modelx1$var.coef
j <- ncol(modelx1$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx1$coef[i]/sqrt(modelx1$var.coef[i,i])
}
tstat #On enleve les valeurs absolues inféfieures à 1,64

modelx2=Arima(covid$tx_incid,order=c(1,0,0), xreg = X[, c(1, 3:4)], seasonal=FALSE, include.mean=FALSE)
modelx2
modelx2$coef
modelx2$var.coef
j <- ncol(modelx2$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx2$coef[i]/sqrt(modelx2$var.coef[i,i])
}
tstat

modelx3=Arima(covid$tx_incid,order=c(1,0,0), xreg = X[, c(1, 3)], seasonal=FALSE, include.mean=FALSE)
modelx3
modelx3$coef
modelx3$var.coef
j <- ncol(modelx3$var.coef)
tstat <- matrix(nrow=j, ncol=1)
for(i in 1:j)
{
  tstat[i,1] <- modelx3$coef[i]/sqrt(modelx3$var.coef[i,i])
}
tstat
y

#Prevision daily data#
# 1 - METHODE DE LISSAGE EXPONENTIEL#
# ETS model
fit1 <- ets(y)
plot(fit1, xlab = "Temps", col = "#b4a7d6", lwd = 2)
#prévision#
fit1_prev <- forecast(fit1,31)
show(fit1_prev)
plot(fit1_prev, main="Prévision avec modèle ETS",
     xlab="Date", ylab="Valeur")

#TBATS#
yy <- msts(y, seasonal.periods=c(7,365.25))
fittbats = tbats(yy)
plot(fittbats,xlab = "Temps", col = "#b4a7d6", lwd = 2 )
#Pas de saisonnalité donc pas de T#
#prevision#
fittbats_prev <- forecast(fittbats,31)
show(fittbats_prev)
plot(fittbats_prev, main="Prévision avec modèle TBATS",
     xlab="Date", ylab="Valeur")

# 2 - SARIMA MODELE#
# SARIMA model
fitarima <- auto.arima(y)
fitarima
plot(fitarima, main="Racine inversé AR", xlab="Partie réelle", ylab="Partie imaginaire")
#prevision#
fitarima_prev <- forecast(fitarima,31)
show(fitarima_prev)
plot(fitarima_prev, main="Prévision avec modèle SARIMA",
     xlab="Date", ylab="Valeur")

#3 - ADAM ETS ARIMA models
#1#
fitadam1 <- auto.adam(y, model="ZZZ", lags=c(1,7), select=TRUE)
fitadam1
#prevision#
fitadam1_prev <- forecast(fitadam1,31)
show(fitadam1_prev)
plot(fitadam1_prev, main="Prévision avec modèle auto.adam",
     xlab="Date", ylab="Valeur")

#2#
fitadam2 <- auto.adam(y, model="ZZZ", lags=c(1,1,7), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fitadam2
#prevision#
fitadam2_prev <- forecast(fitadam2,31)
show(fitadam2_prev)
plot(fitadam2_prev, main="Prévision avec modèle auto.adam",
     xlab="Date", ylab="Valeur")

#4 - CES models#
fitces <- auto.ces(y, models=c("n","s","p","f"))
fitces
#prevision#
fitces_prev <- forecast(fitces,31)
show(fitces_prev)
plot(fitces_prev)

#5 - SSARIMA#
fitssarima <- auto.ssarima(y, lags=c(1,7), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fitssarima
#prevision#
fitssarima_prev <- forecast(fitssarima,31)
show(fitssarima_prev)
plot(fitssarima_prev)

#ARX#
trainingbase <- covid[1:699,]
testingbase <- Base_décembre
observedbase <- covid[700:730,1]
forecast <- NULL

y <- trainingbase[,2]
varx <- ts(trainingbase[,c(3,5)])
testingbase1 <- testingbase[,c(3,5)]


# ARMAX without auto.arima modified
# First check to obtain the same auto.arima model with ARX(1,0,0) without auto.arima
modelx11=Arima(y,order=c(1,0,0), xreg = varx,
               seasonal=FALSE, include.mean=FALSE)
f1= predict(modelx11, newxreg = testingbase1, n.ahead = 31)
plot(f1$pred, type='l', main='Prévisions ARX(1,0,0)', xlab='Temps', ylab='Prédiction')

#Graphique de prévision#
Prévisions <- data.frame(
  ETS = fit1_prev$mean,
  TBATS = fittbats_prev$mean,
  ARIMA = fitarima_prev$mean,
  ADAM_ETS_ARIMA_1 = fitadam1_prev$mean,
  ADAM_ETS_ARIMA_2 = fitadam2_prev$mean,
  ADAM_CES = fitces_prev$mean,
  SSARIMA = fitssarima_prev$mean,
  OBS = Base_décembre$tx_incid,
  ARMAX = f1$pred)

#Transformation en format long#
Prévision_long <- gather(Prévisions, key = "Methodes", value = "Prévision")
# Ajout de la colonne "times" à Prévision_long
times <- rep(1:31, times = length(colnames(Prévisions)))
Prévision_long <- Prévision_long %>%
  mutate(Times = times)

#veteur de couleurs#
Couleurs <- replicate(length(times), rgb(sample(0:255, 1), sample(0:255, 1), sample(0:255, 1), maxColorValue = 255))

ggplot(Prévision_long, aes(x = Times, y = Prévision, color = Methodes, linetype = ifelse(Methodes == "OBS", "OBS", "other"))) +
  geom_line() +
  labs(title = "Comparaison des prévisions des différents modèles et de la série réelle",
       x = "Temps", y = "Prévisions") +
  scale_color_manual(values = Couleurs) +
  scale_linetype_manual(values = c("OBS" = "solid", "other" = "dashed"), guide = FALSE) +
  theme_minimal() +
  theme(legend.position = "right")


#Calcul des MSE#
MSE <- data.frame(
  ETS_mse = ((Base_décembre$tx_incid - fit1_prev$mean)^2),
  TBATS_mse = ((Base_décembre$tx_incid - fittbats_prev$mean)^2),
  ARIMA_mse = ((Base_décembre$tx_incid - fitarima_prev$mean)^2),
  ADAM_ETS_ARIMA_1_mse = ((Base_décembre$tx_incid - fitadam1_prev$mean)^2),
  ADAM_ETS_ARIMA_2_mse = ((Base_décembre$tx_incid - fitadam2_prev$mean)^2),
  ADAM_CES_mse = ((Base_décembre$tx_incid - fitces_prev$mean)^2),
  SSARIMA_mse = ((Base_décembre$tx_incid - fitssarima_prev$mean)^2),
  ARMAX_mse = ((Base_décembre$tx_incid - f1$pred)^2)
)
MSE_long <- gather(MSE, key = "Methodes", value = "MSE")
times <- rep(1:31, times = length(colnames(MSE)))
MSE_long <- MSE_long %>%
  mutate(Times = times)

MSE_total <- data.frame(ETS = mean(MSE$ETS_mse),
                        TBATS = mean(MSE$TBATS_mse),
                        ARIMA = mean(MSE$ARIMA_mse),
                        ADAM_ETS_ARIMA_1 = mean(MSE$ADAM_ETS_ARIMA_1_mse),
                        ADAM_ETS_ARIMA_2 = mean(MSE$ADAM_ETS_ARIMA_2_mse),
                        ADAM_CES = mean(MSE$ADAM_CES_mse),
                        SSARIMA = mean(MSE$SSARIMA_mse),
                        ARMAX = mean(MSE$ARMAX_mse))
kable(MSE_total, align = "c", caption = "MSE de chaque méthode")

#Visualisation#
ggplot(MSE_long, aes(x = Times, y = MSE, color = Methodes)) +
  geom_line() +
  labs(title = "Indicateur MSE pour chaque modéle de prévision",
       x = "Temps en jour", y = "Valeur de l'indicateur") +
  scale_color_manual(values = Couleurs) +
  theme_minimal() +
  theme(legend.position = "right")

#Autre indicateur#
#1 - R200S#
R2OOS <- data.frame(
  ETS_R2OOS = 1 - sum((Base_décembre$tx_incid - fit1_prev$mean)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2),
  TBATS_R2OOS = 1 - sum((Base_décembre$tx_incid - fittbats_prev$mean)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2),
  ARIMA_R2OOS = 1 - sum((Base_décembre$tx_incid - fitarima_prev$mean)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2),
  ADAM_ETS_ARIMA_1_R2OOS = 1 - sum((Base_décembre$tx_incid - fitadam1_prev$mean)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2),
  ADAM_ETS_ARIMA_2_R2OOS = 1 - sum((Base_décembre$tx_incid - fitadam2_prev$mean)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2),
  ADAM_CES_R2OOS = 1 - sum((Base_décembre$tx_incid - fitces_prev$mean)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2),
  SSARIMA_R2OOS = 1 - sum((Base_décembre$tx_incid - fitssarima_prev$mean)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2),
  ARMAX_R2OOS = 1 - sum((Base_décembre$tx_incid - f1$pred)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2)
)
kable(R2OOS, align = "c", caption = "R^2OOS de chaque méthode")
1 - sum((Base_décembre$tx_incid - fit1_prev$mean)^2)/sum((Base_décembre$tx_incid - mean(Base_décembre$tx_incid))^2)
#2 - CSSDE#
CSSDE <- data.frame(
  ETS_CSSDE = ((Base_décembre$tx_incid - fit1_prev$mean)^2),
  TBATS_CSSDE = ((Base_décembre$tx_incid - fittbats_prev$mean)^2),
  ARIMA_CSSDE = ((Base_décembre$tx_incid - fitarima_prev$mean)^2),
  ADAM_ETS_ARIMA_1_CSSDE = ((Base_décembre$tx_incid - fitadam1_prev$mean)^2),
  ADAM_ETS_ARIMA_2_CSSDE = ((Base_décembre$tx_incid - fitadam2_prev$mean)^2),
  ADAM_CES_CSSDE = ((Base_décembre$tx_incid - fitces_prev$mean)^2),
  SSARIMA_CSSDE = ((Base_décembre$tx_incid - fitssarima_prev$mean)^2),
  ARMAX_CSSDE = ((Base_décembre$tx_incid - f1$pred)^2)
)
CSSDE_long <- gather(CSSDE, key = "Methodes", value = "CSSDE")
times <- rep(1:31, times = length(colnames(CSSDE)))
CSSDE_long <- CSSDE_long %>%
  mutate(Times = times)

CSSDE_total <- data.frame(ETS = sum(CSSDE$ETS_CSSDE),
                        TBATS = sum(CSSDE$TBATS_CSSDE),
                        ARIMA = sum(CSSDE$ARIMA_CSSDE),
                        ADAM_ETS_ARIMA_1 = sum(CSSDE$ADAM_ETS_ARIMA_1_CSSDE),
                        ADAM_ETS_ARIMA_2 = sum(CSSDE$ADAM_ETS_ARIMA_2_CSSDE),
                        ADAM_CES = sum(CSSDE$ADAM_CES_CSSDE),
                        SSARIMA = sum(CSSDE$SSARIMA_CSSDE),
                        ARMAX_CSSDE = sum(CSSDE$ARMAX_CSSDE))
kable(CSSDE_total, align = "c", caption = "CSSDE de chaque méthode")

#Test de précision
#Prévision AR(1)
y_ts <- ts(covid$tx_incid)
ar_1 <- Arima(y_ts, order=c(1,0,0))
ar_1_prev<- forecast(ar_1, h=31)
ar_1_prev$mean

#Prévision naïve
y_ts <- ts(covid$tx_incid)
naive_prev <- naive(y_ts,h=31)
naive_prev$mean

#Le test de Diebold-Mariano
#Base de données prévisions : 
Prévisions <- data.frame(
  ETS = fit1_prev$mean,
  TBATS = fittbats_prev$mean,
  ARIMA = fitarima_prev$mean,
  ADAM_ETS_ARIMA_1 = fitadam1_prev$mean,
  ADAM_ETS_ARIMA_2 = fitadam2_prev$mean,
  ADAM_CES = fitces_prev$mean,
  SSARIMA = fitssarima_prev$mean,
  OBS = Base_décembre$tx_incid,
  ARX = f1$pred,
  NAIVE = naive_prev$mean,
  AR_1 = ar_1_prev$mean)

# Le test de Diebold-Mariano nécessite des erreurs de prévision, pas des prévisions elles-mêmes
Prévisions$Erreur_ETS <- Prévisions$ETS - Prévisions$OBS
Prévisions$Erreur_TBATS <- Prévisions$TBATS - Prévisions$OBS
Prévisions$Erreur_ARIMA <- Prévisions$ARIMA - Prévisions$OBS
Prévisions$Erreur_ADAM_ETS_ARIMA_1 <- Prévisions$ADAM_ETS_ARIMA_1 - Prévisions$OBS
Prévisions$Erreur_ADAM_ETS_ARIMA_2 <- Prévisions$ADAM_ETS_ARIMA_2 - Prévisions$OBS
Prévisions$Erreur_ADAM_CES <- Prévisions$ADAM_CES - Prévisions$OBS
Prévisions$Erreur_SSARIMA <- Prévisions$SSARIMA - Prévisions$OBS
Prévisions$Erreur_ARX <- Prévisions$ARX - Prévisions$OBS
Prévisions$Erreur_NAIVE <- Prévisions$NAIVE - Prévisions$OBS
Prévisions$Erreur_AR_1 <- Prévisions$AR_1 - Prévisions$OBS

# Effectuer le test de Diebold-Mariano avec prévision naïve
dm.test(Prévisions$Erreur_NAIVE, Prévisions$Erreur_ETS, h = 1, power = 2)
dm.test(Prévisions$Erreur_NAIVE, Prévisions$Erreur_TBATS, h = 1, power = 2)
dm.test(Prévisions$Erreur_NAIVE, Prévisions$Erreur_ARIMA, h = 1, power = 2)
dm.test(Prévisions$Erreur_NAIVE, Prévisions$Erreur_ADAM_ETS_ARIMA_1, h = 1, power = 2)
dm.test(Prévisions$Erreur_NAIVE, Prévisions$Erreur_ADAM_ETS_ARIMA_2, h = 1, power = 2)
dm.test(Prévisions$Erreur_NAIVE, Prévisions$Erreur_ADAM_CES, h = 1, power = 2)
dm.test(Prévisions$Erreur_NAIVE, Prévisions$Erreur_SSARIMA, h = 1, power = 2)
dm.test(Prévisions$Erreur_NAIVE, Prévisions$Erreur_ARX, h = 1, power = 2)

# Effectuer le test de Diebold-Mariano avec prévision AR(1)
dm.test(Prévisions$Erreur_AR_1, Prévisions$Erreur_ETS, h = 1, power = 2)
dm.test(Prévisions$Erreur_AR_1, Prévisions$Erreur_TBATS, h = 1, power = 2)
dm.test(Prévisions$Erreur_AR_1, Prévisions$Erreur_ARIMA, h = 1, power = 2)
dm.test(Prévisions$Erreur_AR_1, Prévisions$Erreur_ADAM_ETS_ARIMA_1, h = 1, power = 2)
dm.test(Prévisions$Erreur_AR_1, Prévisions$Erreur_ADAM_ETS_ARIMA_2, h = 1, power = 2)
dm.test(Prévisions$Erreur_AR_1, Prévisions$Erreur_ADAM_CES, h = 1, power = 2)
dm.test(Prévisions$Erreur_AR_1, Prévisions$Erreur_SSARIMA, h = 1, power = 2)
dm.test(Prévisions$Erreur_AR_1, Prévisions$Erreur_ARX, h = 1, power = 2)

# Afficher le résultat du test









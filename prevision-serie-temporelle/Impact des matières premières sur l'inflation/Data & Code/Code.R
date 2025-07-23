# Charger les fichiers
#1#
#balance_paie <- read_excel("Desktop/ECAP/Master 2/Série temporelle multi/DOSSIER/Balance - paie.xlsx", 
#                          sheet = "valeurs_mensuelles", col_names = FALSE, 
#                           skip = 4)
#balance_paie <- balance_paie %>%
#  rename(Time = `...1`,Balance = `...2`)
#2#
#mat_1er <- read_excel("Desktop/ECAP/Master 2/Série temporelle multi/DOSSIER/Mat-1er.xlsx", 
#                           sheet = "valeurs_mensuelles", col_names = FALSE, 
#                           skip = 4)
#mat_1er <- mat_1er %>%
#  rename(Time = `...1`,Matiere = `...2`)
#3#
#prix_conso <- read_excel("Desktop/ECAP/Master 2/Série temporelle multi/DOSSIER/prix-conso.xlsx", 
#                      sheet = "valeurs_mensuelles", col_names = FALSE, 
#                      skip = 4)
#prix_conso <- prix_conso %>%
#  rename(Time = `...1`,Prix = `...2`)
#4#
#cac40_data <- read_csv("Desktop/ECAP/Master 2/Série temporelle multi/DOSSIER/CAC 40 Historical Data.csv")
#cac40_data <- cac40_data[,1:2]
#cac40_data <- cac40_data %>%
#  rename(Time = `Date`,Bourse = `Price`)
#Bon typage des dates 
#1#
#balance_paie <- balance_paie %>%
#  mutate(
#    Time = as.Date(paste0(Time, "-01"), format = "%Y-%m-%d"),
#    Time = format(Time, "%d/%m/%Y")
#  )
#2#
#mat_1er <- mat_1er %>%
#  mutate(
#    Time = as.Date(paste0(Time, "-01"), format = "%Y-%m-%d"),
#    Time = format(Time, "%d/%m/%Y")
#  )
#3#
#prix_conso <- prix_conso %>%
#  mutate(
#    Time = as.Date(paste0(Time, "-01"), format = "%Y-%m-%d"),  
#    Time = format(Time, "%d/%m/%Y")  
#  )
#4#
#cac40_data <- cac40_data %>%
#  mutate(
#    Time = mdy(Time),  # Convertir en objet Date, en supposant que la date est au format "MM/JJ/AAAA"
#    Time = format(Time, "%d/%m/%Y")  # Reformater en "JJ-MM-AAAA"
#  )
#Jointure des datas
# Joindre les datasets selon les colonnes communes (adaptez selon vos données)
# Remplacez "key_column" par les colonnes communes appropriées
#Eco <- cac40_data %>%
#  inner_join(balance_paie, by = "Time") %>%
#  inner_join(mat_1er, by = "Time") %>%
#  inner_join(prix_conso, by = "Time")

# Sauvegarder le fichier fusionné
#write.csv(Eco, "Desktop/ECAP/Master 2/Série temporelle multi/DOSSIER/Data_eco.csv", row.names = FALSE)

#Importation de la data finale
library(readxl)
library(dplyr)
library(readr)
Bourse <- read_csv("~/Desktop/ECAP/Master 2/Série temporelle multi/DOSSIER/Data_eco.csv")
#Nous enlevons les données à partir du 1 février 2020 date ou à lieu le premier cas de covid en France
Bourse_pro <- Bourse[-c(1:52),]
#transformation de la base de donnée en time-series
#Bourse$Time <- as.Date.character(Bourse$Time)
Bourse_rev <- Bourse_pro[nrow(Bourse_pro):1, ]
Bourse_ts <- ts(Bourse_rev, start=c(2008,1), frequency = 12)
#visualisation de la data
library(TSstudio)
ts_plot(Bourse_ts[,2:5],type = "multiple",
        title = "Visualisation de l'ensemble des variables",
        Xtitle = "Temps",
        Ytitle = "Valeur",
        Xgrid = TRUE,
        Ygrid = TRUE)
#Suppresion de la colonne Time
Bourse_ts <- Bourse_ts[,-1]

#SAISONNALITE#
library(seastests)
for (i in 1:ncol(Bourse_ts)) {
  print(combined_test(Bourse_ts[, i]))  
  print(isSeasonal(Bourse_ts[, i], test="combined"))
}
#Bourse/matiére = NON Saisonnié
#Balance/Prix = saisonnié

#Désaisonnalisation des séries
#1 - Différenciation saisonnière#
library(forecast)
Bourse_ts[,"Prix"] <- seasadj(stl(ts(Bourse_ts[,"Prix"], frequency = 12), s.window = "periodic"))
#Vérification
plot(stl(ts(Bourse_ts[,"Prix"], frequency = 12), s.window = "periodic"))
combined_test(Bourse_ts[,"Prix"])
isSeasonal(Bourse_ts[,"Prix"], test="combined")
#2#
Bourse_ts[,"Balance"] <- seasadj(stl(ts(Bourse_ts[,"Balance"], frequency = 12), s.window = "periodic"))
#Vérification
combined_test(Bourse_ts[,"Balance"])
isSeasonal(Bourse_ts[,"Balance"], test="combined")
plot(stl(ts(Bourse_ts[,"Balance"], frequency = 12), s.window = "periodic"))

#Visualisation des séries désaisonnaliosées
ts_plot(Bourse_ts[,c("Prix","Balance")],type = "multiple",
        title = "Visualisation de la série désaisonnalisée",
        Xtitle = "Prix",
        Ytitle = "Valeur",
        Xgrid = TRUE,
        Ygrid = TRUE)
#OUTLIERS#
library(tsoutliers)
#Bourse
fit <- tso(Bourse_ts[,"Bourse"]) #Detection des points atypiques
show(fit$outliers)
plot(fit)
#3 outliers
#Bourse_ts_sai[,"Bourse"] <- fit$yadj
#Balance
fit2 <- tso(Bourse_ts[,"Balance"]) #Detection des points atypiques
show(fit2$outliers)
plot(fit2)
#1 outliers
#show(fit2)
#Bourse_ts_sai[,"Balance"] <- fit2$yadj
fit3 <- tso(Bourse_ts[,"Matiere"]) #Detection des points atypiques
show(fit3$outliers)
plot(fit3)
#2 outliers
#Bourse_ts_sai[,"Matiere"] <- fit3$yadj
#Bourse
fit4 <- tso(Bourse_ts[,"Prix"]) #Detection des points atypiques
show(fit4$outliers)
plot(fit4)
#3 outliers

#STATIONNARITE#
library(urca)
for (i in 1:ncol(Bourse_ts)) {
  m1 <- ar(diff(Bourse_ts[, i]), method = "mle")
  m1_order <- m1$order
  za.grp <- ur.za(Bourse_ts[,i],model = "both", lag = m1_order)
  print(i)
  print(za.grp@teststat)
  print(za.grp@cval)
  print(za.grp@bpoint)
}
#Bourse non stationnaire car V_stat < V_critique (ensemble)
#Balance stationnaire car V_stat > V_critique (ensemble)
#Matiére non stationnaire car V_stat < V_critique (ensemble)
#Prix non stationnaire car V_stat < V_critique (ensemble)

#AUTRE STATIONNARITE#
library(fUnitRoots)
library(tseries)
m1=ar(diff(Bourse_ts[,"Bourse"]),method="mle")
adfTest(Bourse_ts[,"Bourse"],lags=m1$order)
kpss.test(Bourse_ts[,"Bourse"], null = "Level")
#Bourse est non stationnaire Pvalue > 0.10

m2=ar(diff(Bourse_ts[,"Balance"]),method="mle") 
adfTest(Bourse_ts[,"Balance"],lags=m2$order)
kpss.test(Bourse_ts[,"Balance"], null = "Level")
#Balance est stationnaire I(0) Pvalue < 0.05

m3=ar(diff(Bourse_ts[,"Matiere"]),method="mle") 
adfTest(Bourse_ts[,"Matiere"],lags=m3$order)
kpss.test(Bourse_ts[,"Matiere"], null = "Level")
#Matieres est non stationnaire Pvalue > 0.10

m4=ar(diff(Bourse_ts[,"Prix"]),method="mle") 
adfTest(Bourse_ts[,"Prix"],lags=m4$order)
kpss.test(Bourse_ts[,"Prix"], null = "Level")
#Prix est non stationnaire Pvalue > 0.10

#Nous décidons d'exclure la variable balance car pouvant biais notre analyse dans le VECM
#car étant mieux modéliser avec un VAR cependant l'intégré comme variable exogéne pourrait 
#être intérresant

Bourse_ts_3 <- Bourse_ts[,c(1,3,4)]

#Vérification de présence ou non de la co-intégration
library(vars)
#Selection du nombre de retards 
VARselect(Bourse_ts_3, lag.max = 12, type = "both")
#Choix de 2 retards

#CO-INTEGRATION#
#Nombre de lag = 2
# 1er Méthode - Trace & Eigen
H1.trace <- summary(ca.jo(Bourse_ts_3, K=2, type=c("trace")))
H1.eigen <- summary(ca.jo(Bourse_ts_3, K=2, type=c("eigen")))
H1.trace #Il existe au moins 1 relation de co-intégration car Vtest < Vcritique
H1.eigen #Il existe au moins 1 relation de co-intégration car Vtest < Vcritique

# 2eme Méthode - Johansen-Juselius 
vecm <- ca.jo(Bourse_ts_3, K=2, type=c("eigen", "trace"), ecdet = "trend")
jo.results <- summary(vecm)
jo.results #Il existe au moins 1 relation de co-intégration car Vtest < Vcritique
#Nous préférons le VECM au VAR ainsi, nous pouvons régarder les stats

#STATISTIQUE DESCRIPTIVES
#stats
summary(Bourse_ts_3)
#variance
apply(Bourse_ts_3, 2, var)
apply(Bourse_ts_3, 2, sd)
#distribution
library(e1071)
apply(Bourse_ts_3, 2, skewness)
apply(Bourse_ts_3, 2, kurtosis)
#Standardisation des données pour les visualisés
data_standardized <- as.data.frame(scale(Bourse_ts_3))
data_long <- reshape2::melt(data_standardized)
library(ggplot2)
ggplot(data_long, aes(x = value, color = variable, fill = variable)) +
  geom_density(alpha = 0.4) +
  stat_function(fun = dnorm, args = list(mean = mean(data_long$value), sd = sd(data_long$value)), color = "black", linetype = "dashed") +
  labs(
    title = "Distributions Standardisées des Colonnes",
    x = "Valeurs Standardisées",  # Titre pour l'axe des X
    y = "Densité"                # Titre pour l'axe des Y
  )
#corrélation 
library(corrplot)
corrplot(cor(Bourse_ts_3), method = "color", type = "upper", 
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

#APPLICATION DU VECM 
library(tsDyn)
#Spécification
# Cas des differentes spécifications des modèles VECM : 
# Cas 1 include = 'none'
# Cas 2 LRinclude = 'const'
# Case 3 include = 'const'
# Cas 4 LRinclude = 'trend'
# Case 5 include = 'both'
#1#
suppressWarnings
({library(tsDyn)
  M1 <- VECM(Bourse_ts_3, lag = 2, estim = "ML", include = 'none')
  summary(rank.test(M1))
})
#2#
suppressWarnings
({library(tsDyn)
  M2 <- VECM(Bourse_ts_3, lag = 2, estim = "ML", LRinclude = 'const')
  summary(rank.test(M2))
})
#3#
suppressWarnings
({library(tsDyn)
  M3 <- VECM(Bourse_ts_3, lag = 2, estim = "ML", include = 'const')
  summary(rank.test(M3))
})
#4#
suppressWarnings
({library(tsDyn)
  M4 <- VECM(Bourse_ts_3, lag = 2, estim = "ML", LRinclude = 'trend')
  summary(rank.test(M4))
})
#5#
suppressWarnings
({library(tsDyn)
  M5 <- VECM(Bourse_ts_3, lag = 2, estim = "ML", include = 'both')
  summary(rank.test(M5))
})
#Determination du modéle le plus adéquate
Compa <- data.frame(
  AIC = c(AIC(M1), AIC(M2), AIC(M3), AIC(M4), AIC(M5)),
  BIC = c(BIC(M1), BIC(M2), BIC(M3), BIC(M4), BIC(M5)),
  LogLik = c(logLik(M1), logLik(M2), logLik(M3), logLik(M4), logLik(M5)),  # Log-vraisemblance
  RMSE = c(sqrt(mean(residuals(M1)^2)), sqrt(mean(residuals(M2)^2)), sqrt(mean(residuals(M3)^2)),
           sqrt(mean(residuals(M4)^2)), sqrt(mean(residuals(M5)^2)))  # RMSE
)
print(Compa)
#Pour l'AIC / le BIC / le modèle 2 est le meilleur
#Pour le Loglik / le RMSE le modéle 5


M2$coefficients
pred_M2 <- predict(M2, n.ahead = 20)
ee2 = rbind(Bourse_ts_3, pred_M2)
ee2_plot <- ts(ee2, start=c(2008,1), frequency = 12)
ts_plot(ee2_plot,type = "multiple",
        title = "Visualisation de l'ensemble des variables",
        Xtitle = "Temps",
        Ytitle = "Valeur",
        Xgrid = TRUE,
        Ygrid = TRUE)

#Modèle finale le plus adéquate
#relation de co-intégration
trace.2 <- ca.jo(Bourse_ts_3, type = "trace", ecdet = "const",K = 2)  
summary(trace.2)
#valeur propore
eigen.2 <- ca.jo(Bourse_ts_3, type = "eigen", ecdet = "const",K = 2)  
summary(eigen.2)
#Estimation du modéle
vecm <- ca.jo(Bourse_ts_3[, c("Bourse", "Matiere", "Prix")], type = "eigen",ecdet = "const", K = 2)
plot(vecm)
vecm.r1 <- cajorls(vecm, r = 1)
#résultats des estimations de co-intégration estimé
summary(vecm.r1$rlm)
#Matrice Alpha
alpha <- coef(vecm.r1$rlm)[1, ]
alpha
#Matrice Beta
beta <- vecm.r1$beta
beta
#Résidus
resids <- resid(vecm.r1$rlm)
N <- nrow(resids)
sigma <- crossprod(resids)/N
sigma
#La vitesse d'ajustement
#Alpha
alpha.se <- sqrt(solve(crossprod(cbind(vecm@ZK %*% beta, 
                                       vecm@Z1)))[1, 1] * diag(sigma))
alpha.t <- alpha/alpha.se
print(rbind(alpha, alpha.t))
#Beta
beta.se <- sqrt(diag(kronecker(solve(crossprod(vecm@RK[,-1])), 
                               solve(t(alpha) %*% solve(sigma) %*% alpha))))
beta.t <- c(NA, beta[-1]/beta.se)
print(t(cbind(beta, beta.t)))
#normalité des résidus
library(tseries)
#1 - test
#Normalité des résidus pas grave
jarque.bera.test(resids[,"Bourse.d"]) #Pvalue > 0.05 donc normalité des résidus
jarque.bera.test(resids[,"Matiere.d"]) #Pvalue < 0.05 donc pas normalité des résidus
jarque.bera.test(resids[,"Prix.d"]) #Pvalue < 0.05 donc normalité des résidus 
#Autocorrélation
#2 - test
Box.test(resids[,"Bourse.d"],type = "Ljung-Box") #Pvalue > 0.05 donc pas d'autocorrélation
Box.test(resids[,"Matiere.d"],type = "Ljung-Box") #Pvalue > 0.05 donc pas d'autocorrélation
Box.test(resids[,"Prix.d"],type = "Ljung-Box") #Pvalue > 0.05 donc pas d'autocorrélation
#TEST DE FAIBLE EXOGENEITE#
#Utilisation de restriction
# 1-BOURSE qui est exclut de la relation#
A1 <- matrix(c(0, 0, 1, 0, 0, 1), nrow = 3,ncol = 2, byrow = TRUE)
#2 2-MATIERE qui est exclut de la relation#
A2 <- matrix(c(1, 0, 0, 0, 0, 1), nrow = 3,ncol = 2, byrow = TRUE)
# 3-PRIX qui est exclut de la relation#
A3 <- matrix(c(1, 0, 0, 1, 0, 0), nrow = 3,ncol = 2, byrow = TRUE)
#test
summary(alrtest(z = vecm, A = A1, r = 1)) #Les restrictions sont cohérente (0.63)
summary(alrtest(z = vecm, A = A2, r = 1)) #Les restrictions sont presque cohérente (0.09)
summary(alrtest(z = vecm, A = A3, r = 1)) #Les restrictions ne sont pas cohérentes (0)
#Ainsi la relation à LT soit entre la bourse et matiére
#Les deux restrictions peuvent être interprété

#TEST DE SIGNE#
# 1-BOURSE#
B1 <- matrix(c(1, 0, 0, -1, 0, 0, 0, 1, 0, 0, 0, 1), nrow = 4, ncol = 3, byrow = TRUE)
# 2-MATIERE#
B2 <- matrix(c(1, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0, 1), nrow = 4, ncol = 3, byrow = TRUE)
# 3-PRIX#
B3 <- matrix(c(1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, -1), nrow = 4, ncol = 3, byrow = TRUE)
#test
summary(blrtest(z = vecm, H = B1, r = 1)) #nous rejetons la restriction du signe (0)
summary(blrtest(z = vecm, H = B2, r = 1)) #nous rejetons la restriction du signe (0)
summary(blrtest(z = vecm, H = B3, r = 1)) #nous rejetons la restriction du signe (0)
#Aucune matrice de valide
#Nous interprétons alors que les restrictions sur alpha
#Estimation du VECM sous-contrainte d'alpha uniquement
# Matrice 2#
vecm2 <- alrtest(z = vecm, A = A2, r = 1)
#Encore une constante à trouver à imposer
# VECM comme VAR en niveau
vecm.level <- vec2var(vecm, r=1)
#Attention pour ortho
#ortho = TRUE 
#Si vous avez une idée claire de l'ordre causal entre les variables.
#Si vous souhaitez interpréter les chocs comme indépendants et spécifiques à une variable donnée.
#ortho = FALSE 
#Si vous pensez que les chocs des variables sont naturellement corrélés.
#Si l'ordre des variables dans le modèle est incertain ou arbitraire.
#Fonction d'impulsion de Matiére
impulse_bou = vars::irf(vecm.level, impulse = "Bourse", response =c("Bourse"), 
                        n.ahead = 18, 
                        ortho = TRUE, 
                        cumulative = FALSE, 
                        boot = TRUE, 
                        ci = 0.95, 
                        runs = 100, 
                        seed = 123)
plot(impulse_bou)
#Fonction d'impulsion de Matiére
impulse_mat = vars::irf(vecm.level, impulse = "Bourse", response =c("Matiere"), 
                    n.ahead = 18, 
                    ortho = TRUE, 
                    cumulative = FALSE, 
                    boot = TRUE, 
                    ci = 0.95, 
                    runs = 100, 
                    seed = 123)
plot(impulse_mat)
#Fonction d'impulsion de Prix
impulse_prix = vars::irf(vecm.level, impulse = "Bourse", response =c("Prix"), 
                        n.ahead = 18, 
                        ortho = TRUE, 
                        cumulative = FALSE, 
                        boot = TRUE, 
                        ci = 0.95, 
                        runs = 100, 
                        seed = 123)
plot(impulse_prix)

#Décomposition de la variance
#FEVD
n=18
FEDV_vecm = vars::fevd(vecm.level,n.ahead = n)
FEDV_vecm$Bourse
FEDV_vecm$Matiere
FEDV_vecm$Prix
#Couleur
colors <- c("#80B17E","#D76E6E","#6B8EAE")
# Normalisation pour obtenir des pourcentages
library(reshape2)
FEVD_data <- prop.table(t(FEDV_vecm$Bourse), 2)
#Juste changer le nom de la variable aprés le pourcentage 
colnames(FEVD_data) <- paste0("Horizon ", 1:n)
rownames(FEVD_data) <- c("Indice du CAC40",
                         "indice des prix des matières première",
                         "indice des prix à la consommation")
FEVD_long <- melt(as.data.frame(FEVD_data), variable.name = "Horizon", value.name = "Percentage")
FEVD_long$Variable <- rep(rownames(FEVD_data), times = ncol(FEVD_data))
FEVD_long$Variable
# Visualisation graphique
ggplot(FEVD_long, aes(x = Horizon, y = Percentage, fill = Variable)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = colors, name = "Variables explicatives") +
  labs(
    title = "Décomposition de la variance : Indice Boursier",
    x = "Horizon",
    y = "Pourcentage"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14), # Centrage et taille du titre
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10), # Rotation des labels de l'axe X
    legend.title = element_text(size = 12), # Taille du titre de la légende
    legend.text = element_text(size = 10) # Taille du texte de la légende
  )

#Prévision
prev.U=predict(vecm.level)
plot(prev.U)










#LIBRAIRIES#
library(RColorBrewer)
library(gridExtra)
library(FactoMineR)
library(EnvStats)
library(outliers)
library(gvlma)
library(corrplot)
library(vcd)
library(dplyr)
library(cowplot)
library(psych)
library(sjPlot)
library(sjmisc)
library(PerformanceAnalytics)
library(lmtest)
library(car)
library(AER)
library(broom)
library(kableExtra)
library(ggplot2)
library(tidyverse)

#IMPORTATION ET NETTOYAGE DES DONNES#
STRESS = read.csv2("/Users/rododo/Desktop/ECAP/Regression linéaire/Le stress (bonne réponse) - Réponses au formulaire 1.csv")
new_names = c("HORODATEUR","GENRE","AGE","STATUT","PERSO","TRACAS","STRESS","SPORT","CULTU","ALIM",
              "TEL","SOM","CONF","EXIG","ECHEC","COMPA","ENTOURAGE","RESEAU","HABITAT","VISITE",
              "BAC?","ETUDE","HCOURS","PRESSION","JOB","TRAJET","AIDE","SOM.M","TEL.M"
)
colnames(STRESS) = new_names
STRESS_B = STRESS[,-c(1,11,12)]
S = na.omit(STRESS_B)

#TYPAGE DES VARIABLES#
str(S)

col_num = c("AGE", "STRESS", "SPORT","CONF", "EXIG", "HCOURS", "PRESSION", "TRAJET", "SOM.M","TEL.M")
S[, col_num] <- lapply(S[, col_num], function(x) as.numeric(gsub(",", ".", x)))
col_fac = setdiff(names(S), col_num)
S[,col_fac] = lapply(S[,col_fac],as.factor)
str(S)
col_fac

#RENOMMAGE DES VARIABLES QUALI#
nx_2 = c("NON", "PARFOIS","REGULIEREMENT","RAREMENT")
nx_3 = c("MAUVAISE", "NEUTRE", "SAINES")
nx_4 = c("PARFOIS","JAMAIS","RAREMENT","SOUVENT","TOUJOURS")
nx_5 = c("BOF","NON","OUI")
nx_6 = c("PARFOIS","CHEZ_PARENTS","RAREMENT","SOUVENT")
nx_7 = c("SANTE","S_ECO","S_TECHNO","S_HUM")
nx_8 = c("AGREABILITE","CONSCIENCE","EXTRAVERSION","OUVERTURE","SOUCIEUX")
levels(S$PERSO) = nx_8
levels(S$CULTU) = nx_2
levels(S$ALIM) = nx_3
levels(S$COMPA) = nx_4
levels(S$ENTOURAGE) =nx_5
levels(S$VISITE) = nx_6
levels(S$ETUDE) = nx_7


#STATISTIQUE DESCRIPTIVES DES DONNEES#
#ANALYSE UNIVARIES#

#QUANTI#
summary(S[,col_num])
round(sapply(S[,col_num],sd),2)

#QUANTI GRAPHIQUES
par(mar = c(5, 4, 4, 2) + 0.1)
par(mfrow = c(3, 3))
hist(S$AGE, main = "Distribution de la variable AGE", col="darkblue")
hist(S$SPORT, main = "Distribution de la variable SPORT", col="darkblue")
hist(S$CONF, main = "Distribution de la variable CONFIANCE EN SOI", col="darkblue")
hist(S$EXIG, main = "Distribution de la variable EXIGENCE ENVERS SOI", col="darkblue")
hist(S$HCOURS, main = "Distribution de la variable HEURE DE COURS", col="darkblue")
hist(S$PRESSION, main = "Distribution de la variable PRESSION ACADEMIQUE", col="darkblue")
hist(S$TRAJET, main = "Distribution de la variable TEMPS DE TRAJET", col="darkblue")
hist(S$SOM.M, main = "Distribution de la variable TEMPS DE SOMMEIL", col="darkblue")
hist(S$TEL.M, main = "Distribution de la variable TEMPS D'ECRAN", col="darkblue")
par(mfrow = c(1, 1))
hist(S$STRESS, main = "Distribution de la variable STRESS", col="blue")

#QUALI#
summary(S[,col_fac])

#CAMMEMBERT DE LA DISTRIBUTION DES MODALITES#
par(mfrow=c(1,1))
summary(S[,col_fac])
lapply(col_fac, function(variable) {
  freq_table <- table(S[[variable]])
  pastel_colors <- brewer.pal(length(freq_table), "Pastel1")
  percentages <- round(prop.table(freq_table) * 100, 1)
  pie(freq_table, main = paste("Diagramme de la répartition des individus pour", variable),
      col = pastel_colors, labels = paste(percentages, "%"), cex = 0.8)
  legend(x = 1, y = 0, legend = names(freq_table), title = "Légende", fill = pastel_colors,
         title.adj = 0.4, cex = 0.6, inset = c(0, 0))
})

#REGROUPEMENT DES MODALITES#

#BAC#
#NOUVELLE VAR AVEC DE NOUVEUX REGROUPEMENT#
S$`BAC_regroupe` <- as.character(S$`BAC?`)
indices_regroupement <- S$`BAC?` %in% c("Au delà de bac+5", "Bac+5")
S$`BAC_regroupe`[indices_regroupement] <- "Bac+5 et plus"
#RECODAGE DE LA VARIABLES#
S$`BAC_regroupe` <- factor(S$`BAC_regroupe`, levels = c("Bac+1", "Bac+2", "Bac+3", "Bac+4", "Bac+5 et plus"))
summary(S$`BAC_regroupe`)
S <- S[, -which(names(S) == 'BAC?')]

#RESEAU#
#NOUVELLE VAR AVEC DE NOUVEUX REGROUPEMENT#
S$RESEAU_REGROUPE = as.character(S$RESEAU)
indices_regroupement2 <- S$RESEAU %in% c("Autre", "Facebook")
S$RESEAU_REGROUPE[indices_regroupement2] <- "Autres"
#RECODAGE DE LA VARIABLES#
S$RESEAU_REGROUPE <- factor(S$RESEAU_REGROUPE, levels = c("Instagram", "Snapchat", "Tiktok", "X (twitter)", "Autres"))
summary(S$RESEAU_REGROUPE)
S <- S[, -which(names(S) == 'RESEAU')]

#COMPA#
#NOUVELLE VAR AVEC DE NOUVEUX REGROUPEMENT#
S$COMPA_REGROUPE = as.character(S$COMPA)
indices_regroupement3 <- S$COMPA %in% c("JAMAIS", "RAREMENT")
S$COMPA_REGROUPE[indices_regroupement3] <- "RAREMENT"
#RECODAGE DE LA VARIABLES#
S$COMPA_REGROUPE <- factor(S$COMPA_REGROUPE, levels = c("PARFOIS", "RAREMENT", "SOUVENT", "TOUJOURS"))
summary(S$COMPA_REGROUPE)
S <- S[, -which(names(S) == 'COMPA')]


#NOUVEAU CAMEMBERT DE LA DISTRIBUTION DES MODALITES#
col_fac2 <- names(S)[sapply(S, is.factor)]
lapply(col_fac2, function(variable) {
  freq_table <- table(S[[variable]])
  pastel_colors <- brewer.pal(length(freq_table), "Pastel1")
  percentages <- round(prop.table(freq_table) * 100, 1)
  pie(freq_table, main = paste("Diagramme de la répartition des indivius pour", variable),
      col = pastel_colors, labels = paste(percentages, "%"), cex = 0.8)
  legend(x = 1, y = 0, legend = names(freq_table), title = "Légende", fill = pastel_colors,
         title.adj = 0.4, cex = 0.6, inset = c(-0.00004, 0))
})

#RECHERCHE DE VALEURS ATYPIPIQUE DANS LES VAL NUM#

# AGE : Visuellement 4 valeurs atypiques => rosner
boxplot(S$AGE, xlab = 'Âge', 
        main = "Répartition de l'âge de notre échantillon", 
        col = 'orange', cex.main=0.85)
# SPORT : pas de valeurs atypiques
boxplot(S$SPORT,  xlab = 'Tréquence de sport dans la semaine', 
        main = "Répartition de la fréquence de sport dans la semaine de notre échantillon", 
        col = 'orange', cex.main=0.85)
# CONFIANCE EN SOI : pas de valeurs atypiques
boxplot(S$CONF,  xlab = 'Niveau de confiance en soi', 
        main = "Répartition du niveau de confiance en soi de notre échantillonx", 
        col = 'orange', cex.main=0.85)
# EXIGENCE DE SOI : Visuellement 2 valeurs atypiques => rosner
boxplot(S$EXIG,  xlab = 'Niveau d exigence de soi', 
        main = "Répartition du niveau d exigence de soi de notre échantillon", 
        col = 'orange', cex.main=0.85)
#HEURES COURS : pas de valeurs atypiques
boxplot(S$HCOURS,  xlab = 'Nombre d heures de cours', 
        main = "Répartition du nombre d heure de cours de notre échantillon", 
        col = 'orange', cex.main=0.85)
# PRESSION ACADEMIQUE : Visuellement 3 valeurs atypiques => rosner
boxplot(S$PRESSION ,  xlab = 'Niveau de pression académique perçu',
        main = "Répartition du niveau de pression académique perçu de notre échantillon", 
        col = 'orange', cex.main=0.85)
# TEMPS TRAJET : Visuellement 5 valeurs atypiques => rosner
boxplot(S$TRAJET,  xlab = 'Temps de trajet en minutes', 
        main = "Répartition du temps de trajet en minute de notre échantillon", 
        col = 'orange', cex.main=0.85)
# TEMPS DE SOMMEIL : Visuellement 1 valeurs atypique => grubbs
boxplot(S$SOM.M ,  xlab = 'Temps de sommeil en minutes', 
        main = "Répartition du temps de sommeil en minute de notre échantillon", 
        col = 'orange', cex.main=0.85)
# TEMPS ECRAN : Visuellement 6 valeurs atypiques => rosner
boxplot(S$TEL.M,  xlab = 'Temps de télèphone en minutes', 
        main = "Répartition du temps de télèphone en minutes de notre échantillon", 
        col = 'orange', cex.main=0.85)
# STRESS Visuellement 2 valeurs atypiques => rosner
boxplot(S$STRESS,  xlab = 'Niveau de stress', 
        main = "Répartition du niveau de stress de notre échantillon", 
        col = 'orange', cex.main=0.85)


#TEST DES VALEURS ATYPIQUES#

#TEST DE ROSNER#
rosnerTest(S$AGE, k=10, alpha = 0.05) #85 ; 87 ; 91#
rosnerTest(S$STRESS, k=10, alpha = 0.05) #AUCUNE#
rosnerTest(S$EXIG, k=10, alpha = 0.05) #41 ; 96#
rosnerTest(S$PRESSION, k=10, alpha = 0.05) #AUCUNE#
rosnerTest(S$TEL.M, k=10, alpha = 0.05) #AUCUNE#
rosnerTest(S$TRAJET, k=10, alpha = 0.05) #1 VAL ATYPIQUE#

# TEST DE GRUBBS#
rosnerTest(S$SOM.M, k=10, alpha = 0.05) #AUCUNE#
grubbs.test(S$SOM.M,type=10, two.sided = TRUE) # P-value > 0.05 la valeur n'est pas atypique au seuil de risque de 5%

#TEST DE ESD#
y = S$TRAJET
rval = function(y){
  ares = abs(y - mean(y))/sd(y)
  df = data.frame(y, ares)
  r = max(df$ares)
  list(r, df)}
n = length(y)
alpha = 0.05
lam = c(1:12)
R = c(1:12)
for (i in 1:12){
  if(i==1){
    rt = rval(y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  else if(i!=1){
    rt = rval(newdf$y)
    R[i] = unlist(rt[1])
    df = data.frame(rt[2])
    newdf = df[df$ares!=max(df$ares),]}
  p = 1 - alpha/(2*(n-i+1))
  t = qt(p,(n-i-1))
  lam[i] = t*(n-i) / sqrt((n-i-1+t**2)*(n-i+1))
}
newdf = data.frame(c(1:12),R,lam)
names(newdf)=c("No. Outliers","Test Stat.", "Critical Val.")
newdf
sort(S$TRAJET) # 85 #

#SUPPRESSION DES VALEURS ATYPIQUES#
ST = S[-c(41,85,87,91,96),]

#ANALYSE DESCRIPTIVES DE LA NOUVELLE BASE#
summary(ST[,col_num])
round(sapply(ST[,col_num],sd),2)

#NOUVEAU BOXPLOT APRES SUPPRESION DES VALEURS ATYPIQUES#
boxplot(ST$AGE, xlab = 'Âge', main = "Répartition de l'âge de notre échantillon", col = 'orange', cex.main=0.85)
boxplot(ST$STRESS,  xlab = 'Niveau de stress', main = "Répartition du niveau de stress de notre échantillon", col = 'orange', cex.main=0.85)
boxplot(ST$EXIG,  xlab = 'Niveau d exigence de soi', main = "Répartition du niveau d exigence de soi de notre échantillon", col = 'orange', cex.main=0.85)
boxplot(ST$PRESSION ,  xlab = 'Niveau de pression académique perçu', main = "Répartition du niveau de pression académique perçu de notre échantillon", col = 'orange', cex.main=0.85)
boxplot(ST$TRAJET,  xlab = 'Temps de trajet en minutes', main = "Répartition du temps de trajet en minute de notre échantillon", col = 'orange', cex.main=0.85)
boxplot(ST$SOM.M ,  xlab = 'Temps de sommeil en minutes', main = "Répartition du temps de sommeil en minute de notre échantillon", col = 'orange', cex.main=0.85)
boxplot(ST$TEL.M,  xlab = 'Temps de télèphone en minutes', main = "Répartition du temps de télèphone en minutes de notre échantillon", col = 'orange', cex.main=0.85)


#ANALYSE BIVARIES#
plot(S[,col_num])

#QUANTI-QUANTI#
#NUAGE DE POINT ET DROITE DE REGRESSION#
#POUR VISUALISATION DE A DISTRIBUTION#
col_num2 <- c("AGE", "SPORT", "CONF", "EXIG", "HCOURS", "PRESSION", "TRAJET", "SOM.M", "TEL.M")
ST[, col_num2] <- lapply(ST[, col_num2], function(x) as.numeric(gsub(",", ".", x)))
for (var in col_num2) {
  plot_name <- paste("Régression linéaire de STRESS en fonction de", var)
  plot(ST[[var]], ST$STRESS, main = plot_name, xlab = var, ylab = "STRESS")
  abline(lm(ST$STRESS ~ ST[[var]], data = ST), col = "red")
}

#CORRELATION#
#MATRICE DE CORRELATION ENTRE Y ET VARIABLES QUANTITATIVES#
#UTILISATION DE SHAPIRO POUR VERIFIER LA NORMALITE DE NOS VARIABLES#
shapiro_resultats <- lapply(ST[,col_num], shapiro.test)
shapiro_resultats
#SI NOS DONNES SUIVENT UNE LOI NORMALE -> "p" SINON "s"#
Correlation = cor(ST[,col_num],use = "complete.obs",method="s" )
Correlation
corrplot(Correlation, type = "upper", order = 'AOE', col = colorRampPalette(c("#3333FF", "#FFCCFF", "#FF0000"))(100), tl.col = "#333333")
#AUTRES METHODES / GRAPHIQUES#
mydata = ST[,col_num]
chart.Correlation(mydata, histogram=TRUE, pch=19,method = c("spearman"))

#MATRICE DE CORRELATION SANS Y#
col_num_s_stress =  c("AGE", "SPORT","CONF", "EXIG", "HCOURS", "PRESSION", "TRAJET", "SOM.M","TEL.M")
#UTILISATION DE SHAPIRO POUR VERIFIER LA NORMALITE DE NOS VARIABLES#
shapiro_resultats <- lapply(ST[,col_num_s_stress], shapiro.test)
shapiro_resultats
#SI NOS DONNES SUIVENT UNE LOI NORMALE -> "p" SINON "s"#
Correlation = cor(ST[,col_num_s_stress],use = "complete.obs",method="s" )
Correlation
corrplot(Correlation, type = "upper", order = 'AOE', col = colorRampPalette(c("#3333FF", "#FFCCFF", "#FF0000"))(100), tl.col = "#333333")
#AUTRES METHODES / GRAPHIQUES#
mydata = ST[,col_num_s_stress]
chart.Correlation(mydata, histogram=TRUE, pch=19,method = c("spearman"))


#QUANTI-QUALI#
#BOITE A MOUSTACHE#
#POUR LA VISUALISATION DE LA DISTRIBUTION#
col_fac2
ST[, col_fac2] <- lapply(ST[, col_fac2], as.factor)
variable_Y <- "STRESS"

#VISUALISATION DES BOXPLOTS#
boxplot_function <- function(data, col_fac2, variable_Y) {
  data[, col_fac2] <- lapply(data[, col_fac2], as.factor)
  modalite_colors <- viridisLite::viridis(length(unique(data[[col_fac2[3]]])))
  plots_list <- list()
  for (i in 1:length(col_fac2)) {
    var <- col_fac2[i]
    p <- ggplot(data, aes(x = as.factor(!!sym(var)), y = data[[variable_Y]], color = as.factor(!!sym(var)))) +
      geom_boxplot() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      labs(title = paste("Régression linéaire de", variable_Y, "en fonction de", var),
           x = var, y = variable_Y) +
      scale_color_manual(values = modalite_colors) +
      theme_minimal() +
      theme(legend.position = "top") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des étiquettes sur l'axe x
    
    plots_list[[i]] <- p
  }
  print(plots_list)
}
boxplot_function(ST, col_fac2, "STRESS")

#CORRELATION#
#CORRELATION DES VARIABLES QUALI#
ST_quali = as.data.frame(ST[,col_fac2])
sjp.chi2(ST_quali,show.legend = TRUE)
#TEST DE VIF A FAIRE SUR LES MODELES AFIN DE VERIFIER SI LA DEPENDENCE PEUT NUIRE A NOS MODELES#

#CRÉATION DE VARIABLES#
#NOUVELLE BASE DE DONNES#
STR <- ST

#BINARISATION#
STR$GENRE <- if_else(ST$GENRE == "Homme","1","0") #INSTRUMENT#
STR$TRACAS <- if_else(ST$TRACAS == "Oui", "1","0")
STR$ECHEC <- if_else(ST$ECHEC == "Oui", "1","0")
STR$HABITAT <- if_else(ST$HABITAT == "Oui", "1","0")
STR$JOB <- if_else(ST$JOB == "Oui", "1","0")
STR$STATUT.AISEE <- if_else(ST$STATUT == "Classe aisée", "1","0")
STR$STATUT.MOYENNE <- if_else(ST$STATUT == "Classe moyenne", "1","0")
STR$PERSO.SOUCIEUX <- if_else(ST$PERSO == "SOUCIEUX", "1","0")
STR$PERSO.OUVERTURE <- if_else(ST$PERSO == "OUVERTURE", "1","0")
STR$PERSO.AGREABILITE <- if_else(ST$PERSO == "AGREABILITE", "1","0")
STR$PERSO.CONSCIENCE <- if_else(ST$PERSO == "CONSCIENCE", "1","0")
STR$CULTU.PARFOIS <- if_else(ST$CULTU == "PARFOIS", "1","0")
STR$CULTU.RAREMENT <- if_else(ST$CULTU == "RAREMENT", "1","0")
STR$CULTU.REGULIEREMENT <- if_else(ST$CULTU == "REGULIEREMENT", "1","0")
STR$ALIM.SAINES <- if_else(ST$ALIM == "SAINES", "1","0")
STR$ALIM.MAUVAISE <- if_else(ST$ALIM == "MAUVAISE", "1","0")
STR$ENTOURAGE.OUI <- if_else(ST$ENTOURAGE == "OUI", "1","0")
STR$ENTOURAGE.BOF <- if_else(ST$ENTOURAGE == "BOF", "1","0")
STR$VISITE.SOUVENT <- if_else(ST$VISITE == "SOUVENT", "1","0")
STR$VISITE.PARFOIS <- if_else(ST$VISITE == "PARFOIS", "1","0")
STR$VISITE.RAREMENT <- if_else(ST$VISITE == "RAREMENT", "1","0")
STR$ETUDE.ECO <- if_else(ST$ETUDE == "S_ECO", "1","0")
STR$ETUDE.HUM <- if_else(ST$ETUDE == "S_HUM", "1","0")
STR$ETUDE.SANTE <- if_else(ST$ETUDE == "SANTE", "1","0")
STR$AIDE.L.SUFFI <- if_else(ST$AIDE == "Largement suffisant", "1","0")
STR$AIDE.SUFFI <- if_else(ST$AIDE == "Suffisant", "1","0")
STR$AIDE.JUSTE <- if_else(ST$AIDE == "Juste", "1","0")
STR$BAC1 <- if_else(ST$BAC_regroupe == "Bac+1", "1","0")
STR$BAC2 <- if_else(ST$BAC_regroupe == "Bac+2", "1","0")
STR$BAC3 <- if_else(ST$BAC_regroupe == "Bac+3", "1","0")
STR$BAC4 <- if_else(ST$BAC_regroupe == "Bac+4", "1","0")
STR$RESEAU.INSTA <- if_else(ST$RESEAU_REGROUPE == "Instagram", "1","0")
STR$RESEAU.TIKTOK <- if_else(ST$RESEAU_REGROUPE == "Tiktok", "1","0")
STR$RESEAU.X <- if_else(ST$RESEAU_REGROUPE == "X (twitter)", "1","0")
STR$RESEAU.SNAP <- if_else(ST$RESEAU_REGROUPE == "Snapchat", "1","0")
STR$COMPA.TOUJOURS <- if_else(ST$COMPA_REGROUPE == "TOUJOURS", "1","0")
STR$COMPA.SOUVENT <- if_else(ST$COMPA_REGROUPE == "SOUVENT", "1","0")
STR$COMPA.PARFOIS <- if_else(ST$COMPA_REGROUPE == "PARFOIS", "1","0")

#SUPPRESSION DES ANCIENNES COLONNES# 
STR <- STR[, -c(3, 4, 8, 9, 13, 15, 16, 21, 24, 25, 26)]

#ESTIMATION ECONOMETRIQUE 1#
#FONCTION STEP#
library(leaps)
reg0 <- (lm(STRESS~1,data = STR))
reg0

reg <- lm(STRESS ~ TRACAS+ECHEC+HABITAT+JOB+PERSO.SOUCIEUX+PERSO.OUVERTURE+PERSO.AGREABILITE+PERSO.CONSCIENCE+
            CULTU.PARFOIS+CULTU.RAREMENT+CULTU.REGULIEREMENT+ENTOURAGE.OUI+ENTOURAGE.BOF+VISITE.SOUVENT+VISITE.PARFOIS+
            VISITE.RAREMENT+ETUDE.ECO+ETUDE.HUM+ETUDE.SANTE+AIDE.L.SUFFI+AIDE.SUFFI+AIDE.JUSTE+BAC1+BAC2+BAC3+BAC4+
            COMPA.TOUJOURS+COMPA.SOUVENT+COMPA.PARFOIS + SPORT + CONF+ EXIG + HCOURS+ PRESSION+
            TRAJET+ SOM.M+ TEL.M, data = STR)
summary(reg)
summary(STR)
#LM : MCO#

#METHODES ASCENDANTE#
step(reg0, scope=list(lower=reg0, upper=reg),data=STR, direction="forward")

#MEILLEUR REGRESSION#
#lm(formula = STRESS ~ CONF + ECHEC + EXIG + ETUDE.ECO + PERSO.SOUCIEUX + 
#     SOM.M + TRACAS + SPORT + HABITAT + TRAJET + PERSO.OUVERTURE + 
#     BAC1, data = STR)

#METHODES DESCENDANTE#
step(reg, data=base3,direction="backward")

#MEILLEUR REGRESSION#
#lm(formula = STRESS ~ TRACAS + ECHEC + HABITAT + PERSO.SOUCIEUX + 
#PERSO.OUVERTURE + VISITE.RAREMENT + ETUDE.ECO + AIDE.L.SUFFI + 
#  BAC1 + COMPA.TOUJOURS + COMPA.SOUVENT + SPORT + CONF + EXIG + 
#  TRAJET + SOM.M + TEL.M, data = STR)

#METHODES DANS LES 2 SENS#
step(reg0, scope = list(upper=reg),data=base3,direction="both")

#MEILLEUR REGRESSION#
#lm(formula = STRESS ~ CONF + ECHEC + EXIG + ETUDE.ECO + PERSO.SOUCIEUX + 
#SOM.M + TRACAS + SPORT + HABITAT + TRAJET + PERSO.OUVERTURE + 
#  BAC1, data = STR)

#MODELE INITIALE / MCO#
modele = lm(formula = STRESS ~ CONF + ECHEC + EXIG + ETUDE.ECO + PERSO.SOUCIEUX + 
              SOM.M + TRACAS + SPORT + HABITAT + TRAJET + PERSO.OUVERTURE + 
              BAC1, data = STR)

#TEST#
#NORMALITE#
par(mfrow=c(1,1))
residus = residuals(modele)
plot(modele)
hist(residus, col="darkblue")
gvlma(modele)

#KS TEST#
residus = residuals(modele)
ks.test(residus, "pnorm", mean(residus), sd(residus)) #SUP A 30 CAS#

#SHAPIRO#
shapiro.test(residus) #INF A 30 PAS NOTRE CAS#

#REPRESENTATION GRAPHIQUE DES RESIDUS ET DE LEURS DISTRIBUTIONS#
qqnorm(residus, main="QQ Plot des Résidus", xlab="Quantiles théoriques de la distribution normale", ylab="Résidus observés")
qqline(residus, col="red", lwd=2)
grid()
legend("topleft", legend="Résidus", col="black", pch=1)

#FORME FONCTIONNELLE#
reset(modele) #FORME FONCTIONNELLE LINEAIRE ACCEPTE#

#MULTICOLINEARITE#
vif(modele) #FAIBLE COLINEARITE#

#TEST DE BP#
bptest(modele) #L'HYPOTHESE D'HOMOSCEDASTICITE DES RESIDUS EST ACCEPTEE#

#DISTANCE DE COOK#
plot(cooks.distance(modele),type="h", ylab="Distance de Cook")
#INFLUENCE QU'A L'OBSERVATION SUR LE COEF DE REGRESSION AUCUNE OBSERVATION A RETIRE CAR < 1#

#ESTIMATION ECONOMETRIQUE 2#
#MODELE LOG-LOG#
STR$lnCONF <- log(STR$CONF + 0.001)
STR$lnEXIG <- log(STR$EXIG + 0.001)
STR$lnSOM.M <- log(STR$SOM.M + 0.001)
STR$lnTRAJET <- log(STR$TRAJET + 0.001)
STR$lnSPORT <- log(STR$SPORT + 0.001)
STR$lnSTRESS <- log(STR$STRESS + 1)
modele2 = lm(STR$lnSTRESS ~ lnCONF + ECHEC + lnEXIG + ETUDE.ECO + PERSO.SOUCIEUX +
               lnSOM.M + TRACAS + lnSPORT + HABITAT + lnTRAJET + PERSO.OUVERTURE +
               BAC1, data = STR)
#TEST MODELE LOG-LOG#
summary(modele2)
reset(modele2)
residus2 = residuals(modele2)
ks.test(residus2, "pnorm", mean(residus2), sd(residus2))
bptest(modele2)
gvlma(modele2)
vif(modele2)

#ESTIMATION ECONOMETRIQUE 3#
#MODELE LOG-LINEAIRE#
modele3 <- lm(lnSTRESS ~ CONF + ECHEC + EXIG + ETUDE.ECO + PERSO.SOUCIEUX +
                SOM.M + TRACAS + SPORT + HABITAT + TRAJET + PERSO.OUVERTURE +
                BAC1, data = STR)
#TEST MODELE LOG-LINEAIRE#
summary(modele3)
reset(modele3)
residus3 = residuals(modele3)
ks.test(residus3, "pnorm", mean(residus3), sd(residus3))
bptest(modele3)
gvlma(modele3)
vif(modele3)

#INTERPRETATION DU MEILLEUR MODELE, L'ESTIMATION ECONOMETRIQUE 1#
summary(modele)
#F p_value < 0,05 NOUS GARDONS LE MODELE#
#R^2 CAR NOUS COMPARONS PAS LE MODELE#

#VERIFICATION DE L'ENDOGENEITE#
reg_iv <- ivreg(STRESS ~ CONF + ECHEC + EXIG + ETUDE.ECO + PERSO.SOUCIEUX + 
                  SOM.M + TRACAS + SPORT + HABITAT + TRAJET + PERSO.OUVERTURE + 
                  BAC1 | ECHEC + EXIG + ETUDE.ECO + PERSO.SOUCIEUX + TRACAS + 
                  SPORT + HABITAT + TRAJET + PERSO.OUVERTURE + BAC1 + GENRE + AGE + 
                  ALIM.MAUVAISE+ ALIM.SAINES + TEL.M + RESEAU.SNAP + RESEAU.X + RESEAU.TIKTOK + 
                  RESEAU.INSTA
                ,data=STR)
summary(reg_iv,vcov=sandwich,diagnostics = TRUE)
#IVREG : DMC#
#LES INSTRUMENTS POUR CONF N'ONT PAS D'IMPACT SIGNIFICATIF#
#LES INSTRUMENTS POUR SOM.M ONT  UN IMPACT SIGNIFICATIF#
#UTILISATION DES DMC#
#VALIDITE DES INSTRUMENTS JUSTE POUR SOMMEIL #
#SUPPRESSION VARIABLE CONF CAR AYANT FAIT UN QUESTIONNAIRE NOUS NE POUVONS PAS AVOIR DE 

#PREVISIONS#
par(mfrow=c(1,1))
STR$predict = round(predict(modele),2)
STR$predict
STR$STRESS
round(STR$predict, 2)
plot()
plot(STR$predict, STR$STRESS, xlab="Niveau de stress prédit", 
     ylab="Niveau de stress observé", 
     col=0,main = "Niveau de stree prédit par le modèle et Niveau de stress réel", 
     abline(a=0, b=1, col="red", lty=1), ylim = c(0, 50), xlim = c(0, 10))
text(STR$predict, STR$STRESS, row.names(STR), cex=.6)

par(mfrow=c(1,1))
STR$predict = round(predict(modele2),2)
STR$predict
STR$STRESS
round(STR$predict, 2)
plot()

#PREDICTION DU NIVEAU DE STRESS#
plot(STR$predict, STR$STRESS, xlab="Niveau de stress prédit", 
     ylab="Niveau de stress observé", 
     col=0,main = "Niveau de stree prédit par le modèle et Niveau de stress réel", 
     abline(a=0, b=1, col="red", lty=1), ylim = c(0, 50), xlim = c(0, 10))
text(STR$predict, STR$STRESS, row.names(STR), cex=.6)
Graphique 1 : Evolution de la production industrielle au Royaume-Uni de janvier 1950 à juillet 2024

#DISTRIBUTION OBSERVEE DE LA VARIABLE STRESS#
par(mfrow = c(1,2))
hist(STR$predict, col = "lightblue", xlab = "Niveau de stress prédit", 
     ylab = "Fréquence", ylim = c(0, 50), xlim = c(0, 10), 
     main = "Distribution prédite de la variable STRESS")
hist(STR$STRESS, main = "Distribution observée de la variable STRESS", 
     col="blue",ylim = c(0, 50), xlab = "Niveau de stress observé",
     ylab = "Fréquence")
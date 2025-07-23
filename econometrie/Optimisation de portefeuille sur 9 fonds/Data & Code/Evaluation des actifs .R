#library#
library(data.table)
library(ggplot2)
library(scales)
library(corrplot)
library(factoextra)
library(FactoMineR)
library(gridExtra)
library(AER)
library(ggrepel)
library(e1071)
library(doBy)
library(moments)
library(knitr)
library(dplyr)
library(xts)
library(PerformanceAnalytics)



#Importation#
Ais <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/AIS.F.csv", sep=","),ticker="Ais")
Amundi <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/Amundi.F.csv", sep=","),ticker="Amundi")
Ande <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/ANDE.csv", sep=","),ticker="Ande")
Carmignac <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/Carmignac.F.csv", sep=","),ticker="Carmignac")
Epsens <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/Epsens.F.csv", sep=","),ticker="Epsens")
Fidelity <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/Fidelity.L.csv", sep=","),ticker="Fidelity")
Mainfirst <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/mainFirst.F.csv", sep=","),ticker="Mainfirst")
Pluvalca <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/Pluvalca.F.csv", sep=","),ticker="Pluvalca")
Thematics <- data.table(read.csv2("/Users/rododo/Desktop/ECAP/Evaluation des actifs/DOSSIER/BASE DE DONNEE/Thematics.csv", sep=","),ticker="Thematics")

#Selection des colonnes importante#
Ais <- Ais[,-c(2:5,7)]
Amundi <- Amundi[,-c(2:5,7)]
Ande <- Ande[,-c(2:5,7)]
Carmignac <- Carmignac[,-c(2:5,7)]
Epsens <- Epsens[,-c(2:5,7)]
Fidelity <- Fidelity[,-c(2:5,7)]
Mainfirst <- Mainfirst[,-c(2:5,7)]
Pluvalca <- Pluvalca[,-c(2:5,7)]
Thematics <- Thematics[,-c(2:5,7)]

#Numérisation des variables#
convertToNumeric <- function(dataframe, columnName) {
  dataframe[[columnName]] <- as.numeric(dataframe[[columnName]])
  return(dataframe)
}

Ais <- convertToNumeric(Ais,"Adj.Close")
Amundi <- convertToNumeric(Amundi,"Adj.Close")
Ande <- convertToNumeric(Ande,"Adj.Close")
Carmignac <- convertToNumeric(Carmignac,"Adj.Close")
Epsens <- convertToNumeric(Epsens,"Adj.Close")
Fidelity <- convertToNumeric(Fidelity,"Adj.Close")
Mainfirst <- convertToNumeric(Mainfirst,"Adj.Close")
Pluvalca <- convertToNumeric(Pluvalca,"Adj.Close")
Thematics <- convertToNumeric(Thematics,"Adj.Close")
str(Thematics)

#Création de la bas de donnée#
base <- rbind(Ais,Amundi,Ande,Carmignac,Epsens,Fidelity,Mainfirst,Pluvalca,Thematics)
base[, Date := as.Date(Date, format = "%Y-%m-%d")]
str(base)
View(base)
basebis<-base[,c("Date","Adj.Close","ticker")]
colnames(basebis) <- c("Date", "prix","titre")

#Graphique de l'evolution du prix pour chaque fonds indiciel
#Ais#
basebis_Ais = basebis[basebis$titre=="Ais",]
mean(basebis_Ais$prix, na.rm = TRUE)
plot_prix_Ais = ggplot(basebis_Ais, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en €") +
  scale_color_discrete(name = "Actif")
plot_prix_Ais = plot_prix_Ais + labs(title = "Evolution du prix de l'actif AIS Mandarine Global Transition",
                                         subtitle = "du 01/01/23 au 31/12/23",
                                         caption = "Data source: Yahoo Finance")
plot_prix_Ais = plot_prix_Ais + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Ais

#Amundi#
basebis_Amundi = basebis[basebis$titre=="Amundi",]
mean(basebis_Amundi$prix, na.rm = TRUE)
plot_prix_Amundi = ggplot(basebis_Amundi, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en €") +
  scale_color_discrete(name = "Actif")
plot_prix_Amundi = plot_prix_Amundi + labs(title = "Evolution du prix de l'actif Amundi CPR Climate Action I",
                                     subtitle = "du 01/01/23 au 31/12/23",
                                     caption = "Data source: Yahoo Finance")
plot_prix_Amundi = plot_prix_Amundi + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Amundi

#Ande#
basebis_Ande = basebis[basebis$titre=="Ande",]
mean(basebis_Ande$prix, na.rm = TRUE)
plot_prix_Ande = ggplot(basebis_Ande, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en $") +
  scale_color_discrete(name = "Actif")
plot_prix_Ande = plot_prix_Ande + labs(title = "Evolution du prix de l'actif The Andersons : (ANDE)
",
                                           subtitle = "du 01/01/23 au 31/12/23",
                                           caption = "Data source: Yahoo Finance")
plot_prix_Ande = plot_prix_Ande + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Ande

#Carmignac#
basebis_Carmignac = basebis[basebis$titre=="Carmignac",]
mean(basebis_Carmignac$prix, na.rm = TRUE)
plot_prix_Carmignac = ggplot(basebis_Carmignac, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en €") +
  scale_color_discrete(name = "Actif")
plot_prix_Carmignac = plot_prix_Carmignac + labs(title = "Evolution du prix de l'actif Carmignac Emergents",
                                       subtitle = "du 01/01/23 au 31/12/23",
                                       caption = "Data source: Yahoo Finance")
plot_prix_Carmignac = plot_prix_Carmignac + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Carmignac

#Epsens#
basebis_Epsens = basebis[basebis$titre=="Epsens",]
mean(basebis_Epsens$prix, na.rm = TRUE)
plot_prix_Epsens = ggplot(basebis_Epsens, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en €") +
  scale_color_discrete(name = "Actif")
plot_prix_Epsens = plot_prix_Epsens + labs(title = "Evolution du prix de l'actif Epsens EdR Tricolore Rendement",
                                                 subtitle = "du 01/01/23 au 31/12/23",
                                                 caption = "Data source: Yahoo Finance")
plot_prix_Epsens = plot_prix_Epsens + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Epsens

#Fidelity#
basebis_Fidelity = basebis[basebis$titre=="Fidelity",]
mean(basebis_Fidelity$prix, na.rm = TRUE)
plot_prix_Fidelity = ggplot(basebis_Fidelity, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en £") +
  scale_color_discrete(name = "Actif")
plot_prix_Fidelity = plot_prix_Fidelity + labs(title = "Evolution du prix de l'actif Fidelity Sustainable Water & Waste R Acc",
                                           subtitle = "du 01/01/23 au 31/12/23",
                                           caption = "Data source: Yahoo Finance")
plot_prix_Fidelity = plot_prix_Fidelity + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Fidelity

#Mainfirst#
basebis_Mainfirst = basebis[basebis$titre=="Mainfirst",]
mean(basebis_Mainfirst$prix, na.rm = TRUE)
plot_prix_Mainfirst = ggplot(basebis_Mainfirst, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en €") +
  scale_color_discrete(name = "Actif")
plot_prix_Mainfirst = plot_prix_Mainfirst + labs(title = "Evolution du prix de l'actif MainFirst Global Equities X",
                                               subtitle = "du 01/01/23 au 31/12/23",
                                               caption = "Data source: Yahoo Finance")
plot_prix_Mainfirst = plot_prix_Mainfirst + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Mainfirst

#Pluvalca#
basebis_Pluvalca = basebis[basebis$titre=="Pluvalca",]
mean(basebis_Pluvalca$prix, na.rm = TRUE)
plot_prix_Pluvalca = ggplot(basebis_Pluvalca, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en €") +
  scale_color_discrete(name = "Actif")
plot_prix_Pluvalca = plot_prix_Pluvalca + labs(title = "Evolution du prix de l'actif Pluvalca Sustainable Opportunities",
                                                 subtitle = "du 01/01/23 au 31/12/23",
                                                 caption = "Data source: Yahoo Finance")
plot_prix_Pluvalca = plot_prix_Pluvalca + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Pluvalca

#Thematics#
basebis_Thematics = basebis[basebis$titre=="Thematics",]
mean(basebis_Thematics$prix, na.rm = TRUE)
plot_prix_Thematics = ggplot(basebis_Thematics, aes(x = Date, y = prix, color = titre)) +
  geom_line() +
  theme_classic() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("Prix en $") +
  scale_color_discrete(name = "Actif")
plot_prix_Thematics = plot_prix_Thematics + labs(title = "Evolution du prix de l'actif Thematics Water R/A USD",
                                               subtitle = "du 01/01/23 au 31/12/23",
                                               caption = "Data source: Yahoo Finance")
plot_prix_Thematics = plot_prix_Thematics + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)
plot_prix_Thematics

#Exercice 1#
##Question 1
#indexation prix#
basebis[, idx_price := prix/prix[1] , by = titre]

# Evolution des prix en fonction du temps
ggplot(basebis, aes(x = Date, y = idx_price, color = titre)) +
  geom_line() +
  theme_bw() + ggtitle("Evolution des prix") +
  xlab("Date") + ylab("(01/01/2023 = 1)") +
  scale_color_discrete(name = "Actif")

# Calcul des rendements
basebis[, rdmt := prix / shift(prix, 1) - 1, by = titre]

# Evolution des rendements en fonction du temps
plot_rdmt = ggplot(basebis, aes(x = Date, y = rdmt, color = titre)) +
  geom_line() +
  theme_light() + ggtitle("Evolution des rendements des différents actifs") +
  xlab("Date") + ylab("(01/01/2023 = 0)") +
  scale_color_discrete(name = "Actif")

plot_rdmt = plot_rdmt + labs(title = "Evolution des rendements des différents actifs",
                             subtitle = "du 01/01/23 au 31/12/23",
                             caption = "Data source: Yahoo Finance")

plot_rdmt = plot_rdmt + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.subtitle = element_text(color = "#339900", size = 9),
  plot.caption = element_text(color = "#330066", face ="plain")
)

plot_rdmt

##Question 2
#Dossier
##Question 3
#Dossier

###Question 4
par(mfrow = c(1,3))

Ais[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]
Amundi[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]
Ande[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]
Carmignac[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]
Epsens[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]
Fidelity[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]
Mainfirst[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]
Pluvalca[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]
Thematics[, rdmt := Adj.Close / shift(Adj.Close, 1) - 1]

boxplot(Ais$rdmt,ylab = "AIS Mandarine Global Transition",col = "#000099")
boxplot(Amundi$rdmt,ylab = "Amundi CPR Climate Action I",col = "#000099")
boxplot(Ande$rdmt,ylab = "The Andersons : (ANDE)",col = "#000099")
boxplot(Carmignac$rdmt,ylab = "Carmignac Emergents",col = "#000099")
boxplot(Epsens$rdmt,ylab = "Epsens EdR Tricolore Rendement",col = "#000099")
boxplot(Fidelity$rdmt,ylab = "Fidelity Sustainable Water & Waste R Acc",col = "#000099")
boxplot(Mainfirst$rdmt,ylab = "MainFirst Global Equities X",col = "#000099")
boxplot(Pluvalca$rdmt,ylab = "Pluvalca Sustainable Opportunities",col = "#000099")
boxplot(Thematics$rdmt,ylab = "Thematics Water R/A USD",col = "#000099")

##Question 5
#Statistique descriptives#
calculate_stats <- function(x) {
  stats <- c(Mean = mean(x, na.rm = TRUE),
             Variance = var(x, na.rm = TRUE),
             SD = sd(x, na.rm = TRUE),
             Skewness = skewness(x, na.rm = TRUE),
             Kurtosis = kurtosis(x, na.rm = TRUE))

  shapiro_test_result <- shapiro.test(x)
  c(stats, Shapiro_P_Value = shapiro_test_result$p.value)
}
result <- summaryBy(rdmt ~ titre, data = basebis,
                    FUN = calculate_stats)
setnames(result, c("titre", "Moyenne", "Variance", "Ecart_type", "Asymétrie", "Kurtosis", "Shapiro_P_Value"))
kable(result)

##Question 6
R<-tapply(basebis$rdmt,basebis$titre,mean,na.rm = TRUE)
O<-tapply(basebis$rdmt,basebis$titre,sd,na.rm = TRUE)
r=0.005

# Ratio de Sharpe#
S=(R-r)/O
S <- as.data.frame(S)
setnames(S,c("Ratio de sharpe"))
kable(S)

##Question 7
Ais<-mutate(Ais, idx_prix := Adj.Close/Adj.Close[1])
Amundi<-mutate(Amundi, idx_prix := Adj.Close/Adj.Close[1])
Ande<-mutate(Ande, idx_prix := Adj.Close/Adj.Close[1])
Carmignac<-mutate(Carmignac, idx_prix := Adj.Close/Adj.Close[1])
Epsens<-mutate(Epsens, idx_prix := Adj.Close/Adj.Close[1])
Fidelity<-mutate(Fidelity, idx_prix := Adj.Close/Adj.Close[1])
Mainfirst<-mutate(Mainfirst, idx_prix := Adj.Close/Adj.Close[1])
Pluvalca<-mutate(Pluvalca, idx_prix := Adj.Close/Adj.Close[1])
Thematics<-mutate(Thematics, idx_prix := Adj.Close/Adj.Close[1])

#Nouvelle base de données#
basebis1 = merge(Ais, Amundi, by = "Date")
names(basebis1)[2]="Ajd.Close_Ais"
names(basebis1)[3]="titre_Ais"
names(basebis1)[4]="rdmt_Ais"
names(basebis1)[5]="idx_prix_Ais"
names(basebis1)[6]="Ajd.Close_Amundi"
names(basebis1)[7]="titre_Amundi"
names(basebis1)[8]="rdmt_Amundi"
names(basebis1)[9]="idx_prix_Amundi"

View(basebis1)

basebis2 = merge(basebis1, Ande, by = "Date")
names(basebis2)[10]="Ajd.Close_Ande"
names(basebis2)[11]="titre_Ande"
names(basebis2)[12]="rdmt_Ande"
names(basebis2)[13]="idx_prix_Ande"

basebis3 = merge(basebis2, Carmignac, by = "Date")
names(basebis3)[14]="Ajd.Close_Carmignac"
names(basebis3)[15]="titre_Carmignac"
names(basebis3)[16]="rdmt_Carmignac"
names(basebis3)[17]="idx_prix_Carmignac"

basebis4 = merge(basebis3, Epsens, by = "Date")
names(basebis4)[18]="Ajd.Close_Epsens"
names(basebis4)[19]="titre_Epsens"
names(basebis4)[20]="rdmt_Epsens"
names(basebis4)[21]="idx_prix_Epsens"

basebis5 = merge(basebis4, Fidelity, by = "Date")
names(basebis5)[22]="Ajd.Close_Fidelity"
names(basebis5)[23]="titre_Fidelity"
names(basebis5)[24]="rdmt_Fidelity"
names(basebis5)[25]="idx_prix_Fidelity"

basebis6 = merge(basebis5, Mainfirst, by = "Date")
names(basebis6)[26]="Ajd.Close_Mainfirst"
names(basebis6)[27]="titre_Mainfirst"
names(basebis6)[28]="rdmt_Mainfirst"
names(basebis6)[29]="idx_prix_Mainfirst"

basebis7 = merge(basebis6, Pluvalca, by = "Date")
names(basebis7)[30]="Ajd.Close_Pluvalca"
names(basebis7)[31]="titre_Pluvalca"
names(basebis7)[32]="rdmt_Pluvalca"
names(basebis7)[33]="idx_prix_Pluvalca"

basebis_fin = merge(basebis7, Thematics, by = "Date")
names(basebis_fin)[34]="Ajd.Close_Thematics"
names(basebis_fin)[35]="titre_Thematics"
names(basebis_fin)[36]="rdmt_Thematics"
names(basebis_fin)[37]="idx_prix_Thematics"

str(basebis_fin)
basebisrendement <- xts(basebis_fin[,c(4,8,12,16,20,24,28,32,36)], order.by=as.Date(basebis_fin$Date))
names(basebisrendement) <- c("Ais","Amundi","Ande","Carmignac","Epsens","Fidelity","Mainfirst",
                             "Pluvalca","Thematics")
basebisrendement <- basebisrendement[-1,]
View(basebis_fin)
View(basebisrendement)

#variance covariance#
cov_matrix <- cov(na.omit(basebisrendement), method = "pearson")
kable(cov_matrix)

##Question 8
par(mfrow = c(1,1))
corr<-cor(na.omit(basebisrendement))
corrplot(corr, method = "number", type = "upper",tl.col = "black",
         col = colorRampPalette(c("#00FFFF", "#006666", "#003333"))(100))

##Question 9
#Dossier

##Question 10
#Suppression de NA
Ais <- na.omit(Ais)
Amundi <- na.omit(Amundi)
Ande <- na.omit(Ande)
Carmignac <- na.omit(Carmignac)
Epsens <- na.omit(Epsens)
Fidelity <- na.omit(Fidelity)
Mainfirst <- na.omit(Mainfirst)
Pluvalca <- na.omit(Pluvalca)
Thematics <- na.omit(Thematics)

Aisrdmt <- xts(Ais[,4], order.by=as.Date(Ais$Date))
Amundirdmt <- xts(Amundi[,4], order.by=as.Date(Amundi$Date))
Anderdmt <- xts(Ande[,4], order.by=as.Date(Ande$Date))
Carmignacrdmt <- xts(Carmignac[,4], order.by=as.Date(Carmignac$Date))
Epsensrdmt <- xts(Epsens[,4], order.by=as.Date(Epsens$Date))
Fidelityrdmt <- xts(Fidelity[,4], order.by=as.Date(Fidelity$Date))
Mainfirstrdmt <- xts(Mainfirst[,4], order.by=as.Date(Mainfirst$Date))
Pluvalcardmt <- xts(Pluvalca[,4], order.by=as.Date(Pluvalca$Date))
Thematicsrdmt <- xts(Thematics[,4], order.by=as.Date(Thematics$Date))


#Ratio de Jensen
A_Jensen1<-round(SFM.jensenAlpha(Aisrdmt,Amundirdmt),5)
A_Jensen2<-round(SFM.jensenAlpha(Amundirdmt,Amundirdmt),5)
A_Jensen3<-round(SFM.jensenAlpha(Anderdmt,Amundirdmt),5)
A_Jensen4<-round(SFM.jensenAlpha(Carmignacrdmt,Amundirdmt),5)
A_Jensen5<-round(SFM.jensenAlpha(Epsensrdmt,Amundirdmt),5)
A_Jensen6<-round(SFM.jensenAlpha(Fidelityrdmt,Amundirdmt),5)
A_Jensen7<-round(SFM.jensenAlpha(Mainfirstrdmt,Amundirdmt),5)
A_Jensen8<-round(SFM.jensenAlpha(Pluvalcardmt,Amundirdmt),5)
A_Jensen9<-round(SFM.jensenAlpha(Thematicsrdmt,Amundirdmt),5)

Jensen<-c(A_Jensen1,A_Jensen2,A_Jensen3,A_Jensen4,A_Jensen5,A_Jensen6,A_Jensen7,A_Jensen8,A_Jensen9)
Names<-c("Ais","Amundi","Ande","Carmignac","Epsens","Fidelity","Mainfirst",
         "Pluvalca","Thematics")
Jensen<-rbind(Names,Jensen)
Jensen <- t(Jensen)
kable(Jensen)

# Ratio de Treynor
Treynor1<-round(TreynorRatio(Aisrdmt,Amundirdmt),5)
Treynor2<-round(TreynorRatio(Amundirdmt,Amundirdmt),5)
Treynor3<-round(TreynorRatio(Anderdmt,Amundirdmt),5)
Treynor4<-round(TreynorRatio(Carmignacrdmt,Amundirdmt),5)
Treynor5<-round(TreynorRatio(Epsensrdmt,Amundirdmt),5)
Treynor6<-round(TreynorRatio(Fidelityrdmt,Amundirdmt),5)
Treynor7<-round(TreynorRatio(Mainfirstrdmt,Amundirdmt),5)
Treynor8<-round(TreynorRatio(Pluvalcardmt,Amundirdmt),5)
Treynor9<-round(TreynorRatio(Thematicsrdmt,Amundirdmt),5)

Treynor<-c(Treynor1,Treynor2,Treynor3,Treynor4,Treynor5,Treynor6,
           Treynor7,Treynor8,Treynor9)
Treynor<-rbind(Names,Treynor)
Treynor <- t(Treynor)
kable(Treynor)

# Ratio de Sortino
Sortino1<-round(SortinoRatio(Aisrdmt,MAR = 0),5)
Sortino2<-round(SortinoRatio(Amundirdmt,MAR = 0),5)
Sortino3<-round(SortinoRatio(Anderdmt,MAR = 0),5)
Sortino4<-round(SortinoRatio(Carmignacrdmt,MAR = 0),5)
Sortino5<-round(SortinoRatio(Epsensrdmt,MAR = 0),5)
Sortino6<-round(SortinoRatio(Fidelityrdmt,MAR = 0),5)
Sortino7<-round(SortinoRatio(Mainfirstrdmt,MAR = 0),5)
Sortino8<-round(SortinoRatio(Pluvalcardmt,MAR = 0),5)
Sortino9<-round(SortinoRatio(Thematicsrdmt,MAR = 0),5)

Sortino<-c(Sortino1,Sortino2,Sortino3,Sortino4,Sortino5,Sortino6,
           Sortino7,Sortino8,Sortino9)
Sortino<-rbind(Names,Sortino)
Sortino<-t(Sortino)
kable(Sortino)

# Omegaratio
O1 <- OmegaSharpeRatio(Aisrdmt,)
O2 <- OmegaSharpeRatio(Amundirdmt)
O3 <- OmegaSharpeRatio(Anderdmt)
O4 <- OmegaSharpeRatio(Carmignacrdmt)
O5 <- OmegaSharpeRatio(Epsensrdmt)
O6 <- OmegaSharpeRatio(Fidelityrdmt)
O7 <- OmegaSharpeRatio(Mainfirstrdmt)
O8 <- OmegaSharpeRatio(Pluvalcardmt)
O9 <- OmegaSharpeRatio(Thematicsrdmt)
O3

Omega_ratio <- c(O1,O2,O3,O4,O5,O6,O7,O8,O9)
Omega_ratio<-rbind(Names,Omega_ratio)
Omega_ratio<-t(Omega_ratio)
kable(Omega_ratio)

# Ratio de l'information

RatioInfo <- R/O
RatioInfo <- as.data.frame(RatioInfo)
setnames(RatioInfo,c("Ratio de l'information"))
kable(RatioInfo)

# Ratio de Sterling
S1 <- (SterlingRatio(Aisrdmt,excess = 0.005))
S2 <- (SterlingRatio(Amundirdmt,excess = 0.005))
S3 <- (SterlingRatio(Anderdmt,excess = 0.005))
S4 <- (SterlingRatio(Carmignacrdmt,excess = 0.005))
S5 <- (SterlingRatio(Epsensrdmt,excess = 0.005))
S6 <- (SterlingRatio(Fidelityrdmt,excess = 0.005))
S7 <- (SterlingRatio(Mainfirstrdmt,excess = 0.005))
S8 <- (SterlingRatio(Pluvalcardmt,excess = 0.005))
S9 <- (SterlingRatio(Thematicsrdmt,excess = 0.005))


Sterling<-c(S1,S2,S3,S4,S5,S6,S7,S8,S9)
Sterling<-rbind(Names,Sterling)
Sterling<-t(Sterling)
kable(Sterling)

# Ratio de sharpe ajutée avec biais de série temporelle 
Sh1 <- SharpeRatio(Aisrdmt$rdmt, method = "modified",rf = 0.005)
Sh2 <- SharpeRatio(Amundirdmt$rdmt, method = "modified",rf = 0.005)
Sh3 <- SharpeRatio(Anderdmt$rdmt, method = "modified",rf = 0.005)
Sh4 <- SharpeRatio(Carmignacrdmt$rdmt, method = "modified",rf = 0.005)
Sh5 <- SharpeRatio(Epsensrdmt$rdmt, method = "modified",rf = 0.005)
Sh6 <- SharpeRatio(Fidelityrdmt$rdmt, method = "modified",rf = 0.005)
Sh7 <- SharpeRatio(Mainfirstrdmt$rdmt, method = "modified",rf = 0.005)
Sh8 <- SharpeRatio(Pluvalcardmt$rdmt, method = "modified",rf = 0.005)
Sh9 <- SharpeRatio(Thematicsrdmt$rdmt, method = "modified",rf = 0.005)

sharpe_df <- data.frame(
  Names = c("Ais", "Amundi", "Ander", "Carmignac", "Epsens", "Fidelity", "MainFirst", "PLuvalca", "Thematics"),
  ratio_sharpe_ajusté = c(Sh1, Sh2, Sh3, Sh4, Sh5, Sh6, Sh7, Sh8, Sh9)
)
sharpe_mat <- t(matrix(sharpe_df$ratio_sharpe_ajusté, nrow = length(Sh1)))
noms_colonnes <- c("StdDev Sharpe", "VaR Sharpe", "ES Sharpe")
colnames(sharpe_mat) <- noms_colonnes
rownames(sharpe_mat) <- Names
kable(sharpe_mat)

# le UPR ratio
UPR11 <- UpsidePotentialRatio(Aisrdmt)
UPR12 <- UpsidePotentialRatio(Amundirdmt)
UPR13 <- UpsidePotentialRatio(Anderdmt)
UPR14 <- UpsidePotentialRatio(Carmignacrdmt)
UPR15 <- UpsidePotentialRatio(Epsensrdmt)
UPR16 <- UpsidePotentialRatio(Fidelityrdmt)
UPR17 <- UpsidePotentialRatio(Mainfirstrdmt)
UPR18 <- UpsidePotentialRatio(Pluvalcardmt)
UPR19 <- UpsidePotentialRatio(Thematicsrdmt)

UPR<-c(UPR11,UPR12,UPR13,UPR14,UPR15,UPR16,UPR17,UPR18,UPR19)
UPR<-rbind(Names,UPR)
UPR<-t(UPR)
kable(UPR)

# le DPR ratio
DPR1 <- (DownsidePotential(Aisrdmt))
DPR2 <- (DownsidePotential(Amundirdmt))
DPR3 <- (DownsidePotential(Anderdmt))
DPR4 <- (DownsidePotential(Carmignacrdmt))
DPR5 <- (DownsidePotential(Epsensrdmt))
DPR6 <- (DownsidePotential(Fidelityrdmt))
DPR7 <- (DownsidePotential(Mainfirstrdmt))
DPR8 <- (DownsidePotential(Pluvalcardmt))
DPR9 <- (DownsidePotential(Thematicsrdmt))

DPR<-c(DPR1,DPR2,DPR3,DPR4,DPR5,DPR6,DPR7,DPR8,DPR9)
DPR<-rbind(Names,DPR)
DPR<-t(DPR)
kable(DPR)

#Exercice 1#
##Question 1
#portefeuille équipondéré#
basebisrendement <- na.omit(basebisrendement)
Rdmts_equi<-Return.portfolio(basebisrendement)
Portefolio_equi <- data.table(Rdmts_equi,ticker="Portefeuille")
names(Portefolio_equi) <- c("rendement","titre")
mean(Portefolio_equi$rendement)
sd(Portefolio_equi$rendement)
View(Portefolio_equi)

liste<-rbind(basebis,Portefolio_equi,fill=T)
tab <- liste[!is.na(rdmt), .(titre, rdmt)]
tab <- tab[, .(Er = round(mean(rdmt), 10),
               sd = round(sd(rdmt), 10)),
           by = "titre"]
tab

liste<-rbind(basebis,Portefolio_equi,fill=T)
tab2 <- liste[!is.na(rendement), .(titre, rendement)]
tab2 <- tab2[, .(Er = round(mean(rendement), 10),
               sd = round(sd(rendement), 10)),
           by = "titre"]
tab2

new_row <- data.frame(titre = "Portefeuille", Er = tab2$Er, sd = tab2$sd)
tab <- rbind(tab, new_row)
tab

# Dans un plan moyenne-variance, représentez les 9 séries
plot_moyvar = ggplot(tab, aes(x = sd, y = Er, color = titre)) +
  geom_point(size = 3) +
  theme_light() + ggtitle("Risk-Return Tradeoff") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent) +
  scale_x_continuous(label = percent)

plot_moyvar = plot_moyvar + labs(title = "Risk-Return Tradeoff",
                                 caption = "Data source: Yahoo Finance")

plot_moyvar = plot_moyvar + theme(
  plot.title = element_text(color = "#996666", size = 12, face = "bold"),
  plot.caption = element_text(color = "#330066", face ="plain")
)

plot_moyvar

### Question 2
##Frontière d'efficience ALAGR, MLAAH VFINX
Aisrdmt <- xts(Ais[,4], order.by=as.Date(Ais$Date))
Amundirdmt <- xts(Amundi[,4], order.by=as.Date(Amundi$Date))
Anderdmt <- xts(Ande[,4], order.by=as.Date(Ande$Date))
Carmignacrdmt <- xts(Carmignac[,4], order.by=as.Date(Carmignac$Date))
Epsensrdmt <- xts(Epsens[,4], order.by=as.Date(Epsens$Date))
Fidelityrdmt <- xts(Fidelity[,4], order.by=as.Date(Fidelity$Date))
Mainfirstrdmt <- xts(Mainfirst[,4], order.by=as.Date(Mainfirst$Date))
Pluvalcardmt <- xts(Pluvalca[,4], order.by=as.Date(Pluvalca$Date))
Thematicsrdmt <- xts(Thematics[,4], order.by=as.Date(Thematics$Date))

data <- data.table(Amundirdmt$rdmt, Mainfirst$rdmt, Ande$rdmt, na.omit = TRUE)
data<-data[,-4]
names(data) <- c("x","y","z")
data$x <- as.numeric(data$x)
data$y <- as.numeric(data$y)
data$z <- as.numeric(data$z)
data <- na.omit(data)
summary(data)
str(data)
View(data)

#L'ESPERANCE#
er_x <- mean(data$x)
er_y <- mean(data$y)
er_z <- mean(data$z)
#L'ECART-TYPE#
sd_x <- sd(data$x)
sd_y <- sd(data$y)
sd_z <- sd(data$z)
#LA COVARIANCE
cov_xy <- cov(data$x, data$y)
cov_xz <- cov(data$x, data$z)
cov_yz <- cov(data$y, data$z)
#CREATION DU VECTEUR#
x_weights <- seq(from = 0, to = 1, length.out = 1000)
#CREATION D'UNE TABLE A TROIS TITRES#
three_assets <- data.table(wx = rep(x_weights, each = length(x_weights)),
                           wy = rep(x_weights, length(x_weights)))
three_assets[, wz := 1 - wx - wy]
#CALCULE DE L'ESPERANCE ET L'ECART-TYPE POUR LES PORTE FEUILLES#
three_assets[, ':=' (er_p = wx * er_x + wy * er_y + wz * er_z,
                     sd_p = sqrt(wx^2 * sd_x^2 +
                                   wy^2 * sd_y^2 +
                                   wz^2 * sd_z^2 +
                                   2 * wx * wy * cov_xy +
                                   2 * wx * wz * cov_xz +
                                   2 * wy * wz * cov_yz))]
three_assets <- three_assets[wx >= 0 & wy >= 0 & wz >= 0]
three_assets

#REPRESENTATION DU PORTE FEUILLES#
ggplot() +
  geom_point(data = three_assets, aes(x = sd_p, y = er_p, color = wx - wz)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y, sd_z), mean = c(er_x, er_y, er_z)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  theme_bw() +
  ggtitle("Possible Portfolios with Three Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(three_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(three_assets$sd_p) * 1.2)) +
  scale_color_gradientn(colors = c("#FFCC99", "#99FFCC", "#CC99FF"),
                        name = expression(omega[x] - omega[z]), labels = percent)


## Portefeuille de variance minimale
assetSymbols <- c('Amundi','Mainfirst','Ande')
assetReturns <- data
assetReturns <- data.frame(assetReturns)
mu <- colMeans(assetReturns)
cov.mat <- cov(assetReturns)

getMinVariancePortfolio <- function(mu,covMat,assetSymbols) {
  U <- rep(1, length(mu))
  O <- solve(covMat)
  w <- O%*%U /as.numeric(t(U)%*%O%*% U)
  Risk <- sqrt(t(w) %*% covMat %*% w)
  ExpReturn <- t(w) %*% mu
  Weights <- `names<-`(round(w, 10), assetSymbols)
  list(Weights = t(Weights),
       ExpReturn = round(as.numeric(ExpReturn), 10),
       Risk = round(as.numeric(Risk), 10))
}

Pf_VM <- getMinVariancePortfolio(mu, cov.mat,assetSymbols)
Pf_VM

## Portefeuille tangent
assetsNames <- c('Amundirdmt','Aisrdmt','Fidelityrdmt')
E = c(er_x,er_y,er_z)

r.free = 2.5/(100*252)
cov.mat <- cov(assetReturns)

E <- as.vector(E)
cov.mat <- as.matrix(cov.mat)
cov.mat.inv <- solve(cov.mat)

w.t <- cov.mat.inv %*% (E - r.free)
w.t <- as.vector(w.t/sum(w.t))

names(w.t) <- assetsNames
E.t <- crossprod(w.t,E)
Sd.t <- sqrt(t(w.t) %*% cov.mat %*% w.t)
PTangent <- list("Weights" = w.t,
                 "ExpReturn" = as.vector(E.t),
                 "Risk" = as.vector(Sd.t))
PTangent


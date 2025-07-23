# Etude de l'authenticité du Saumon en fonction de l'origine géographique et du mode d'élevage.
## Lecture et Interprétation des données.
library(caret)
library(DiscriMiner)
library(FactoMineR)
library(klaR)
library(mixOmics)
library(e1071)
library(devtools)
### Importation du jeu de données
IPC_saumon <- read.csv("/Users/rododo/Desktop/ECAP/Modélisation des variables latentes/Dossier/ICPMS Raw data.csv", header=TRUE)
### Nouveaux noms des variables
names(IPC_saumon)
new_col_names <- c("Class", "Li", "Be", "B", "Na", "Mg", "Al", "Si", "P", "K",
                   "Ca", "Ti", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn",
                   "Ga", "Ge", "As", "Se", "Rb", "Sr", "Nb", "Mo", "Ag", "Cd",
                   "Cs", "Ba", "Ta", "W", "Tl", "Pb1", "Pb2", "Pb3")
colnames(IPC_saumon) <- new_col_names
names(IPC_saumon)
### Suppression des colonnes indésirables
IPC_saumon <- IPC_saumon [, c("Class","Li", "B", "Al", "V", "Cr", "Mn", "Fe", "Co", "Ni", "Cu", "Zn", "As", "Se", "Rb", "Sr", "Nb", "Mo", "Cd", "Cs", "Ta")]
length(IPC_saumon)
#Normalisation des données :
numeric_cols <- sapply(IPC_saumon, is.numeric) & colnames(IPC_saumon) != "Class"

IPC_saumon[, numeric_cols] <- lapply(IPC_saumon[, numeric_cols], function(x) (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)))
IPC_saumon$Class <- as.factor(IPC_saumon$Class)
## Analyse Descriptive du jeu de données
summary(IPC_saumon)
str(IPC_saumon)
### Visualisation des données
boxplot(IPC_saumon[,-c(1)], col = rainbow(ncol(IPC_saumon)-1),
        main = "Statistique descriptives du jeu de données")

points(1:ncol(IPC_saumon[,-c(1)]), colMeans(IPC_saumon[,-c(1)]), pch = 16, col = "black")
text(1:ncol(IPC_saumon[,-c(1)]) +0.1, colMeans(IPC_saumon[,-c(1)]), round(colMeans(IPC_saumon[,-c(1)]), 2), pos = 4, col = "black")
### Matrice de corrélation
cor_ICP3 <- round(cor(IPC_saumon[, numeric_cols]),2)
print(cor_ICP3)
corrplot::corrplot(cor(IPC_saumon[, numeric_cols]),method="shade",type="upper",
                   order="FPC")
## Découpage du jeu de données
saumon = IPC_saumon
set.seed(123)
intrain <- createDataPartition(saumon$Class, p=0.8, list=FALSE)


# Jeu d'apprentissage
X.app<- saumon[intrain,-1]
Y.app<- saumon[intrain,1]
# Jeu test
X.test<- saumon[-intrain,-1]
Y.test<- saumon[-intrain,1]
## Analyse de la répartition des échantillons
freq.test <- table(Y.test)
freq.app <- table(Y.app)
round(freq.test/103,6)
round(freq.app/418,6)
### Analyse Discriminante Partielle des Moindres Carrés ( PLS-DA)
plsda.saumon <- mixOmics::plsda(X.app,Y.app,ncomp=10, scale=FALSE)
### Visualisation des résultats de l'analyse PLS-DA
plotVar(plsda.saumon, comp=1:2)
plotIndiv(plsda.saumon, comp=1:2, centroid=TRUE,ellipse=TRUE,legend=TRUE)
### Choix du nombre de composantes
perf.saumon <- mixOmics::perf(plsda.saumon, validation = "Mfold", folds = 5, 
                              progressBar = FALSE, auc = TRUE, nrepeat=10)
plot(perf.saumon,measure = "overall", legend.position = "vertical")

perf.saumon$choice.ncomp
perf.saumon$error.rate

nb_comp=perf.saumon$choice.ncomp[1,1]
nb_comp

vip.saumon <- vip(plsda.saumon)[,nb_comp]
vip.saumon
barplot(vip.saumon,xlab=colnames(X.app),las=1)
abline(h=1,col="#FA8067")
## ROC
roc.saumon<-auroc(plsda.saumon,roc.comp=nb_comp)

## Modèle finale
### Prédictions des classes sur les données de test
plsda.fin.saumon<-mixOmics::plsda(X.app,Y.app,ncomp=nb_comp, scale=FALSE)
plsda.test<-predict(plsda.fin.saumon,dist="max.dist",newdata=X.test)
### Matrice de confusion
mat.confusion <- table(Y.test,plsda.test)
mat.confusion
### Prévison du modèle
sum(diag(mat.confusion))/sum(mat.confusion)
1-sum(diag(mat.confusion))/sum(mat.confusion)
### Sensibilité du modéle 
sensi <- diag(mat.confusion) / rowSums(mat.confusion)
sensi
mean(sensi)
# Calcul du taux de spécificité
speci <- diag(mat.confusion) / colSums(mat.confusion)
speci
mean(speci)


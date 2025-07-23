VIH = read_excel("/Users/rododo/Desktop/ECAP/AD/VIH/export.xlsx")
Z = c(2:14)
VIH[,Z] = apply(VIH[,Z], 2, as.numeric)
summary(VIH)
VIH_propre = na.omit(VIH[,])
summary(VIH_propre)

#ACP#
library(FactoMineR)
numerique_data <- VIH_propre[, 2:13]
standardise_data <- scale(numerique_data)
pca_resulta <- PCA(standardise_data, graph = FALSE)
summary(pca_resulta) #Résumé de l'ACP#

#CALCULE DES VARIABLES LES PLUS IMPORTANTES#
pca_resulta$var$coord
round(pca_resulta$var$contrib,2)
round(pca_resulta$var$cos2,2)

#INERTIE#
par(mfcol = c(1,1))
plot(pca_resulta$eig[,1],type = "b",main = "Valeurs propres",xlab="Nombre de dimensions",ylab="")
round(pca_resulta$eig,2)
sum(pca_resulta$eig^2)
inertie_totale <- sum(pca_resulta$eig^2)
print(paste("Inertie totale : ", round(inertie_totale, 2)))

#INDIVIDUS#
plot(pca_resulta, choix = "ind")
plot.PCA(pca_resulta,title="Graphique des individus", axes=c(1,2),choix="ind")
plot.PCA(pca_resulta,title="Graphique des individus", axes=c(3,4),choix="ind")

#CONTRIBUTION#
Contribution = print(pca_resulta$ind$contrib,1)
Contribution = print(pca_resulta$var$contrib,1)

plot(pca_resulta,title = "20 pays les plus contributifs",select = "contrib15")
plot(pca_resulta,title = "20 pays les plus contributifs",select = "contrib15", axes = c(1,3))

#VARIABLE#
print(pca_resulta$var)
round(pca_resulta$var$contrib,1)
plot(pca_resulta, choix = "var",axes = c(1,2))
plot(pca_resulta, choix = 'var',axes = c(1,3))

#SEPARATION DES VARIABLES#
#REGRESSION AVEC LES VARIABLES LATENTES 1 ET 2#

str(VIH_propre)
lm = lm(VIH_propre$`TAUX DE MORTALITE DU VIH` ~ pca_resulta$ind$coord[,1]+
          pca_resulta$ind$coord[,2])
summary(lm)
#REGRESSION AVEC LES VARIABLES LATENTES 1 ET 3#

lm2 = lm(VIH_propre$`TAUX DE MORTALITE DU VIH` ~ pca_resulta$ind$coord[,1]+
          pca_resulta$ind$coord[,3])
summary(lm2)

#REPRESENTATION DES RESIDUS#
modele = round(residuals(lm),2)
modele

VIH_actifs = VIH_propre[,2:12]
VIH_illus = VIH_propre[,1]


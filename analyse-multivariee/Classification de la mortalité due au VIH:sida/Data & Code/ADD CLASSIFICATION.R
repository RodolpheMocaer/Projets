#ADD CLASSIFICATION#
library(readxl)
library(ggplot2)
library(cluster)
library(FactoMineR)
library(factoextra)
library(NbClust)
library(VIM)

#1#
VIH = read_excel("~/Desktop/ECAP/Master 1/AD/VIH/export copie.xlsx")
VIH = as.data.frame(VIH)
rownames(VIH) <- VIH[, 1]
VIH = VIH[,-1]
new_names = c("VIH","PIB","VIE","ELEC","POL","POPA","ATV","SANTE","POPU","TUB","FEM","EAU","RNB")
colnames(VIH) = new_names
VIH_propre = na.omit(VIH[,])
VIH_propre[] <- lapply(VIH_propre, function(x) as.numeric(as.character(x)))
str(VIH_propre)
palet <- c("#1B9E77" ,"#D95F02" ,"#7570B3","#E7298A", "#66A61E", "#E6AB02", "#A6761D", "#666666")
summary(VIH_propre[,])
str(VIH_propre)
boxplot(VIH_propre[,-1])
VIH_propre <- VIH_propre[,-1]
boxplot(VIH_propre)

#2 - ACP #
res.pca <- PCA (VIH_propre,scale.unit=TRUE)
res.pca$eig
res<-barplot(res.pca$eig[,2],xlab="Dim.",ylab="Percentage of variance")
par(mfrow=c(1,1))
plot(res.pca,choix="var")
plot(res.pca,choix="ind",cex=0.8)

#3 - Variance#
diag(var(VIH_propre))

#4 - Classification#
VIH_propre.cr <-  scale(VIH_propre, center = TRUE , scale = TRUE) #standardisation des données#
boxplot(VIH_propre.cr)
VIH_propre.d  <-  dist(VIH_propre.cr, method = "euc")#calcul des distances#
VIH_propre.hca <- hclust(VIH_propre.d, method = "ward.D2")  #CAH avec Ward#
barplot(rev(VIH_propre.hca$height), 
        xlab = "Agg.", 
        ylab = "Delta I intra",
        col = "skyblue",  # Couleur des barres
        main = "Graphique de Barres",  # Titre du graphique
        border = "black",  # Couleur de la bordure des barres
        ylim = c(0, max(rev(VIH_propre.hca$height)) + 10))  # Plage de l'axe y
plot(VIH_propre.hca,hang=-1,cex=0.8)
VIH_propre.NbClust<-NbClust(data=VIH_propre.cr,distance="euclidean",method="ward.D2")

#5 - Représentation de K-classe#
K=3 
part <- cutree(VIH_propre.hca,K)
plot(VIH_propre.hca,hang=-1,cex=0.8)
rect.hclust(VIH_propre.hca,k=K)
part_fviz <- hcut(VIH_propre.d,stand=FALSE,k=K,isdiss=TRUE,hc_func="hclust",hc_method="ward.D2")
fviz_dend(part_fviz, rect = TRUE, cex = 0.5,k_colors =palet ,main=paste("Dendrogramme avec ",K," classes"))

#6 - Partition / barycentre#
#A#
part.cah <- cutree(VIH_propre.hca,k=K)
print("Partition obtenue par la CAH")
part.cah

#B#
print("Barycentres de la partition obtenue par la CAH")
bary.cah <- aggregate(VIH_propre.cr, by=list(part),mean)
round(bary.cah,2)

#7 - Qualité#
ss <- function(x) sum(scale(x, scale = FALSE)^2)
part.cah.ss <- ss(VIH_propre.cr)
part.cah.ss
wss <- function(part,x) {li <- by(1:length(part),as.factor(part),FUN=list);
sum(unlist(lapply(li, function(g){ ss(x[g,])})))}
part.cah.wss <- wss(part, VIH_propre.cr)
paste('qualité de la partition égale à :',round(1-part.cah.wss/part.cah.ss,2)) #Qualité, Iinter/Itotale

#8 - A - Consolidation kmeans#
res.kmeans<-kmeans(VIH_propre.cr, centers=bary.cah[,-1],algorithm="MacQueen") #Changer centers et mettre un chiffre pour aléatoire#
res.kmeans
res.kmeans$iter
part<- res.kmeans$cluster
table(part)
for (h in 1:K) {
  li <- which(res.kmeans$cluster==h)
  if (length(li)==1)
    print(paste('Singleton',h,':',names(res.kmeans$cluster[li])))
  else {
    parangon <- which.min(as.matrix(dist(rbind(res.kmeans$centers[h,],VIH_propre.cr[li,])))[-1,1])
    print(paste('plus proche individu du barycentre de la classe',h,':',names(parangon)))
  }
}
round(res.kmeans$centers,2)

#8 - B - Consolidation kmeans aléatoire #
res.kmeans<-kmeans(VIH_propre.cr, centers=3,algorithm="MacQueen",nstart = 50) #Refait 50 avec 3 centre#
res.kmeans$iter #Nombre d'itération jusqu'à la convergence dans ce modèle/parangon#
res.kmeans$betweenss/res.kmeans$totss #Qualité si initialisation aléatoire#
paste('qualité de la partition égale à :',res.kmeans$betweenss/res.kmeans$totss)


#9 - Test#
catdes(data.frame(Partition=as.factor(part),VIH_propre.cr),num.var=1)

#10 - Visualisation#
#A#
par(mfrow=c(1,1))
barplot(res.kmeans$centers,col=palet[1:K],ylim=c(-2,4),beside=T,las=2)
legend(x="topright", legend=paste("C",1:K,sep=""), cex=0.8,fill=palet[1:K],bty="y") 
part <- res.kmeans$cluster

#B#
acp <- PCA(data.frame(Partition = part, VIH_propre),scale.unit = TRUE ,quali.sup = 1)
acp
rai=data.frame(comp=1:min(10,nrow(acp$eig)),cum=acp$eig[1:min(10,nrow(acp$eig)),3])
round(acp$eig,2)

#C#
choix.axes=c(1,2) 
pvar=fviz_pca_var(acp,axes = choix.axes,repel = TRUE,labelsize =4,arrowsize =1,circlesize=1)+theme_bw()+theme(axis.title = element_text(face="bold",size=10))
print(pvar)

#D#
pind=fviz_pca_ind(acp,axes = choix.axes,label="all",repel=TRUE,pointsize =2,habillage = 1  ### à compléter
                  ,addEllipses = TRUE,palette=palet)+theme_bw()+theme(axis.title = element_text(face="bold",size=15))+coord_fixed()
print(pind)

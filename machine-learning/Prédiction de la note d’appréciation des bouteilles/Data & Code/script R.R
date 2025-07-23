# Librairies

library(FactoMineR)
library(factoextra)
library(caret)
library(rpart)
library(partykit)
library(randomForest)
library(VSURF)


# Chargement et vérification des données
dat<-read.csv2("~/Desktop/ECAP/Master 2/Arbre de décision/EVELOCHE DOSSIER/liking_Silhouettes_bootles.csv", na.strings="")
str(dat)
table(dat$Subject)
# 26 sujets
table(dat$Image)
# 49 images de silhouettes de bouteilles (11 jus de fruits, 4 boissons maltés, 16 sodas, 3 boissons pour sportifs, 7 thés, 8 eaux)
table(dat$liking)
# échelle de 1 à 9 mais aucune note maximale enregistrée


# Boxplots

par(mfrow=c(3,4))
for (j in 4:14) boxplot(dat[,j]~dat$liking,main=colnames(dat)[j],las=2,col=palette())
par(mfrow=c(1,1))
# les paramètres lid.Width, Skewness, Kurtosis, Mean des bouteilles notamment semblent bien distinguer les notes données.
# En effet, des notes plus élevées semblent avoir été données aux bouteilles avec une mean, kurtosis et lid.Width plus faible et une skewness plus élevée


# Cercle de corrélations

respca=PCA(dat[,3:14], quali.sup=1,graph=FALSE)
fviz_eig(respca)
fviz_pca_biplot(respca,habillage=1)
fviz_pca_ind(respca,habillage=1,addEllipses = TRUE)
# Corrélation positive entre Centroid et Body Height (première dimension)
# corrélation positive entre Kurtosis et Lid Width sur deuxième dimension et négative avec Skewness
# Moins bonnes notes vers le bas (Kurtosis, Lid Width élevé et Skewness faible)
# Les meilleures notes semblent davantage réparties en haut à droite


# Séparation du jeu de données (train/test)

DATA=dat[,-(1:2)]
# retrait id sujet et id bouteille
ntot=nrow(DATA)

set.seed(123)
train<-createDataPartition(DATA$liking,p=0.8,list=FALSE)
test=setdiff(1:ntot,train)
DATAtrain=DATA[train,]
DATAtest=DATA[test,]
n=nrow(DATAtrain)
ntest=nrow(DATAtest)

# vérification de la repartition
round(table(DATA$liking)/ntot,3)
round(table(DATAtrain$liking)/n,3)
round(table(DATAtest$liking)/ntest,3)


########################################################
# Classification tree : CART (arbre de régression)
########################################################


#----------------------------------------------------------
# sur les donnees d'apprentissage
#----------------------------------------------------------
fit.rpart <- rpart(liking~ .,data=DATAtrain,method="anova",
                   control=rpart.control(minbucket=10,  cp=0.0001, xval=10))
# "anova" pour méthode choisie = classification. Par défaut il regarde en fonction nature Y
# minbucket = feuilles finales doivent avoir minimum 10 observations : permet d'éviter le surapprentissage, en forçant l'arbre à ne pas trop se diviser en petits groupes.
# cp permet de savoir juste ou on va dans la complexité par défaut à 0.01 mais mettre 0.0001 permet d'aller plus profond mais élagage après
# xval = nb de segment de la cross validation qui permet de voir si bon deal entre gain d'explication et complexité de l'arbre, permet d'évaluer l'arbre

plotcp(fit.rpart)
# on cherche le point le plus bas en erreur de CV
# le point minimum + 1 écart type
# dans ce cas on choisirait 4 car en dessous ligne pointillée
printcp(fit.rpart)
# n split = nb de noeuds
# nb de feuilles = nb+1
# Choix de cp=0.01375317 ligne 4

choixcp=fit.rpart$cptable[4,1]
fit.rpart.prune=prune(fit.rpart,cp=choixcp)
summary(fit.rpart.prune)

X11(width=20, height=10)
partykit::party-plot(as.party(fit.rpart.prune),gp=gpar(cex=0.5))
detach("package:partykit", unload=TRUE)

#-----------------------------------------------------------
# prediction sur le jeu d'apprentissage
#---------------------------------------------------------
help(predict.rpart)
predtrain=predict(fit.rpart.prune,type="vector")


# MSE
mse <- mean((predtrain - DATAtrain$liking)^2)
mse
# RMSE
rmse <- sqrt(mse)
rmse
# Pseudo-R2
SS_residual <- sum((DATAtrain$liking - predtrain)^2)
SS_total <- sum((DATAtrain$liking - mean(DATAtrain$liking))^2)
R_squared <- 1 - (SS_residual/SS_total)
R_squared


#----------------------------------------------------------
# prediction sur le jeu de test
#-----------------------------------------------------------
predtest=predict(fit.rpart.prune,newdata=DATAtest,type="vector")

# MSE
mse <- mean((predtest - DATAtest$liking)^2)
mse
# RMSE
rmse <- sqrt(mse)
rmse
# Pseudo-R2
SS_residual <- sum((DATAtest$liking - predtest)^2)
SS_total <- sum((DATAtest$liking - mean(DATAtest$liking))^2)
R_squared <- 1 - (SS_residual/SS_total)
R_squared



######################################################
# randomForest
######################################################

#------------------------------------------------------------
# Premier ajustement du modele sur le jeu d'apprentissage
#------------------------------------------------------------ 

p<-ncol(DATAtrain)-1
n<-nrow(DATAtrain)
valntree=500
valmtry= floor(sqrt(p))
valnodesize=1
rdf=randomForest(formula=liking~.,data=DATAtrain, 
                 ntree=valntree, mtry=valmtry, nodesize = valnodesize, 
                 importance=TRUE, proximity=TRUE,nPerm=1)
print(rdf)

# illustration des erreurs de classification qui évoluent en fonction du nb d'arbres de la forêt
plot(rdf)
rdf$mse
rdf$rsq


# prediction sur les données OOB

PredOOB=predict(rdf)

# MSE
mse <- mean((PredOOB - DATAtrain$liking)^2)
mse
# RMSE
rmse <- sqrt(mse)
rmse
# Pseudo-R2
SS_residual <- sum((DATAtrain$liking - PredOOB)^2)
SS_total <- sum((DATAtrain$liking - mean(DATAtrain$liking))^2)
R_squared <- 1 - (SS_residual/SS_total)
R_squared


#----------------------------------------------------
#"tuning" du parametre mtry (et nodesize)
#---------------------------------------------------- 
# en fct de mtry
# avec caret
# avec method="rf" le seul "tuning parameter" possible est "mtry"
valntree=200
vecmtry=1:10
Nrep=50
fit.control <- trainControl(method = "boot",number=Nrep,  search='grid')
tune.mtry <- expand.grid(.mtry = vecmtry) 
deb=Sys.time()
rf.grid<- train(liking ~ ., 
                data = DATAtrain,
                method = "rf", 
                ntree=valntree,
                nodesize=valnodesize,
                metric='RMSE', 
                tuneGrid =tune.mtry,
                trControl=fit.control)


fin=Sys.time()
print(fin-deb)  
print(rf.grid)
plot(rf.grid)
# ici il y a peu d'impact selon mtry
# mtry = 7 a donné le meilleur résultat en termes de RMSE = 1.086942
# prendre par ex valmtry=7


#-----------------------------------------------------------------
# ajustement avec parametres choisis
#-----------------------------------------------------------------
rdf=randomForest(liking~.,data=DATAtrain, ntree=500,mtry=7, nodesize=1,
                 importance=TRUE, proximity=TRUE,nPerm=1)
print(rdf); 
plot(rdf)


#---------------------------------------------------------------------
# prediction sur le jeu test
#-------------------------------------------------------------------
predRF=predict(rdf,newdata=DATAtest)

# MSE
mse <- mean((predRF - DATAtest$liking)^2)
mse
# RMSE
rmse <- sqrt(mse)
rmse
# Pseudo-R2
SS_residual <- sum((DATAtest$liking - predRF)^2)
SS_total <- sum((DATAtest$liking - mean(DATAtest$liking))^2)
R_squared <- 1 - (SS_residual/SS_total)
R_squared


#--------------------------------------------------------------------
# Variable Importance
#-------------------------------------------------------------
varImpPlot(rdf)
Imp=importance(rdf,scale=TRUE)
Imp
# variables les plus importantes Lid.Width , Skewness, Kurtosis ou encore Body.Width et Centroid.Y
# variables les moins importantes Lid.Height et Body.Height..


# ------------------------------------------------------------
# Selection des variables d'importance (randomForest)
# etape 1
# ------------------------------------------------------------
ntree=500;mtry=7; nodesize=1
Nrep=50
K=nlevels(DATA$liking)
VIrepT=matrix(NA,Nrep,p)
VIrepordT=matrix(NA,Nrep,p)
for (rep in 1:Nrep) {
  rdf=randomForest(formula=liking~.,data=DATA, ntree=valntree, mtry=valmtry, nodesize = valnodesize, importance=TRUE, nPerm=1)
  VIrepT[rep,]=importance(rdf,scale=TRUE)[,K+1]                  # MDA
  VIrepordT[rep,]=order(importance(rdf,scale=TRUE)[,K+1],decreasing=T)
}
rdfimp=sort(apply(VIrepT,2,mean),decreasing=TRUE)
rdfimpord=order(apply(VIrepT,2,mean),decreasing=TRUE)
names(rdfimp)=colnames(DATAtrain[,-1])[rdfimpord]
par(mar=c(8, 4, 4, 2) + 0.1)
boxplot(VIrepT[,rdfimpord],names=names(rdfimp),las=2,cex.axis=1.2)
abline(h=0)
# Niveau moyen d'importance des variables des forêts réalisées

# ----------------------------------------------------
# selection de variables (VSURF)
# ----------------------------------------------------
resVSURF<-VSURF(formula=liking~.,data=DATA,mtry=7,
                ntree.thres=500, nfor.thres=20,
                ntree.interp=200,nfor.interp=20,
                ntree.pred=200,nfor.pred=20)
# fait un test pour savoir les var importantes en terme d'interprétation
plot(resVSURF)
summary(resVSURF)
# VSURF utilise comme critère VI : MDA non standardisé
colnames(DATA)[resVSURF$varselect.thres+1]
colnames(DATA)[resVSURF$varselect.interp+1]
colnames(DATA)[resVSURF$varselect.pred+1]












######################################################################
# gradient boosting
######################################################################

#--------------------------------------------------------------------------
# with gbm
#--------------------------------------------------------------------------
library(gbm)
#--------------------------------------------------------------------------
#pour voir effet du paramètre de shrinkage
vecvalshrink=c(0.1,0.05,0.01,0.001)
matres=NULL
deb=Sys.time()
for (valshrink in vecvalshrink) {
  fit.gbm<- gbm(liking~., data=DATAtrain, 
                n.trees=5000,interaction.depth=2,
                shrinkage = valshrink, bag.fraction=0.5,
                cv.folds=5)
  matres=cbind(matres,fit.gbm$cv.error)
}
print(Sys.time()-deb)  # 1.6 min
X11();
matplot(matres,type="l",col=1:length(vecvalshrink),lty=1,main="depth=2",ylab="_CV",
        xlab="number of trees")
legend("topright",col=1:length(vecvalshrink),lty=1,paste("shrink",vecvalshrink))

# pour voir l'effet de interaction.depth
vecvaldepth=1:5
matres=NULL
deb=Sys.time()
for (valdepth in vecvaldepth) {
  fit.gbm<- gbm(Resp_Category~., data=DATAtrain, 
                n.trees=1000,interaction.depth=valdepth,
                shrinkage = 0.05, bag.fraction=0.5,
                cv.folds=5)
  matres=cbind(matres,fit.gbm$cv.error)
}
print(Sys.time()-deb)  # 42 sec
X11();
matplot(matres,type="l",col=1:length(vecvaldepth),lty=1,main="shrink=0.05",ylab="_CV",
        xlab="number of trees")
legend("topright",col=1:length(vecvaldepth),lty=1,paste("depth",vecvaldepth))


# ajustement avec parametres choisis
fit.gbm<- gbm(Resp_Category~., data=DATAtrain,
              n.trees=50,interaction.depth=4,
              shrinkage = 0.05, bag.fraction=0.5,cv.folds=5)
#Distribution not specified, assuming multinomial ...
#Message d'avis :
#Setting `distribution = "multinomial"` is ill-advised as it is currently broken.
# It exists only for backwards compatibility. Use at your own risk. 
fit.gbm$train.error
gbm.perf(fit.gbm)
probClasstrain = predict(fit.gbm,n.trees=50,type='response')
predGBMtrain <- factor(apply(probClasstrain, 1, which.max),,labels=levels(dat$Resp_Category))
resGBMtrain=confusionMatrix(predGBMtrain,DATAtrain$Resp_Category)
round(resGBMtrain$overall,4)

probClasstest = predict(fit.gbm,newdata=DATAtest,n.trees=50,type='response')
predGBMtest <- factor(apply(probClasstest, 1, which.max),,labels=levels(dat$Resp_Category))
resGBMtest=confusionMatrix(predGBMtest,DATAtest$Resp_Category)
round(resGBMtest$overall,4)
round(1-resGBMtest$overall,4)

summary(fit.gbm,las=1)


#--------------------------------------------------------------------------
library(xgboost)
#--------------------------------------------------------------------------
# eta = learning rate
ytrain=as.numeric(DATAtrain$Resp_Category)-1
Xtrain=as.matrix(DATAtrain[,-1])

#pour voir effet du paramètre de shrinkage
vecvaleta=c(0.01,0.1, 0.3, 0.8)
matres=NULL
deb=Sys.time()
for (valshrink in vecvalshrink) {
  fit.xgb=xgb.cv(data=Xtrain,label=ytrain,nfold=5,
                 objective="multi:softprob",num_class=6,
                 nrounds = 1000,  max_depth = 2,  
                 eta = vecvaleta,nthread=2,
                 verbose=FALSE)
  matres=cbind(matres,fit.xgb$evaluation_log$test_mlogloss_mean)
}
print(Sys.time()-deb)  # 1.1 min
X11();
matplot(matres,type="l",col=1:length(vecvaleta),lty=1,main="max_depth=2",ylab="_CV",
        xlab="nb rounds")
legend("topright",col=1:length(vecvaleta),lty=1,paste("learning rate",vecvaleta))

# pour voir l'effet de max.depth
vecvaldepth=1:8
matres=NULL
deb=Sys.time()
for (valdepth in vecvaldepth) {
  fit.xgb=xgb.cv(data=Xtrain,label=ytrain,nfold=5,
                 objective="multi:softprob",num_class=6,
                 nrounds = 1000,  max_depth = valdepth,  
                 eta = 0.1,nthread=2,
                 verbose=FALSE)
  matres=cbind(matres,fit.xgb$evaluation_log$test_mlogloss_mean)
}
print(Sys.time()-deb)  # 
X11();
matplot(matres,type="l",col=1:length(vecvaldepth),lty=1,main="eta=0.1",ylab="logloss",
        xlab="number of trees")
legend("topright",col=1:length(vecvaldepth),lty=1,paste("depth",vecvaldepth))


# ajustement avec parametres choisis
fit.xgb=xgboost(data=Xtrain,label=ytrain,
                objective="multi:softprob",num_class=6,
                nrounds = 50,  max_depth = 4,  
                eta = 0.1,nthread=2,
                verbose=FALSE)
# eta = learning rate
fit.xgb

predtrain=predict(fit.xgb,newdata=Xtrain)  # a n x nb classes vector
predtrain=matrix(predtrain,n,6,byrow=TRUE)
predXGBtrain=factor(apply(predtrain,1,which.max),labels=levels(dat$Resp_Category))
resXGBtrain=confusionMatrix(predXGBtrain,DATAtrain$Resp_Category)
resXGBtrain$table
round(resXGBtrain$overall,4)

ytest=as.numeric(DATAtest$Resp_Category)-1
Xtest=as.matrix(DATAtest[,-1])
predtest = predict(fit.xgb,newdata=Xtest)
predtest=matrix(predtest,ntest,6,byrow=TRUE)
predXGBtest <- factor(apply(predtest, 1, which.max),,labels=levels(dat$Resp_Category))
resXGBtest=confusionMatrix(predXGBtest,DATAtest$Resp_Category)
resXGBtest$table
round(resXGBtest$overall,4)

importance_matrix <- xgb.importance(colnames(Xtrain), model = fit.xgb)
xgb.plot.importance(importance_matrix, rel_to_first = TRUE, xlab = "Relative importance")












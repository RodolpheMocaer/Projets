# --------------------- Packages --------------------------------------------- #
library(sf)
library(ggplot2)
library(cartography)
library(RColorBrewer)
library(classInt)
library(corrplot)
library(mapview)
library(spdep)
library(spatialreg)
library(car)


# --------------------- Importation et création des données ------------------ #
# Importation du fond de carte
carte <- st_read("Départements/DEPARTEMENT.shp")
str(carte)
st_crs(carte)

ggplot(data = carte) + geom_sf() + theme_minimal() # Premier aperçu du fond de carte

# Importation des données
labo <- read.csv2("Labo_data.csv")
str(labo)

# Création du fichier shp = fond de carte + données
shp_labo <- merge(carte, labo, by.x = "INSEE_DEP", by.y="Code",all.x=TRUE)


# ----------------------- Représentations géographiques des variables -------- #

# Densité labo
ggplot(data = shp_labo) +
  geom_sf(aes(fill = Dens_labo_2020), color = NA) +
  scale_fill_viridis_c(option = "mako",) +
  labs(title = "Densité des labo d'analyse par département (2020)",
       fill = "Nombre de Labo/
100 000 habitants") +
  theme_minimal() +
  theme(text = element_text(family = "Baskerville"),
        plot.title = element_text(face = "bold", size = 14, 
                                  color ="black"),
    panel.background = element_blank(),  
    axis.text = element_blank(),         
    axis.title = element_blank(),        
    axis.ticks = element_blank(),       
    panel.grid = element_blank(),        
    plot.background = element_blank()    
  )

# Taux de chômage
ggplot(data = shp_labo) +
  geom_sf(aes(fill = Tx_chomage_2020), color = NA) +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  labs(title = "Taux de chômage par département (2020)",
       fill = "Taux de 
chômage (%)") +
  theme_minimal() +
  theme(text = element_text(family = "Baskerville"),
        plot.title = element_text(face = "bold", size = 14, 
                                  color ="black"),
        panel.background = element_blank(),  
        axis.text = element_blank(),         
        axis.title = element_blank(),        
        axis.ticks = element_blank(),       
        panel.grid = element_blank(),        
        plot.background = element_blank()    
  )

# Part des résidances secondaires
ggplot(data = shp_labo) +
  geom_sf(aes(fill = Tx_res_sec_2020), color = NA) +
  scale_fill_viridis_c(option = "inferno", direction = -1) +
  labs(title = "Part de résidences secondaires par département (2020)",
       fill = "Taux de résidences
  secondaires (%)") +
  theme_minimal() +
  theme(text = element_text(family = "Baskerville"),
        plot.title = element_text(face = "bold", size = 14, 
                                  color ="black"),
        panel.background = element_blank(),  
        axis.text = element_blank(),         
        axis.title = element_blank(),        
        axis.ticks = element_blank(),       
        panel.grid = element_blank(),        
        plot.background = element_blank()    
  )

# Salaire net horaire moyen
ggplot(data = shp_labo) +
  geom_sf(aes(fill = Salaire_net_horaire), color = NA) +
  scale_fill_viridis_c(option = "cividis", direction = -1) +
  labs(title = "Salaire net horaire moyen par département (2020)",
       fill = "Taux de résidences
  secondaires (%)") +
  theme_minimal() +
  theme(text = element_text(family = "Baskerville"),
        plot.title = element_text(face = "bold", size = 14, 
                                  color ="black"),
        panel.background = element_blank(),  
        axis.text = element_blank(),         
        axis.title = element_blank(),        
        axis.ticks = element_blank(),       
        panel.grid = element_blank(),        
        plot.background = element_blank()    
  ) # Peut-etre que ce sera pas mal de faire des classes pour mieux voir les différences,
    # parce que là tout est capté par Paris

ggplot(data = shp_labo) +
  geom_sf(
    aes(fill = cut(
      Salaire_net_horaire,
      breaks = classIntervals(shp_labo$Salaire_net_horaire, n = 5, style = "quantile")$brks,
      include.lowest = TRUE                      # autre option -> style = "pretty"
    )),
    color = NA
  ) +
  scale_fill_brewer(palette = "YlGnBu") + 
  labs(
    title = "Salaire net horaire moyen par département en fonction de 5 classes (2020)",
    fill = "Salaire net horaire (€)"
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Baskerville"),
    plot.title = element_text(face = "bold", size = 14, color = "black"),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_blank()
  )

# Nombre de naissances
ggplot(data = shp_labo) +
  geom_sf(aes(fill = Nbre_naissances), color = NA) +
  scale_fill_viridis_c(option = "viridis", direction = -1) +
  labs(title = "Nombre de naissances par département (2020)",
       fill = "Nombre de
 naissances") +
  theme_minimal() +
  theme(text = element_text(family = "Baskerville"),
        plot.title = element_text(face = "bold", size = 14, 
                                  color ="black"),
        panel.background = element_blank(),  
        axis.text = element_blank(),         
        axis.title = element_blank(),        
        axis.ticks = element_blank(),       
        panel.grid = element_blank(),        
        plot.background = element_blank()    
  )

# -------------------------- Matrice de corrélation -------------------------- #

# Calculer la matrice de corrélation
corr_mat <- cor(labo[, c(3:7)]) # Variables numériques uniquement

# Afficher la matrice de corrélation
corrplot(
  corr_mat,
  method = "circle",       
  type = "upper",         
  order = "hclust",        
  addCoef.col = "black",  
  tl.col = "black",       
  tl.srt = 45,            
  col = brewer.pal(9, "YlOrRd"),
  diag = FALSE           
)

# --------------------- Statistique descriptives ----------------------------- #
summary(labo)
var(labo$Dens_labo_2020)
which.max(labo$Dens_labo_2020)
which.min(labo$Dens_labo_2020)
# --------------------- Plus proche voisin ----------------------------------- #
# --------------------- Tour ------------------------------------------------- #
nb_tour <- poly2nb(shp_labo, row.names=shp_labo$NOM_M, queen = FALSE)
summary(nb_tour)
nb_lines <- nb2lines(nb_tour, coords = st_centroid(st_geometry(shp_labo)))
nb_sf <- st_as_sf(nb_lines)
ggplot() +
  geom_sf(data = shp_labo, fill = "lightblue", color = "black") + geom_sf(data = nb_sf, color = "red") +
  theme_minimal() +
  ggtitle("Carte des relations de voisinage de type 'Tour'")
WT<-nb2listw(nb_tour,style="W",zero.policy=TRUE)
WT
# --------------------- Reine ------------------------------------------------ #
nb_Reine <- poly2nb(shp_labo, row.names=shp_labo$NOM_M, queen = TRUE)
summary(nb_Reine)
nb_linesR <- nb2lines(nb_Reine, coords = st_centroid(st_geometry(shp_labo)))
nb_sfR <- st_as_sf(nb_linesR)
ggplot() +
  geom_sf(data = shp_labo, fill = "lightblue", color = "black") + geom_sf(data = nb_sfR, color = "red") +
  theme_minimal() +
  ggtitle("Carte des relations de voisinage de type 'Reine'")
WR<-nb2listw(nb_Reine,style="W",zero.policy=TRUE)
WR

# --------------------- la matrice de poids standardisée 1 voisin ------------ #
centroids <- st_centroid(st_geometry(shp_labo))
coords <- st_coordinates(centroids)
crs <- st_crs(carte)
coords_sf <- st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = crs)
coords_sp <- as(coords_sf, "Spatial")
k <- 1
knn_neighbours <- knearneigh(coords, k = k)
neighbors <- knn2nb(knn_neighbours)
summary(neighbors)
nb_lines <- nb2lines(neighbors, coords = coords_sp)
nb_sf <- st_as_sf(nb_lines)
ggplot() +
  geom_sf(data = shp_labo, fill = "lightblue", color = "black")+
  geom_sf(data = nb_sf, color = "red") +
  theme_minimal() +
  ggtitle(paste("Carte des relations de voisinage avec", k, "voisin(s) le(s) plus proche (s)"))
PPV1<- nb2listw(neighbors,style="W", zero.policy=TRUE)

# --------------------- la matrice de poids standardisée 3 voisin ------------ #
k3 <- 3
knn_neighbours3 <- knearneigh(coords, k = k3)
neighbors3 <- knn2nb(knn_neighbours3)
summary(neighbors3)
nb_lines3 <- nb2lines(neighbors3, coords = coords_sp)
nb_sf3 <- st_as_sf(nb_lines3)
ggplot() +
  geom_sf(data = shp_labo, fill = "lightblue", color = "black")+
  geom_sf(data = nb_sf3, color = "red") +
  theme_minimal() +
  ggtitle(paste("Carte des relations de voisinage avec", k3, "voisin(s) le(s) plus proche (s)"))
PPV3<- nb2listw(neighbors3,style="W", zero.policy=TRUE)

# --------------------- Indice de Moran  ------------------------------------- #
# --------------------- Tour ------------------------------------------------- #
globalMoran_WT <- moran.test(shp_labo$Dens_labo_2020,WT, zero.policy=TRUE,randomisation=TRUE)
globalMoran_WT
set.seed(22)
Moranperm_WT = moran.mc(shp_labo$Dens_labo_2020, WT, nsim = 999, zero.policy = TRUE)
Moranperm_WT
shp_labo$Dens_labo_2020 <- scale(shp_labo$Dens_labo_2020)
mp <- moran.plot(as.vector(shp_labo$Dens_labo_2020), WT
                 , xlab="Densité des laboratoires", ylab="Lag Densité des laboratoires"
                 ,main="Matrice type Tour", labels=as.character(shp_labo$NOM_M))
#même constat

# --------------------- Reine ------------------------------------------------ #
globalMoran <- moran.test(shp_labo$Dens_labo_2020,WR, zero.policy=TRUE,randomisation=TRUE)
globalMoran
set.seed(22)
Moranperm = moran.mc(shp_labo$Dens_labo_2020, WR, nsim = 999, zero.policy = TRUE)
Moranperm
shp_labo$Dens_labo_2020 <- scale(shp_labo$Dens_labo_2020)
mp <- moran.plot(as.vector(shp_labo$Dens_labo_2020), WR
                 , xlab="Densité des laboratoires", ylab="Lag Densité des laboratoires"
                 ,main="Matrice type Reine", labels=as.character(shp_labo$NOM_M))
#il existe une relation entre les taux de densité des labo des départements et +


# --------------------- la matrice de poids standardisée 1 voisin ------------ #
globalMoran_PPV1 <- moran.test(shp_labo$Dens_labo_2020,PPV1, zero.policy=TRUE,randomisation=TRUE)
globalMoran_PPV1
Moranperm_PPV1 = moran.mc(shp_labo$Dens_labo_2020, PPV1, nsim = 999, zero.policy = TRUE)
Moranperm_PPV1
mp <- moran.plot(as.vector(shp_labo$Dens_labo_2020), PPV1
                 , xlab="Densité des laboratoires", ylab="Lag Densité des laboratoires"
                 ,main="Matrice type plus proche voisin 1", labels=as.character(shp_labo$NOM_M))
#même constat

# --------------------- la matrice de poids standardisée 3 voisin ------------ #
globalMoran_PPV3 <- moran.test(shp_labo$Dens_labo_2020,PPV3, zero.policy=TRUE,randomisation=TRUE)
globalMoran_PPV3
Moranperm_PPV3 = moran.mc(shp_labo$Dens_labo_2020, PPV3, nsim = 999, zero.policy = TRUE)
Moranperm_PPV3
mp <- moran.plot(as.vector(shp_labo$Dens_labo_2020), PPV3
                 , xlab="Densité des laboratoires", ylab="Lag Densité des laboratoires"
                 ,main="Matrice type plus proche voisin 3", labels=as.character(shp_labo$NOM_M))
#même constat

# --------------------- Estimation des MCO  ---------------------------------- #
fit1 <- lm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
             Salaire_net_horaire+Nbre_naissances, data=shp_labo)
summary(fit1)
vif(fit1)
# --------------------- Reine ------------------------------------------------ #
moran.lm<-lm.morantest(fit1, WR, alternative="two.sided")
print(moran.lm)
# --------------------- Tour ------------------------------------------------- #
moran.lm1<-lm.morantest(fit1, WT, alternative="two.sided")
print(moran.lm1)
# ---------------------  1 voisin -------------------------------------------- #
moran.lm2<-lm.morantest(fit1, PPV1, alternative="two.sided")
print(moran.lm2)
# ---------------------  3 voisin -------------------------------------------- #
moran.lm3<-lm.morantest(fit1, PPV3, alternative="two.sided")
print(moran.lm3)

# --------------------- test de Lagrange & Modèle ---------------------------- #
# --------------------- Reine ------------------------------------------------ #
LM1<-lm.LMtests(fit1, WR, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM1)
#modèle SEM à prendre
sem_WR<-errorsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                     Salaire_net_horaire+Nbre_naissances, data=shp_labo, WR)
summary(sem_WR) #AIC = 219
# --------------------- Tour ------------------------------------------------- #
LM2<-lm.LMtests(fit1, WT, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM2)
#modèle SEM à prendre
sem_WT<-errorsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                     Salaire_net_horaire+Nbre_naissances, data=shp_labo, WT)
summary(sem_WT) #AIC = 219
# ---------------------  1 voisin -------------------------------------------- #
LM3<-lm.LMtests(fit1, PPV1, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM3)
#SEM significatif au seuil de 10%
# ---------------------  3 voisin -------------------------------------------- #
LM4<-lm.LMtests(fit1, PPV3, test=c("LMerr", "LMlag", "RLMerr", "RLMlag"))
print(LM4)
#modèle SEM à prendre
sem_PPV3<-errorsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                     Salaire_net_horaire+Nbre_naissances, data=shp_labo, PPV3)
summary(sem_PPV3)

# --------------------- méthode d’Elhorst ------------------------------------ #
# --------------------- Reine ------------------------------------------------ #
sdm_WR<-lagsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                   Salaire_net_horaire+Nbre_naissances, data=shp_labo, WR, type="mixed")
summary(sdm_WR) # AIC = 216
TestSDM_SEM_WR<-LR.Sarlm(sdm_WR,sem_WR) #216 / #219
print(TestSDM_SEM_WR)
#Différence significative
sar_WR<-lagsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                Salaire_net_horaire+Nbre_naissances, data=shp_labo, WR)
summary(sar_WR) # AIC = 229
TestSDM_SAR_WR<-LR.Sarlm(sdm_WR,sar_WR) #216 / #229
print(TestSDM_SAR_WR)
#Différence significative
# --------------------- Tour ------------------------------------------------- #
sdm_WT<-lagsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                   Salaire_net_horaire+Nbre_naissances, data=shp_labo, WT, type="mixed")
summary(sdm_WT) # AIC = 216
TestSDM_SEM_WT<-LR.Sarlm(sdm_WT,sem_WT) #216 / #219
print(TestSDM_SEM_WT)
#Différence significative
sar_WT<-lagsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                   Salaire_net_horaire+Nbre_naissances, data=shp_labo, WT)
summary(sar_WT) # AIC = 229
TestSDM_SAR_WT<-LR.Sarlm(sdm_WT,sar_WT) #216 / #229
print(TestSDM_SAR_WT)
#Différence significative
# ---------------------  1 voisin -------------------------------------------- #
slx_PPV1<-lmSLX(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                  Salaire_net_horaire+Nbre_naissances, data=shp_labo, PPV1)
summary(slx_PPV1)
AIC(slx_PPV1) # AIC = 254
impacts(slx_PPV1, listw=PPV1)
#Si théta = 0 on estime les MCO
#Sinon on le compare à un SDM ??? 
sdm_PPV1<-lagsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                     Salaire_net_horaire+Nbre_naissances, data=shp_labo, PPV1, type="mixed")
summary(sdm_PPV1) # AIC = 240
TestSDM_SLX_PPV1<-LR.Sarlm(sdm_PPV1,slx_PPV1) #240 / #254
print(TestSDM_SEM_WT)
#Différence significative

# ---------------------  3 voisin -------------------------------------------- #
sdm_PPV3<-lagsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                   Salaire_net_horaire+Nbre_naissances, data=shp_labo, PPV3, type="mixed")
summary(sdm_PPV3) # AIC = 224
TestSDM_SEM_PPV3<-LR.Sarlm(sdm_PPV3,sem_PPV3) #224 / #220
print(TestSDM_SEM_WT)
#Différence significative
sar_PPV3<-lagsarlm(Dens_labo_2020 ~ Tx_chomage_2020+Tx_res_sec_2020+
                   Salaire_net_horaire+Nbre_naissances, data=shp_labo, PPV3)
summary(sar_PPV3) # AIC = 235
TestSDM_SAR_PPV3<-LR.Sarlm(sdm_PPV3,sar_PPV3) #224 / #235
print(TestSDM_SAR_PPV3)
#Différence significative

# --------------------- Interprétation  -------------------------------------- #
# --------------------- Reine ------------------------------------------------ #
impacts.sdm<-impacts(sdm_WR, listw=WR)
impacts.sdm






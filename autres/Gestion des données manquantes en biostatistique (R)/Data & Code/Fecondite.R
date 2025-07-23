#LIBRARY#
library(dplyr)
library(tidyverse)
library(naniar)
library(glmnet)
library(readxl)
library(FactoMineR)
library(missMDA)
library(mice)
library(writexl)
library(gridExtra)
library(grid)

Donne_es_Econome_trie_L3_AE_xlsx <- read_excel("~/Desktop/ECAP/Biostatitique/Taux de fécondité/Données Econométrie L3 AE.xlsx.xlsx")
#VISUALISATION RAPIDE#
fecondite <- Donne_es_Econome_trie_L3_AE_xlsx
fecondite
fecondite <- fecondite[,-c(13:15)]
View(fecondite)
plot(fecondite)
summary(fecondite)

#CREATION DE LA COLONNE DVP ET SUPPRESION D'UNE OBSEVATION#
fecondite <- fecondite |> 
  mutate(Developpement = ifelse(`PIB/habitant` > 7733.8, "fort", "faible"))
obs <- sample(nrow(fecondite),1)
fecondite$Developpement[obs] <- NA
summary(fecondite)
fecondite$Developpement <- as.factor(fecondite$Developpement)

write_xlsx(fecondite , "/Users/rododo/Desktop/ECAP/AD/VIH/export.xlsx")


#VALEURS MANQUANTE#
naniar::gg_miss_upset(fecondite)

#ANALYSE UNIVARIEE#
#VAR QUALI#
count(fecondite, Continent)
count(fecondite, Developpement)
count(fecondite, Continent, Developpement)


#REPRESENTATION GRAPHIQUE#
vecteur_de_couleur <- c("Europe" = "blue","Afrique" = "green", "Asie" = "red", "Océanie" = "darkgoldenrod", 
                        "Amérique du Nord" = "orange", "Amérique Latine" = "pink","forte"="purple","faible"="brown")
#HISTOGRAMME#
#CONTINENT#
fecondite |> 
  ggplot() +
  aes(x = Continent, fill = Continent) +
  geom_bar(stat = "count") +
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = vecteur_de_couleur) +
  theme_classic() +
  labs(title = "Répartition des pays en fonction des continents",
                         x = "Continent",
                         y = "Nombre de pays")

fecondite |>
  ggplot(aes(x = Continent, y = 1, color = Continent, label = Pays)) +
  geom_text(
    aes(y = seq(0, 1, length.out = nrow(fecondite))),
    size = 3,
    vjust = -0.5,
    position = position_stack(vjust = 40)
  ) +
  scale_color_manual(values = vecteur_de_couleur) +
  labs(title = "Noms des pays par continent", x = "Continent", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Répartition des pays en fonction des continents",
       x = "Continent",
       y = "Nombre de pays")

#PERMET D'IDENTIFIER LE CONTINENT DES PAYS NA ET EGALEMENT QUE BRUNEI N'EST PAS CASSE DANS LE BON CONTINENT#
#RETROUVER LE NUMEROS DES LIGNES DE NOS NA DANS LA VARIABLE CONTINENT (2)#
indice_na <- which(is.na(fecondite), arr.ind = TRUE)
print(indice_na)
indice <- indice_na[,1]
fecondite[indice,]

#METHODES : VERIFICATION DES BONNES DONNEES GRACE A NOTRE CULTURE OU INTERNET#
#REMPLACEMENT POUR ARMENIE#
fecondite[6, 2] <- "Asie"

#REMPLACEMENT POUR ESPAGNE#
fecondite[36, 2] <- "Europe"

#IDENTIFICATION DE LA LIGNE DE BRUNEI ET REMPLACEMENT PAR LE BON CONTINENT#
brunei <- which(fecondite$Pays == "Brunei")
print(brunei)
fecondite[17, 2] <- "Asie"

#DEVELOPPEMENT#
#GRAPHIQUE 1 : HISTOGRAMME#
fecondite |> 
  ggplot()+
  aes(x=Developpement, fill= Developpement)+
  geom_bar() + 
  geom_text(aes(label = ..count..), stat = "count", position = position_stack(vjust = 0.5), size = 3, color = "black") +
  scale_fill_manual(values = vecteur_de_couleur)+
  theme_classic() + 
  labs(title = "Répartition des pays en fonction du développement",
       x = "Développement",
       y = "Nombre de pays")

fecondite |>
  ggplot(aes(x = Developpement, y = 1, color = Developpement, label = Pays)) +
  geom_text(
    aes(y = seq(0, 1, length.out = nrow(fecondite))),
    size = 3,
    vjust = -0.5,
    position = position_stack(vjust = 58)
  ) +
  scale_color_manual(values = vecteur_de_couleur) +
  labs(title = "Noms des pays par continent", x = "Continent", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Répartition des pays en fonction du développement",
       x = "Développement",
       y = "Nombre de pays")

#GRAPHIQUE 2 : CAMENBERT#
fecondite |> 
  count(Developpement) |> 
  ggplot()+
  aes(x="", y= n, fill= Developpement)+
  geom_bar(stat="identity") + 
  coord_polar("y")+
  scale_color_manual(values = vecteur_de_couleur) +
  theme_void()

#VAR QUANTI#
create_histogram_plot <- function(data, x_var, fill_var, bins = 30) {
  ggplot(data) +
    aes(x = !!sym(x_var), fill = !!sym(fill_var)) +
    geom_histogram(bins = bins, alpha = 0.7, position = "identity") +
    scale_fill_manual(values = vecteur_de_couleur) +
    labs(title = paste("Histogramme de", x_var, "par", fill_var),
         x = x_var,
         y = "Nombre de pays") +
    theme_minimal() +
    theme(legend.position = "top")  # Déplace la légende en haut
}
create_histogram_plot(fecondite, "Taux de fécondité", "Developpement", bins = 30)
create_histogram_plot(fecondite, "PIB/habitant", "Developpement")
create_histogram_plot(fecondite, "Taux d'irrélegieux", "Developpement", bins = 20)
create_histogram_plot(fecondite, "Mortalité infantile", "Developpement")
create_histogram_plot(fecondite, "Consommation (en $PPA)", "Developpement")
create_histogram_plot(fecondite, "Taux d'enfants non scolarisé", "Developpement")
create_histogram_plot(fecondite, "Taux de chômage", "Developpement")
create_histogram_plot(fecondite, "Pourcentage de femme dans la population active", "Developpement", bins = 10)
create_histogram_plot(fecondite, "Nombre de lit d'hopitaux pour 1000 habitant", "Developpement")
create_histogram_plot(fecondite, "IVG(0 à 6)", "Developpement")

#STATISTIQUE DESCRIPTIVES#
summary(fecondite)
#BOXPLOT POUR TOUTE LES NUMERIQUE#
fecondite |>
  pivot_longer(
    cols = where(is.numeric)
  ) |>
  ggplot() +
  aes(y = value) +
  facet_wrap(~ name, scales = "free_y") +
  geom_boxplot() +
  theme_light()

#ANALYSE BIVARIEE#
#QUANTI-QUANTI#
#RENOMMAGE DES DONNEES#
labels(fecondite)
fecondite2 <- fecondite |>
  rename(
    "fecondité" = "Taux de fécondité",
    "irréligieux" = "Taux d'irrélegieux",
    "pib/hab" = "PIB/habitant",
    "mort_infan" = "Mortalité infantile",
    "conso" = "Consommation (en $PPA)",
    "n_scolaire" = "Taux d'enfants non scolarisé",
    "chôm" = "Taux de chômage",
    "%femmes" = "Pourcentage de femme dans la population active",
    "lit_hopitaux" = "Nombre de lit d'hopitaux pour 1000 habitant",
    "IVG" = "IVG(0 à 6)"
  )
#VECTEUR DE COLONNE NUMERIQUE#
vecteur_col_numeriques <- fecondite2 |> 
  select(where(is.numeric)) |> 
  names()

#CORRELATION#
fecondite2 |>
  select(vecteur_col_numeriques) |>
  drop_na() |>
  cor() |>
  corrplot::corrplot.mixed()

#REGRESSION#
#CREATION DE COLONNE NUMERIQUE, POUR NOS CORRELATION#
col_num2 <- c("%femmes", "irréligieux", "pib/hab", "mort_infan", "conso", "n_scolaire", "chôm", "lit_hopitaux", "IVG")
fecondite2[, col_num2] <- lapply(fecondite2[, col_num2], function(x) as.numeric(gsub(",", ".", x)))

#CREATION ENSEMBLE DE DONNEES#
fecondite_long <- fecondite2 %>%
  select(Developpement, fecondité, all_of(col_num2)) %>%
  pivot_longer(cols = -c(Developpement, fecondité), names_to = "Variable", values_to = "Valeur")

# GRAPHIQUE #
resultat_graphique <- ggplot(fecondite_long, aes(x = Valeur, y = fecondité, color = Developpement)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_color_manual(values = vecteur_de_couleur) +
  facet_wrap(~ Variable, scales = "free", ncol = 1) +
  labs(title = "Régression linéaire de STRESS en fonction du développement",
       x = "Valeur de la variable",
       y = "fécondité") +
  theme_classic()
resultats_graphiques <- list()

#CREATION DE FONCTION#
for (var in col_num2) {
  plot_name <- paste("Régression linéaire de fécondité en fonction de", var)
  plot_data <- ggplot(fecondite_long, aes(x = Valeur, y = fecondité, color = Developpement)) +
    geom_point(data = filter(fecondite_long, Variable == var)) +
    geom_smooth(data = filter(fecondite_long, Variable == var), method = "lm") +
    scale_color_manual(values = vecteur_de_couleur) +
    labs(title = plot_name, x = var, y = "fécondité") +
    theme_classic()
    print(plot_data)
}

#AFFICHAGE GRAPHIQUE#
grid.arrange(grobs = resultats_graphiques, ncol = 3)



#QUALI-QUALI#
count(fecondite,Continent,Developpement)|>
  ggplot() +
  aes(x = Continent, y = Developpement, fill = n) +
  geom_tile(stat = "identity") +
  theme_light()

#QUALI-QUANTI#
#SELON LE CONTINENT#
fecondite_court <- fecondite |>
  pivot_longer(
    cols = 3:12,
    names_to = "mesure",
    values_to = "valeur"
  )

for (var in col_num2) {
  plot_name <- paste("Boxplot de", var , "en fonction des continents")
  plot_data <- ggplot(fecondite_court, aes(x = Continent, y = valeur, color = Continent)) +
    geom_boxplot(data = filter(fecondite_court, mesure == var)) +
    geom_jitter(alpha = 0.3) +
    scale_color_manual(values = vecteur_de_couleur) +
    labs(title = plot_name, x = "Continents", y = var) +
    theme_classic() +
    theme(legend.position = "none") +
    facet_wrap(~ mesure, scales = "free_y")
  
  print(plot_data)
}


for (var in col_num2) {
  plot_name <- paste("Régression linéaire de fécondité en fonction de", var)
  plot_data <- ggplot(fecondite_long, aes(x = valeur, y = fecondité, color = Developpement)) +
    geom_point(data = filter(fecondite_long, mesure == var)) +
    geom_smooth(data = filter(fecondite_long, mesure == var), method = "lm") +
    scale_color_manual(values = vecteur_de_couleur) +
    labs(title = plot_name, x = var, y = "fécondité") +
    theme_classic()
  
  # Afficher chaque graphique séparément
  print(plot_data)
}




#SELON LE DEVELOPPEMENT#
fecondite |>
  pivot_longer(
    cols = 3:12,
    names_to = "mesure",
    values_to = "valeur"
  ) |>
  ggplot() +
  aes(y = valeur, x = Developpement, color = Developpement) +
  geom_boxplot() +
  geom_jitter(alpha = 0.3)  +
  scale_color_manual(values = vecteur_de_couleur) +
  facet_wrap(~ mesure, scales = "free_y") +
  theme_bw()
#SELON LE BOXPLOT LE PAYS EST PAS DEVELOPPE CAR PROCHE DE 0#

#TRAITEMENT DES VALEURS MANQUANTE#
#RETROUVER LES LIGNES#
indice2 <- which(is.na(fecondite), arr.ind = TRUE)
print(indice2)
indice2 <- indice2[,1]
fecondite[indice2,]

#METHODES DES MOYENNES#
#LETONNIE(62)#
#VALEUR MOYENNE PAR CONTINENET ET DEVELOPPEMENT#
moyenne_fecondite_europe <- fecondite |>
  filter(Continent == "Europe" & Developpement == "forte") |>
  select(3:12) |>
  summarise(
    across(
      everything(),
      ~ mean(.x, na.rm = TRUE) |>
        round()
    ) )
view(moyenne_fecondite_europe)
#ON VOIT QUE FECONDITE = 2 POUR UN PAYS EUROPEEN ET DEVELOPPER ON REMPLACE DONC LA VALEUR MANQUANTE DE LA LETTONIE PAR 2#
#MOYENNE FECONDITE(3) POUR LA LETTONIE#
fecondite[62, 3] <- 2

#TANZANIE(102)#
#VALEUR MOYENNE PAR CONTINENET ET DEVELOPPEMENT#
moyenne_fecondite_afrique <- fecondite |>
  filter(Continent == "Afrique" & Developpement == "faible") |>
  select(3:12) |>
  summarise(
    across(
      everything(),
      ~ mean(.x, na.rm = TRUE) |>
        round()
    ) )
#ON VOIT QUE TAUX DE CHÔMAGE = 9 POUR UN PAYS AFRICAIN ET NON DEVELOPPER ON REMPLACE DONC LA VALEUR MANQUANTE DE LA TANZANIE PAR 9#
#MOYENNE TAUX DE CHOMAGE(9) POUR LA LETTONIE#
fecondite[102, 9] <- 9

#CAMBODGE(21)#
#VALEUR MOYENNE PAR CONTINENET ET DEVELOPPEMENT#
moyenne_fecondite_asie <- fecondite |>
  filter(Continent == "Asie" & Developpement == "faible") |>
  select(3:12) |>
  summarise(
    across(
      everything(),
      ~ mean(.x, na.rm = TRUE) |>
        round()
    ) )
#ON VOIT QUE TAUX DE CHÔMAGE = 9 POUR UN PAYS AFRICAIN ET NON DEVELOPPER ON REMPLACE DONC LA VALEUR MANQUANTE DE LA TANZANIE PAR 9#
#MOYENNE TAUX DE CHOMAGE(9) POUR LA CAMB#
fecondite[21, 6] <- 21
fecondite[21, 8] <- 7


#REGRESSION LOGISTIQUE#
#NOUS CHERCHONS DONC LE NIVEAU DE DEVELOPPEMENT DU PAYS#
#BINARISATION#
fecondite <- fecondite |> 
  mutate(Dvp = case_when(
    Developpement == "faible" ~ 0,
    Developpement == "forte" ~ 1
  ))
#CREATION DES JDD D'ENTRAINEMENT ET DE TEST#
#ENTRAINEMENT#
Developpement_entrainement <- fecondite |>
  filter(!is.na(Dvp)) |>
  slice_sample(prop = 0.8)

#VERIFICATION#
Developpement_verification <- anti_join(
  fecondite,
  Developpement_entrainement
) |>
  filter(!is.na(Dvp))

#REGRESSION LOGISTIQUE#
regression_logistique <- glm(
  Dvp ~ `IVG(0 à 6)` +`Nombre de lit d'hopitaux pour 1000 habitant`
  +`Pourcentage de femme dans la population active` +`Taux de chômage` +`Taux d'enfants non scolarisé` 
  +`Consommation (en $PPA)` +`Mortalité infantile` +`Taux d'irrélegieux` +`PIB/habitant`,
  data = Developpement_entrainement,
  family = binomial
)
car::Anova(regression_logistique)
#NON SIGNIFCATIF ENLEVER#
regression_logistique2 <- glm(
  Dvp ~ `IVG(0 à 6)`
  +`Taux de chômage` +`Consommation (en $PPA)` +`Mortalité infantile`,
  data = Developpement_entrainement,
  family = binomial
)
car::Anova(regression_logistique2)

#NON SIGNIFICATIF ENLEVER#
regression_logistique3 <- glm(
  Dvp ~ `IVG(0 à 6)`
  +`Consommation (en $PPA)` +`Mortalité infantile`,
  data = Developpement_entrainement,
  family = binomial
)
car::Anova(regression_logistique3)

#REPRESENTATION GRAPHIQUE#
plot(regression_logistique3)

# Automatiser le processus de visualisation avec une pause de 2 secondes entre chaque tracé
for (i in 1:num_plots) {
  plot(regression_logistique3)
  Sys.sleep(2)  # Pause de 2 secondes
}

#AU VU DES GRAPHIQUES LE NIVEAU DE DVP EGALE A 0#
view(Developpement_verification)
#TEST SUR LES DONNEES#
Developpement_verification <- Developpement_verification |> 
  mutate(
    prediction = predict(regression_logistique3, newdata = Developpement_verification)
  ) |> 
  mutate(
    Dvp_predit = case_when(
      prediction < 0 ~ "faible",
      TRUE ~ "forte"
    )
  )

#MATRICE DE CONFUSION#
matrice_de_confusion <- Developpement_verification |> 
  count( Developpement, Dvp_predit)

VP <- filter(matrice_de_confusion,Developpement == "faible" & Dvp_predit == "faible")$n
FN <- filter(matrice_de_confusion,Developpement == "faible" & Dvp_predit == "forte")$n
VN <- filter(matrice_de_confusion,Developpement == "forte" & Dvp_predit == "forte")$n
FP <- filter(matrice_de_confusion,Developpement == "forte" & Dvp_predit == "faible")$n

#CALCUL DE SENSIBILITE#
"Sensi" <- VP / (VP+FN)
Sensi

#CALCUL DE SPECIFICITE#
"Speci" <- VN / (VN+FP)
Speci

#REGRESSION FINNALE#
regression_logistique_final <- glm(
  Dvp ~ `IVG(0 à 6)`
  +`Consommation (en $PPA)` +`Mortalité infantile`,
  data = Developpement_entrainement,
  family = binomial
)

fecondite <- bind_rows( 
  fecondite |> 
    filter(is.na(Developpement)) |> 
    mutate(
      prediction = predict(regression_logistique_final, newdata = filter(fecondite, is.na(Developpement))) 
    ) |> 
    mutate(
      Developpement = case_when(
        prediction < 0 ~ "faible",
        TRUE ~ "forte"
      )
    )|> 
    select(- prediction), 
  fecondite |> filter(!is.na(Developpement)) 
) |> 
  select(- Dvp)

view(fecondite)

#ACP#
fecondite3 <- fecondite2[,-1] |> 
  drop_na()

fecondite3 |> 
  count(Continent, Developpement)

#CERCLE DE CORREATION#
acp <- fecondite3 |> 
  select( - Continent, - Developpement) |> 
  PCA()

#VALEUR PROPRES ET HISTOGRAMME#
acp$eig
factoextra::fviz_screeplot(acp)

#CONTRIBUTION DES VARIABLES INDEPEDANTE AU VARIABLES LATENTE#
dimdesc(acp)

#VISUALISARION DE NOTRE VARIABLES Y DANS LE CERLE DE CORRELATION#
acp_var_suppl <- PCA(
  X = fecondite3,
  quali.sup = c(1, 12),
  quanti.sup = 3
)


#VISUALISATION NUAGE DE POINT EN FONCTION DE PLEIN TRUC#
plot.PCA(acp_var_suppl, choix = "ind", label = "quali")
plot.PCA(acp_var_suppl, choix = "ind", habillage = "Continent", label = "none")
plot.PCA(acp_var_suppl, choix = "ind", habillage = "Developpement", label = "none")
plotellipses(acp_var_suppl, label = "none")


#MISSMDA#
fecondite_MDA <- MIPCA(
  fecondite[,c(-1,-14)] |>
    select(- Continent, - Developpement, ncp=2)
)
#VISUALISATION CERCLE DE CORRELATION#
plot.MIPCA(fecondite_MDA)
fecondite_MDA$res.imputePCA

fecondite_MDA <- bind_cols(
  fecondite[,c(-1,-14)] |> 
    select( Continent, Developpement), fecondite_MDA[["res.imputePCA"]]
)
fecondite_MDA


##
create_boxplot_plot <- function(data, x_var, fill_var) {
  ggplot(data) +
    aes(x = !!sym(x_var), fill = !!sym(fill_var)) +
    geom_boxplot(alpha = 0.7, position = "identity") +
    scale_fill_manual(values = vecteur_de_couleur) +
    labs(title = paste("Histogramme de", x_var, "par", fill_var),
         x = x_var,
         y = "Nombre de pays") +
    theme_minimal() +
    theme(legend.position = "top")
}


ggplot(fecondite, aes(x = Continent, y = fecondite, fill = Continent)) +
  geom_boxplot(alpha = 0.7, position = "identity") +
  scale_fill_manual(values = vecteur_de_couleur) +
  labs(title = "Boxplot de la fécondité par continent",
       x = "Continent",
       y = "Fécondité") +
  theme_minimal() +
  theme(legend.position = "top")

vecteur_de_couleur2 <- c("Europe" = "blue", "Afrique" = "green", "Asie" = "red", "Océanie" = "darkgoldenrod", 
                        "Amérique du Nord" = "orange", "Amérique Latine" = "pink")
fecondite$Continent <- factor(fecondite$Continent, levels = names(vecteur_de_couleur2))
boxplot(fecondite$`pib/hab` ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")



boxplot(fecondite$fecondité ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")
boxplot(fecondite$irréligieux ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")
boxplot(fecondite$mort_infan ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")
boxplot(fecondite$conso ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")
boxplot(fecondite$n_scolaire ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")
boxplot(fecondite$chôm ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")
boxplot(fecondite$lit_hopitaux ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")
boxplot(fecondite$IVG ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")
boxplot(fecondite$`%femmes` ~ fecondite$Continent, col = vecteur_de_couleur, main = "Boxplot du PIB par habitant par continent", xlab = "Continent", ylab = "PIB/hab")

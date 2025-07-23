#Appel API GOT
#Library
library(httr)
library(jsonlite)
library(sf)
library(ggplot2)
library(geojsonio)

#importation de des données cartographique à l'aide d'une requête API
url <- "https://raw.githubusercontent.com/jeffreylancaster/game-of-thrones/master/data/lands-of-ice-and-fire.json"
response <- GET(url)

if (status_code(response) == 200) {
  content_text <- content(response, as = "text", encoding = "UTF-8")
  data_carte <- fromJSON(content_text, flatten = TRUE)
  print(names(data_carte))
} else {
  cat("Erreur lors du téléchargement du fichier :", status_code(response))
}
#Importation du fonds de carte carte 
fonds_carte <- topojson_read(url)
remove(content_text ,url, response)

#mise en place d'un CRS pour le fonds de carte
crs_fictif <- st_crs(4326)
fonds_carte <- st_set_crs(fonds_carte, crs_fictif)

#Visualisation du fonds de carte
ggplot() +
  geom_sf(data = fonds_carte) +
  theme_minimal() +
  ggtitle("Carte des données 'Lands of Ice and Fire'")

#traitemment des données géographique
#Place
points_data_places <- as.data.frame(data_carte$objects$places)

#mise en place des données latitude / longitude
points_data_places$longitude <- sapply(points_data_places$geometries.coordinatesGeo, function(x) x[1])
points_data_places$latitude <- sapply(points_data_places$geometries.coordinatesGeo, function(x) x[2])
points_data_places_clean <- points_data_places %>%
  select(-c('geometries.coordinatesGeo','geometries.coordinates'))

#Converstion de la data en format sf
points_sf <- st_as_sf(points_data_places_clean, coords = c("longitude", "latitude"), crs = 4326)

#Ajout de colonnes qui compléter notre visualisation
region_mapping <- data.frame(
  name = {c("Oldtown", "Starfall", "Sunspear", "Highgarden", "Summerhall", "Storm's End", 
           "King's Landing", "Lannisport", "Stoney Sept", "Dragonstone", "Harrenhal", "Pyke", 
           "Riverrun", "The Eyrie", "Gulltown", "The Twins", "White Harbor", "Barrowton", 
           "Torrhen's Square", "Winterfell", "Deepwood Motte", "The Dreadfort", "Karhold", 
           "Castle Black", "Eastwatch", "Hardhome",  
           # Free Cities
           "Lys", "Tyrosh", "Myr", "Pentos", "Braavos", "Lorath", "Norvos", "Qohor", "Volantis",
           # Valyria and surroundings
           "Valyria", "Mantarys",  
           # Slaver's Bay
           "Meereen", "Yunkai", "Astapor", "Old Ghis", "New Ghis",  
           # Inner Essos
           "Vaes Dothrak", "Kayakayanaya", "Samyriana", "Bayasabahd", "Vaes Tolorro",  
           # Qarth and surroundings
           "Qarth", "Faros",  
           # Summer Isles and beyond
           "Yin", "Asshai", "Port of Ibben", "Lotus Port", "Tall Trees Town",  
           # Other locations
           "Moat Cailin", "Last Hearth",  
           # Missing locations
           "Ghoyan Drohe", "Chroyane")},
  continent = {c(
    # Westeros
    rep("Westeros", 26),  
    # Free Cities
    rep("Essos", 9),  
    # Valyria and surroundings
    rep("Essos", 2),  
    # Slaver's Bay
    rep("Essos", 5),  
    # Inner Essos
    rep("Essos", 5),  
    # Qarth and surroundings
    rep("Essos", 2),  
    # Summer Isles and beyond
    rep("Summer Isles", 5),  
    # Other locations in Westeros
    rep("Westeros", 2),  
    # Missing locations
    rep("Essos", 2)
  )},
  region_specific = {c(
    # Westeros
    "The Reach", "The Reach", "Dorne", "The Reach", "The Stormlands", "The Stormlands", 
    "The Crownlands", "The Westerlands", "The Riverlands", "The Crownlands", "The Riverlands", 
    "The Iron Islands", "The Riverlands", "The Vale", "The Vale", "The Riverlands", 
    "The North", "The North", "The North", "The North", "The North", "The North", "The North", 
    "Beyond the Wall", "Beyond the Wall", "Beyond the Wall",
    # Free Cities
    "Free Cities", "Free Cities", "Free Cities", "Free Cities", "Free Cities", 
    "Free Cities", "Free Cities", "Free Cities", "Free Cities",
    # Valyria and its ruins
    "Valyria", "Valyria",
    # Slaver's Bay
    "Slaver's Bay", "Slaver's Bay", "Slaver's Bay", "Slaver's Bay", "Slaver's Bay",
    # Inner Essos
    "Inner Essos", "Inner Essos", "Inner Essos", "Inner Essos", "Inner Essos",
    # Qarth and surroundings
    "Qarth", "Qarth",
    # Summer Isles
    "Summer Isles", "Summer Isles", "Summer Isles", "Summer Isles", "Summer Isles",
    # Other locations in Westeros
    "The North", "The North",
    # Newly added missing locations
    "Rhoyne Region", "Rhoyne Region"
  )}
)

#Ajout de détail
points_sf <- points_sf %>%
  left_join(region_mapping, by = c("geometries.properties.name" = "name"))

#Ajout d'une colonne couleur pour la visualisation
points_sf <- points_sf %>%
  mutate(color = case_when(
    geometries.properties.type == "castle" ~ "#8B0000",  
    geometries.properties.type == "town" ~ "#000080",   
    geometries.properties.type == "city" ~ "#006400",   
    geometries.properties.type == "ruin" ~ "#CD853F",  
    TRUE ~ "#000000"
  ))
head(points_sf,5)
#Visualisation
leaflet() %>%
  # Ajouter le fond de carte (polygones)
  addPolygons(data = fonds_carte, 
              color = "darkgrey",  
              fillColor = "lightblue", 
              weight = 1) %>%
  
  # Ajouter des marqueurs pour les points d'intérêt
  addCircleMarkers(data = points_sf, 
                   lng = ~st_coordinates(geometry)[,1], 
                   lat = ~st_coordinates(geometry)[,2], 
                   color = ~color, 
                   popup = ~paste("<b>Nom:</b>", geometries.properties.name, 
                                  "<br><b>Type:</b>", geometries.properties.type,
                                  "<br><b>Continent:</b>",continent,
                                  "<br><b>Région:</b>",region_specific),
                   radius = 6,
                   stroke = TRUE, 
                   weight = 1,  
                   opacity = 1) %>%

  # Ajuster la carte pour afficher les bornes des points
  fitBounds(lng1 = -45.57871, lat1 = -2.21711, lng2 = 109.9649, lat2 = 62.3253) %>%
  # Limiter la carte à la zone délimitée par les points
  setMaxBounds(lng1 = min_lon*1.25, lat1 = min_lat*1.25, lng2 = max_lon*1.15, lat2 = max_lat*1.15) %>%
  # Ajouter une légende pour décrire les couleurs
  addLegend(position = "bottomright", 
            colors = c("#8B0000", "#000080", "#006400", "#CD853F"), 
            labels = c("Château", "Ville", "Bourg", "Ruine"), 
            title = "Type de lieu", 
            opacity = 1)%>%
  
  addLegend(position = "topright", 
            colors = c(NA), 
            labels = "", 
            title = "Carte des points d'intérêt de Game of Thrones", 
            opacity = 0,
  ) %>%
  
  # Ajouter un fond blanc
  htmlwidgets::onRender("function(el, x) { 
      var map = this;
      map.getContainer().style.backgroundColor = 'white';  // Ajoute un fond blanc
  }")

colnames(points_sf)



#Visualisation par histogramme des monuments par continents 
library(shiny)
library(plotly)
library(dplyr)


# Regrouper les données pour compter les types par continent
df_count <- points_sf %>%
  group_by(continent, geometries.properties.type) %>%
  summarise(count = n(), .groups = 'drop')

points_sf$geometries.properties.type
# Créer un graphique de base avec ggplot2
p <- ggplot(df_count, aes(x = type, y = count, fill = continent)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  theme_minimal() +
  labs(title = "Nombre de monuments par type et par continent",
       x = "Type de Monument", y = "Nombre de monuments") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Convertir en graphique interactif avec plotly
p_interactif <- ggplotly(p) %>%
  layout(
    title = "Nombre de monuments par type et continent",
    barmode = "dodge",
    xaxis = list(title = "Type de Monument"),
    yaxis = list(title = "Nombre de monuments")
  )

# Afficher
p_interactif

str(points_sf$geometries.properties.type)
#importation de des données des personnages pour les différents graphique à l'aide d'une requête API
url <- "https://raw.githubusercontent.com/jeffreylancaster/game-of-thrones/master/data/characters.json"
response <- GET(url)

if (status_code(response) == 200) {
  content_text <- content(response, as = "text", encoding = "UTF-8")
  data_personnage <- fromJSON(content_text, flatten = TRUE)
  print(names(data_personnage))
} else {
  cat("Erreur lors du téléchargement du fichier :", status_code(response))
}

data_personnage <- as.data.frame(data_personnage)
remove(content_text ,url, response)
#Filtre de famille importante
noms_nobles <- unique(unlist(data_personnage$characters.houseName))
#Vérification
data_personnage_house <- data_personnage[, c("characters.characterName", "characters.houseName", "characters.servedBy", "characters.marriedEngaged","characters.guardedBy")] %>%
  mutate(across(everything(), ~sapply(., function(x) {
    if (is.null(x)) {
      return(NA)
    } else {
      return(paste(x, collapse = ", "))
    }
  })))%>%
  mutate(characters.houseName = ifelse(is.na(characters.houseName) & 
                                         word(characters.characterName, 2) %in% noms_nobles,
                                       word(characters.characterName, 2),
                                       characters.houseName))%>%
  filter(!grepl("young", characters.characterName, ignore.case = TRUE)) %>% 
  filter(!grepl("Young", characters.characterName, ignore.case = TRUE))%>%
  filter(
    !is.na(characters.houseName) | 
      !is.na(characters.servedBy) | 
      !is.na(characters.guardedBy) |
      !is.na(characters.marriedEngaged)  
  )

#Filtrage des donnée pour ne garder aucun NA
data_personnage_house_unique <- data_personnage_house %>%
  separate_rows(characters.houseName, sep = ", ")
data_personnage_house_clean <- data_personnage_house_unique %>%
  filter(!is.na(characters.houseName))

#Visualisation graphique du nombre de personnage par maison
ggplot(data_personnage_house_clean, aes(x = reorder(characters.houseName, characters.houseName, FUN = function(x) length(x)))) +
  geom_bar(fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Répartition des personnages par maison", 
       x = "Maison", y = "Nombre de personnages")

#Création des graphiques relationnels
#Génération des couleurs pour chaque maison
couleurs_maisons <- c(
  "Targaryen" = "#D32F2F",  
  "Greyjoy"   = "#37474F",  
  "Lannister" = "#FFB300",  
  "Stark"     = "#1976D2",  
  "Baratheon" = "#388E3C",  
  "Frey"      = "#8E24AA",  
  "Tully"     = "#0288D1", 
  "Martell"   = "#FF7043",  
  "Mormont"   = "#2C6B2F",  
  "Tyrell"    = "#76FF03", 
  "Arryn"     = "#0277BD",  
  "Umber"     = "#3E2723",  
  "Bolton"    = "#9C27B0",  
  "Tarly"     = "#E65100",
  "Sans_titre" = "#D3D3D3"
)

# Servitude
servitude <- data_personnage_house %>%
  separate_rows(characters.houseName, sep = ", ") %>%
  separate_rows(characters.servedBy, sep = ", ") %>%
  filter(!is.na(characters.servedBy)) %>%
  select(character = characters.characterName, servitude = characters.servedBy, house = characters.houseName)

edges_servitude <- servitude
nodes_servitude <- unique(c(edges_servitude$character, edges_servitude$servitude))

#Mariage
mariage <- data_personnage_house %>%
  separate_rows(characters.houseName, sep = ", ") %>%
  filter(!is.na(characters.marriedEngaged)) %>%
  separate_rows(characters.marriedEngaged, sep = ", ") %>%
  select(character = characters.characterName, marriage = characters.marriedEngaged, house = characters.houseName)
edges_mariage <- mariage
nodes_mariage <- unique(c(edges_mariage$character, edges_mariage$marriage))

#Maison
maison <- data_personnage_house %>%
  filter(!is.na(characters.houseName)) %>%
  separate_rows(characters.houseName, sep = ", ") %>%
  select(character = characters.characterName, house = characters.houseName)
edges_maison <- maison
nodes_maison <- unique(c(edges_maison$character, edges_maison$house))


# Gardien
guardien <- data_personnage_house %>%
  separate_rows(characters.houseName, sep = ", ") %>%
  separate_rows(characters.guardedBy, sep = ", ") %>%
  filter(!is.na(characters.guardedBy)) %>%
  select(character = characters.characterName, Gardien = characters.guardedBy, house = characters.houseName)

edges_guardien <- guardien
nodes_guardien <- unique(c(edges_guardien$character, edges_guardien$Gardien))

# Union des graphes : Gardien et Servitude
edges_total <- bind_rows(
  data.frame(from = edges_servitude$character, to = edges_servitude$servitude, type = "servitude"),
  data.frame(from = edges_guardien$character, to = edges_guardien$Gardien, type = "Gardien"),
  data.frame(from = edges_maison$character, to = edges_maison$house, type = "house"),
  data.frame(from = edges_mariage$character, to = edges_mariage$marriage, type = "mariage")
)
edges_total$type
nodes_total <- unique(c(nodes_mariage, nodes_maison, nodes_servitude, nodes_guardien))

# Création des nœuds avec les couleurs
nodes <- data.frame(id = nodes_total,
                    label = nodes_total,
                    color = ifelse(nodes_total %in% maison$house,
                                   couleurs_maisons[nodes_total], 
                                   couleurs_maisons[maison$house[match(nodes_total, maison$character)]]))
nodes$color
# Application des couleurs sur les arêtes : doré pour gardien, gris foncé pour servitude
edges <- data.frame(from = edges_total$from,
                    to = edges_total$to,
                    color = ifelse(edges_total$type == "mariage", "#DC143C", 
                                   ifelse(edges_total$type == "house", "royalblue", 
                                          ifelse(edges_total$type == "Gardien", "gold", "darkgray"))),
                    stringsAsFactors = FALSE)
edges$color

# Visualisation interactive avec plusieurs types de liens
visNetwork(nodes, edges,background = "#FFF")%>%
  visNodes(size = 20,
           font = list(size = 16, color = "black")) %>% 
  visEdges(color = edges$color, 
           width = 2,   
           arrows = "from") %>% 
  visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE),
             nodesIdSelection = list(enabled = TRUE, 
                                     values = nodes$id[nodes$id %in% maison$house])) %>% 
  visLayout(randomSeed = 123, improvedLayout = TRUE) %>% 
  visLegend(main = "Relations des personnages", 
            useGroups = FALSE, 
            width = 0.2) %>% 
  visPhysics(stabilization = TRUE, 
             solver = "forceAtlas2Based") %>% 
  visLegend(main = "Relations des personnages", 
            useGroups = FALSE, 
            width = 0.2, 
            addEdges = data.frame(
              label = c("Marié ou engagé à un personnage", 
                        "Serf ou allégeance", 
                        "Fait partie de la famille ou de la maison", 
                        "Protégé par un gardien"), 
              color = c("#DC143C", "darkgray", "gold", "royalblue"),
              font = list(size = 50)
            ))%>%
  visExport(type = "png") 


nodes_maison <- nodes[nodes$id %in% maison$house, ]

#### FIN DU PROGRAMME ####

getwd()
save.image("Cours.RData")




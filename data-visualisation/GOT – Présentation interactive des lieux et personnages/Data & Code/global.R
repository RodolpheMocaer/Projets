#Library
library(shiny)
library(httr)
library(readr)
library(data.table)
library(sf)
library(jsonlite)
library(ggplot2)
library(geojsonio)
library(dplyr)
library(ggrepel)
library(plotly)
library(leaflet)
library(DT)
library(tidyr)
library(igraph)
library(stringr)
library(visNetwork)
library(htmlwidgets)


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

#mise en place des données latitude / longitude
points_data_places <- as.data.frame(data_carte$objects$places)
points_data_places$longitude <- sapply(points_data_places$geometries.coordinatesGeo, function(x) x[1])
points_data_places$latitude <- sapply(points_data_places$geometries.coordinatesGeo, function(x) x[2])
points_data_places_clean <- points_data_places %>%
  select(-c('geometries.coordinatesGeo','geometries.coordinates'))

#Importation d'une nouvelle donnée
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

#Gestion de cette donnée 
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


load(file = "/Users/rododo/Desktop/ECAP/Master 2/GOT_shiny/Cours.RData")
`%ni%` <- Negate(`%in%`)

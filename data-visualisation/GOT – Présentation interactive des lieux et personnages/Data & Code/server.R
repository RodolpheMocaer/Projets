#Library
library(shiny)

#Application
shinyServer(function(input, output) {
  # Filtrer les points d'intérêt en fonction du continent sélectionné
  #Visualisation
  output$map <- renderLeaflet({
    #Ajout de variable utilise à la visualisation de la carte
    region_mapping <- data.frame(
      name = {c("Oldtown", "Starfall", "Sunspear", "Highgarden", "Summerhall", "Storm's End", 
                "King's Landing", "Lannisport", "Stoney Sept", "Dragonstone", "Harrenhal", "Pyke", 
                "Riverrun", "The Eyrie", "Gulltown", "The Twins", "White Harbor", "Barrowton", 
                "Torrhen's Square", "Winterfell", "Deepwood Motte", "The Dreadfort", "Karhold", 
                "Castle Black", "Eastwatch", "Hardhome",  
                "Lys", "Tyrosh", "Myr", "Pentos", "Braavos", "Lorath", "Norvos", "Qohor", "Volantis",
                "Valyria", "Mantarys",  
                "Meereen", "Yunkai", "Astapor", "Old Ghis", "New Ghis",  
                "Vaes Dothrak", "Kayakayanaya", "Samyriana", "Bayasabahd", "Vaes Tolorro",  
                "Qarth", "Faros",  
                "Yin", "Asshai", "Port of Ibben", "Lotus Port", "Tall Trees Town",  
                "Moat Cailin", "Last Hearth",  
                "Ghoyan Drohe", "Chroyane")},
      continent = {c(
        rep("Westeros", 26),  
        rep("Essos", 9),  
        rep("Essos", 2),  
        rep("Essos", 5),  
        rep("Essos", 5),  
        rep("Essos", 2),  
        rep("Summer Isles", 5),  
        rep("Westeros", 2),  
        rep("Essos", 2)
      )},
      region_specific = {c(
        "The Reach", "The Reach", "Dorne", "The Reach", "The Stormlands", "The Stormlands", 
        "The Crownlands", "The Westerlands", "The Riverlands", "The Crownlands", "The Riverlands", 
        "The Iron Islands", "The Riverlands", "The Vale", "The Vale", "The Riverlands", 
        "The North", "The North", "The North", "The North", "The North", "The North", "The North", 
        "Beyond the Wall", "Beyond the Wall", "Beyond the Wall",
        "Free Cities", "Free Cities", "Free Cities", "Free Cities", "Free Cities", 
        "Free Cities", "Free Cities", "Free Cities", "Free Cities",
        "Valyria", "Valyria",
        "Slaver's Bay", "Slaver's Bay", "Slaver's Bay", "Slaver's Bay", "Slaver's Bay",
        "Inner Essos", "Inner Essos", "Inner Essos", "Inner Essos", "Inner Essos",
        "Qarth", "Qarth",
        "Summer Isles", "Summer Isles", "Summer Isles", "Summer Isles", "Summer Isles",
        "The North", "The North",
        "Rhoyne Region", "Rhoyne Region"
      )}
    )
    points_sf <- points_sf %>%
      left_join(region_mapping, by = c("geometries.properties.name" = "name"))

    #Ajout de couleur pour la visualisation de la carte
    points_sf <- points_sf %>%
      mutate(color = case_when(
        geometries.properties.type == "castle" ~ "#8B0000",  
        geometries.properties.type == "town" ~ "#000080",    
        geometries.properties.type == "city" ~ "#006400",    
        geometries.properties.type == "ruin" ~ "#CD853F",    
        TRUE ~ "#000000"  
      ))
    
    #Filtrage de la data en fonction du continent
    points_sf_continent <- points_sf %>%
      filter(if (is.null(input$continent) || input$continent == "Tous") TRUE else continent == input$continent)

    #Affichage des images en fonction du continent sélectionné
    output$continent_image <- renderUI({
      img_src <- switch(input$continent,
                        "Westeros" = "Westeros.png",
                        "Essos" = "Essos.png",
                        "Summer Isles" = "Summer.png",
                        "all.png")
      tags$img(src = img_src, width = "100%", height = "auto")
    })
    # Sélection du texte en fonction du continent choisi
    output$continent_text <- renderUI({
      continent_description <- switch(input$continent,
                                      "Westeros" = "Westeros est un continent aux climats variés, avec des hivers rigoureux dans le Nord et des étés plus doux au Sud. 
                                      La politique y est dominée par de grandes maisons nobles en quête de pouvoir.",
                                      "Essos" = "Essos est un continent chaud et diversifié, avec des déserts et des jungles. 
                                      Les cités et empires y rivalisent pour la domination, créant une dynamique politique complexe.",
                                      "Summer Isles" = "Les Summer Isles sont un archipel tropical, connu pour son climat chaud et sa paix, loin des conflits des autres continents.",
                                      "Veuillez sélectionner un continent pour voir les informations détaillées.")
      tags$p(continent_description, style = "font-size: 16px; color: black;") 
    })
    
    #Visualisation de la carte de GOT
    leaflet() %>%
      addPolygons(data = fonds_carte, 
                  color = "darkgrey", 
                  fillColor = "#4A90D9", 
                  weight = 1) %>%
      addCircleMarkers(data = points_sf_continent, 
                       lng = ~st_coordinates(geometry)[,1], 
                       lat = ~st_coordinates(geometry)[,2], 
                       color = ~color, 
                       popup = ~paste("<b>Nom:</b>", geometries.properties.name, 
                                      "<br><b>Type:</b>", geometries.properties.type,
                                      "<br><b>Continent:</b>", continent, 
                                      "<br><b>Région:</b>", region_specific), 
                       radius = 6, 
                       stroke = TRUE, 
                       weight = 1, 
                       opacity = 1) %>%
      fitBounds(lng1 = -45.57871, lat1 = -2.21711, lng2 = 109.9649, lat2 = 62.3253) %>%
      setMaxBounds(lng1 = (-45.57871)*1.25, lat1 = (-2.21711)*1.4, lng2 = 109.9649*1.15, lat2 = 62.3253*1.15) %>%
      addLegend(position = "bottomright", 
                colors = c("#8B0000", "#000080", "#006400", "#CD853F"), 
                labels = c("Château", "Ville", "Bourg", "Ruine"), 
                title = "Type de lieu", 
                opacity = 1) %>%
      addLegend(position = "topright", 
                colors = c(NA), 
                labels = "", 
                title = "Carte des points d'intérêt de Game of Thrones", 
                opacity = 0) %>%
      htmlwidgets::onRender("function(el, x) { 
          var map = this;
          map.getContainer().style.backgroundColor = '#FAFAFA';  // Ajoute un fond blanc
      }")
    
    #Visualisation de la table qui à permis la carte
  })
  output$Histogramme <- renderPlotly({
    region_mapping <- data.frame(
      name = {c("Oldtown", "Starfall", "Sunspear", "Highgarden", "Summerhall", "Storm's End", 
                "King's Landing", "Lannisport", "Stoney Sept", "Dragonstone", "Harrenhal", "Pyke", 
                "Riverrun", "The Eyrie", "Gulltown", "The Twins", "White Harbor", "Barrowton", 
                "Torrhen's Square", "Winterfell", "Deepwood Motte", "The Dreadfort", "Karhold", 
                "Castle Black", "Eastwatch", "Hardhome",  
                "Lys", "Tyrosh", "Myr", "Pentos", "Braavos", "Lorath", "Norvos", "Qohor", "Volantis",
                "Valyria", "Mantarys",  
                "Meereen", "Yunkai", "Astapor", "Old Ghis", "New Ghis",  
                "Vaes Dothrak", "Kayakayanaya", "Samyriana", "Bayasabahd", "Vaes Tolorro",  
                "Qarth", "Faros",  
                "Yin", "Asshai", "Port of Ibben", "Lotus Port", "Tall Trees Town",  
                "Moat Cailin", "Last Hearth",  
                "Ghoyan Drohe", "Chroyane")},
      continent = {c(
        rep("Westeros", 26),  
        rep("Essos", 9),  
        rep("Essos", 2),  
        rep("Essos", 5),  
        rep("Essos", 5),  
        rep("Essos", 2),  
        rep("Summer Isles", 5),  
        rep("Westeros", 2),  
        rep("Essos", 2)
      )},
      region_specific = {c(
        "The Reach", "The Reach", "Dorne", "The Reach", "The Stormlands", "The Stormlands", 
        "The Crownlands", "The Westerlands", "The Riverlands", "The Crownlands", "The Riverlands", 
        "The Iron Islands", "The Riverlands", "The Vale", "The Vale", "The Riverlands", 
        "The North", "The North", "The North", "The North", "The North", "The North", "The North", 
        "Beyond the Wall", "Beyond the Wall", "Beyond the Wall",
        "Free Cities", "Free Cities", "Free Cities", "Free Cities", "Free Cities", 
        "Free Cities", "Free Cities", "Free Cities", "Free Cities",
        "Valyria", "Valyria",
        "Slaver's Bay", "Slaver's Bay", "Slaver's Bay", "Slaver's Bay", "Slaver's Bay",
        "Inner Essos", "Inner Essos", "Inner Essos", "Inner Essos", "Inner Essos",
        "Qarth", "Qarth",
        "Summer Isles", "Summer Isles", "Summer Isles", "Summer Isles", "Summer Isles",
        "The North", "The North",
        "Rhoyne Region", "Rhoyne Region"
      )}
    )
    points_sf <- points_sf %>%
      left_join(region_mapping, by = c("geometries.properties.name" = "name"))
    
    #Filtrage de la data en fonction du lieu
    points_sf_lieu <- points_sf %>%
      filter(if (is.null(input$lieu) || input$lieu == "Tous") TRUE else geometries.properties.type == input$lieu)
    
    # Regrouper les données pour compter les types par continent
    df_count <- points_sf_lieu %>%
      group_by(continent, geometries.properties.type) %>%
      summarise(count = n(), .groups = 'drop')

    #Affichage des images en fonction du lieu sélectionné
    output$Type_lieu <- renderUI({
      img_src <- switch(input$lieu,
                        "ruin" = "Ruin.png",
                        "town" = "Town.png",
                        "castle" = "Castle.png",
                        "city" = "City.png",
                        "Throne.png")
      tags$img(src = img_src, width = "100%", height = "auto")})
  #Couleur en fonction du lieu 
    type_colors <- c(
      "ruin" = "#5A7F8B",    
      "town" = "#9B3D3D",   
      "castle" = "#A0A0A0",  
      "city" = "#D0C6B8"     
    )
  # Création du graphique
    p <- ggplot(df_count, aes(y = continent, x = count, fill = geometries.properties.type)) + 
      geom_bar(stat = "identity") + 
      scale_fill_manual(values = type_colors) + 
      theme_minimal() + 
      labs(title = "Nombre de monuments par type et par continent", 
           x = "Nombre de monuments", y = "Continent") + 
      theme(axis.text.y = element_text(angle = 0, hjust = 1))  

  #Mise de l'intéractivité
    ggplotly(p) %>%
      layout(
        title = "Nombre de lieu par type et continent",
        barmode = "dodge",
        xaxis = list(title = "Nombre de lieu important"),
        yaxis = list(title = "Continent")
      )
  })
  output$tab_capa <- DT::renderDataTable({
    table_continent <- as.data.table(data_personnage_house)
    DT::datatable(data = table_continent, extensions = 'Buttons', options = list(rownames = FALSE))
  })
  output$network_plot <- renderVisNetwork({
    #Visualisation des relations
    lien_types <- input$lien_type

    #Couleur des maisons 
    house_colors <- c(
      "Targaryen" = "#9C1C27",  
      "Greyjoy"   = "#FFD700",  
      "Lannister" = "#D50032",  
      "Stark"     = "#4A4A4A",  
      "Baratheon" = "#006400",  
      "Frey"      = "#A4A4A4",  
      "Tully"     = "#1F61A4",  
      "Martell"   = "#FF8C00",  
      "Mormont"   = "#2F6A4E",  
      "Tyrell"    = "#8BC34A",  
      "Arryn"     = "#4A90E2",  
      "Umber"     = "#5C3A21",  
      "Bolton"    = "#A80000",  
      "Tarly"     = "#5A3D31"
    )
unique(data_personnage_house$characters.houseName)    
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
    
    edges_total <- reactive({
      bind_rows(
        data.frame(from = edges_servitude$character, to = edges_servitude$servitude, type = "servitude"),
        data.frame(from = edges_guardien$character, to = edges_guardien$Gardien, type = "gardien"),
        data.frame(from = edges_maison$character, to = edges_maison$house, type = "maison"),
        data.frame(from = edges_mariage$character, to = edges_mariage$marriage, type = "mariage")
      )
    })
    
    nodes_total <- unique(c(nodes_mariage, nodes_maison, nodes_servitude, nodes_guardien))
    
    # Création des nœuds avec les couleurs
    nodes <- data.frame(id = nodes_total,
                        label = nodes_total,
                        color = ifelse(nodes_total %in% maison$house,
                                       house_colors[nodes_total], 
                                       house_colors[maison$house[match(nodes_total, maison$character)]]),
                        stringsAsFactors = FALSE)
    
    # Données réactives pour filtrer les arêtes selon le choix de l'utilisateur
    edges_filtered <- reactive({
      edges_total() %>%
        filter(type %in% input$lien_type) %>%
        mutate(color = case_when(
          type == "mariage" ~ "#DC143C",
          type == "maison" ~ "royalblue",
          type == "gardien" ~ "gold",
          type == "servitude" ~ "darkgray"
        ))
    })
    
    
    
    # Créer le graphique visuel
    visNetwork(nodes, edges_filtered(), background = "#FFF")%>%
      visNodes(size = 20,
               font = list(size = 16, color = "black")) %>% 
      visEdges(color = edges_filtered()$color, 
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
                  label = c("Mariage", 
                            "Servitude", 
                            "Maison", 
                            "Gardien"),
                  color = c("#DC143C", "darkgray", "royalblue", "gold"))) %>%  
      visExport(type = "png")

})
  output$info_lien <- renderUI({
    lien_descriptions <- sapply(input$lien_type, function(lien) {
      switch(lien,
             "mariage" = "- Mariage : Marié ou engagé à un personnage au moins une fois dans la série",
             "servitude" = "- Servitude : Serf ou allégeance",
             "maison" = "- Maison : Fait partie de la famille ou de la maison",
             "gardien" = "- Gardien : Protégé par un gardien") 
    })
    
    lien_description <- paste(lien_descriptions, collapse = "<br>")
    tags$p(HTML(lien_description), style = "font-size: 16px; color: black;")
  })
})








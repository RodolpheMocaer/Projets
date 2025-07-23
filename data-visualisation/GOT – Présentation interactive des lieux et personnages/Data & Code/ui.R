#Library
library(shiny)

shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  
  tags$div(class = "title-panel", "Game of Thrones"),
  
  navbarPage("",
             navbarMenu("Univers de Game of throne",
                        tabPanel("Contexte",
                                 sidebarPanel(class = "sidebar",
                                              img(src = "Throne.png",
                                                  height = "auto",
                                                  width = "100%")),
                                 mainPanel(h2("L'univers de Game of Thrones"),
                                           tags$p("Game of Thrones est une saga épique, adaptée des romans A Song of Ice and Fire de George R.R. Martin,
                                               qui plonge les spectateurs dans un univers vaste et impitoyable mêlant intrigues politiques, guerres de
                                               pouvoir et forces surnaturelles. L’histoire se déroule principalement sur le continent de Westeros, un territoire
                                               divisé en plusieurs royaumes, où le Trône de Fer représente l’ultime symbole de domination. À l’est, le continent d’Essos
                                               regorge de cités marchandes, de cultures fascinantes et de territoires inexplorés, où se cachent de nombreux mystères du passé."
                                               ,style = "text-align: justify;"),
                                           tags$p("Parmi les lieux les plus emblématiques de Westeros, on retrouve Port-Réal, la capitale où se joue l’essentiel
                                               des jeux de pouvoir entre les grandes maisons nobles. Winterfell, le fief ancestral des Stark, incarne l’honneur
                                               et la rigueur du Nord, tandis que Le Mur, gigantesque fortification de glace, protège le royaume des dangers surnaturels
                                               venus du Grand Nord, notamment les terrifiants Marcheurs Blancs. À l’est, Essos abrite des cités mythiques comme Pentos et Meereen,
                                               où Daenerys Targaryen, la dernière héritière de sa lignée, rassemble une armée et élève ses dragons dans l’espoir de reconquérir 
                                               le Trône de Fer.",
                                                  style = "text-align: justify;"),
                                           tags$p("L’univers de Game of Thrones est peuplé de personnages complexes et inoubliables. Jon Snow, bâtard élevé à Winterfell, 
                                               embrasse son destin héroïque en rejoignant la Garde de Nuit, avant de découvrir ses véritables origines. Daenerys Targaryen, 
                                               survivante d’une dynastie déchue, traverse un parcours semé d’épreuves pour affirmer sa légitimité et sa puissance. Tyrion Lannister, 
                                               esprit vif et stratège hors pair, navigue entre intrigues et trahisons pour assurer sa survie dans un monde qui le sous-estime. 
                                               Arya Stark, animée par la vengeance, devient une redoutable combattante après avoir tout perdu. 
                                               À l’opposé, Cersei Lannister, ambitieuse et impitoyable, est prête à sacrifier quiconque se met en travers de son chemin pour 
                                               protéger son pouvoir et sa famille.",
                                                  style = "text-align: justify;"),
                                           tags$p("Mais au-delà des luttes de pouvoir entre les grandes familles, une menace bien plus grande plane sur Westeros. Les Marcheurs Blancs, 
                                               créatures ancestrales oubliées, émergent des terres glacées du Nord, annonçant une apocalypse imminente. Seuls les dragons, 
                                               symboles de puissance et de destruction, semblent capables de les affronter. Dans cet univers impitoyable où la loyauté est rare et où chaque décision 
                                               peut être fatale, le destin des Sept Royaumes se joue dans un équilibre fragile entre alliances, trahisons et batailles épiques.",
                                                  style = "text-align: justify;"))),
                        tabPanel("Donnée utlisée",
                                 h2("Sélection des données"),
                                       tags$p("Les données utilisées dans ce projet proviennent de requêtes API permettant d’extraire des informations géographiques et des données sur les personnages de l’univers de Game of Thrones. 
                                         Tout d'abord, les données cartographiques issues du fichier JSON 'Lands of Ice and Fire' ont été récupérées et traitées afin de visualiser les différentes régions et lieux emblématiques de Westeros et Essos.
                                         Une projection cartographique a été appliquée pour garantir une bonne représentation des données sur un fond de carte interactif, enrichi avec des marqueurs colorés différenciant les types de lieux (châteaux, villes, ruines, etc.).
                                         Par la suite, un second jeu de données a été importé pour analyser les personnages et leurs affiliations. Ces données ont permis d'explorer les relations entre les maisons, les alliances par mariage, les servitudes et les gardiens à travers des graphes interactifs. 
                                         Enfin, des visualisations sous forme d’histogrammes et de réseaux relationnels ont été réalisées afin d’illustrer la répartition des personnages par maison et leurs connexions. 
                                         L’ensemble de ces traitements et visualisations contribue à une meilleure compréhension des interactions et de la géographie dans l’univers de la saga.",
                                        tags$p(
                                          tags$a(href = "https://github.com/jeffreylancaster/game-of-thrones", "Voir les données sélectionnés")), 
                                        style = "text-align: justify;"))),
             navbarMenu("Lieux importants",
                        tabPanel("Cartographie des lieux importants",
                                 sidebarLayout(
                                   sidebarPanel(
                                     class = "sidebar",
                                     selectInput(
                                       inputId = "continent", 
                                       label = "Sélectionner un continent", 
                                       choices = c("Tous", "Westeros", "Essos", "Summer Isles"), 
                                       selected = "Tous", 
                                       multiple = FALSE),
                                     uiOutput("continent_image"),
                                     uiOutput("continent_text")),
                                   mainPanel(
                                     leafletOutput("map")))),
                        tabPanel("Répartitions des lieux importatnt",
                                 sidebarLayout(
                                   sidebarPanel(
                                     class = "sidebar",
                                     selectInput(
                                       inputId = "lieu", 
                                       label = "Sélectionner un type de lieu", 
                                       choices = c("Tous", "ruin", "town", "castle","city"), 
                                       selected = "Tous", 
                                       multiple = FALSE),
                                     uiOutput("Type_lieu")),
                                   mainPanel(
                                     plotlyOutput("Histogramme"))))),
             navbarMenu("Relationnel des personnages",
                        tabPanel("Visualisation des différentes relations",
                                 sidebarLayout(
                                   sidebarPanel(
                                     class = "sidebar",
                                     selectInput(inputId ="lien_type",
                                                 label = "Sélectionner un Type de lien", 
                                                 choices = c("mariage", "servitude", "gardien", "maison"), 
                                                 selected = "maison",
                                                 multiple = TRUE),
                                     hr(),
                                     h4("Informations sur les différents types de liens sélectionnés"),
                                     uiOutput("info_lien")
                                     ),
                                   mainPanel(visNetworkOutput("network_plot")))),
                        tabPanel("Table",
                                 div(dataTableOutput("tab_capa"), align = "")))),
  hr(),
  div(class = "footer",
      img(src = "logoiae.png", height = "30px"),
      p("Rodolphe Mocaër")
  ),
  hr()))

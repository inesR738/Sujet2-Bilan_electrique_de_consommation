#Bilan de consommation electrique

#Construction d'un shiny qui permet de visualiser le bilan électrique 
#(consommation et production locale) à une maille locale au pas annuelle 
#et de comparer plusieurs collectivités.
#Les mailles serront être des régions, des départements ou des communes.


# librairies
library(dplyr)
library(tidyverse)
library(shinydashboard)
library(shiny)
library(ggplot2)
library(plotly)
library(DT)

# Collecte des donnees

setwd("../Sujet2-Bilan_electrique_de_consommation")

#Donnees sur les régions
consommation_region <- read.csv("consommation-electrique-par-secteur-dactivite-region.csv", sep=";", fileEncoding = "UTF-8")
production_region <- read.csv("production-electrique-par-filiere-a-la-maille-region.csv", sep=";", fileEncoding = "UTF-8")

#Donnees sur les departements
consommation_departement <- read.csv("consommation-electrique-par-secteur-dactivite-departement.csv", sep=";", fileEncoding = "UTF-8")
production_departement <- read.csv("production-electrique-par-filiere-a-la-maille-departement.csv", sep=";", fileEncoding = "UTF-8")

#Donnees sur les communes
consommation_commune <- read.csv("consommation-electrique-par-secteur-dactivite-commune.csv", sep=";", fileEncoding = "UTF-8")
production_commune <- read.csv("production-electrique-par-filiere-a-la-maille-commune.csv", sep=";", fileEncoding = "UTF-8")


ui <- dashboardPage(
  # Titre de l'application
  dashboardHeader(
    title="Bilan electrique de consommation",
    titleWidth=1000
  ),
  
  dashboardSidebar(
    
    #Nos deux onglets : Resume et Details
    sidebarMenu(
      menuItem("Resume", tabName = "Resume"),
      menuItem("Details", tabName = "Details")
    ),
    
    #Inputs :
    # Choix de la maille d'interet
    selectInput('Maille',
                'Choisissez la collectivite :',
                choices = c('Region', 'Departement', 'Commune', 'Moyenne France')),
    
    # Choix de l'annee d'interet 
    selectInput("annee",
                "Choisissez votre/vos annee(s) :",
                choices = sort(unique(consommation_commune$Année)),
                multiple = TRUE),
    
    
    #ui
    uiOutput('ui_moyenne'),
    
    uiOutput('ui_region'),
    
    uiOutput('ui_departement'),
    
    uiOutput('ui_commune'),
    
    
    #actionButton 
    actionButton(inputId = 'action','Clickez ici')
  ),
  
  dashboardBody(
    tabItems(
      # Premier onglet : Resume
      tabItem('Resume',
              
              h5(textOutput('nom_commune'), 
                 dataTableOutput('table',width = "60%"))
              
              # barplots consommation et production
              , plotOutput('consommation_production')
              
              # consommation totale sur la periode consideree
              , valueBox(value = textOutput('valeur_de_la_consommation'),
                         subtitle = 'Consommation totale en TWh'
                         , color = "blue", width = 6)
              # production totale sur la periode consideree
              , valueBox(value = textOutput('valeur_de_la_production'),
                         subtitle = 'Production totale en TWh'
                         , color = "blue", width = 6)
              # evolution des consommations et productions
              , h5(plotOutput('graphique_interactif'))
              
      ),
      # Deuxieme onglet : Details
      tabItem('Details',
              #Consommations sur l’ensemble de la periode choisie
              h5(dataTableOutput('Consommation'))
              
              , downloadButton("Download", "Telecharger")
              
              , h5(dataTableOutput('Production'))
              
              , downloadButton("Download2", "Telecharger")
      )
    )
  )
)


server <- function(input, output) {
  
  output$ui_region <-  renderUI({
    # Pour selectionner les regions correctement
    if (input$Maille == "Region" | input$Maille == "Departement" | input$Maille == "Commune"){
      Choix <- consommation_region %>%
        pull(Nom.Région) %>%
        unique()
      
      selectInput("region",
                  "Choisissez votre region :",
                  choices = sort(Choix),
                  selected = Choix[1])
    }
  })
  
  output$ui_departement <-  renderUI({
    # Pour selectionner les departements correctement
    if (input$Maille == "Departement" | input$Maille == "Commune"){
      Choix1 <- consommation_departement %>% 
        filter(Nom.Région == input$region) %>%
        pull(Nom.Département) %>%
        unique()
      
      selectInput("departement",
                  "Choisissez votre departement :",
                  choices = sort(Choix1),
                  selected = Choix1[1])
    }
  })
  
  output$ui_commune <-  renderUI({
    
    # Pour selectionner les communes correctement
    if (input$Maille == "Commune"){
      Choix2 <- consommation_commune %>% 
        filter(Nom.Département == input$departement) %>%
        pull(Nom.Commune) %>%
        unique()
      
      selectInput("commune",
                  "Choisissez votre commune :",
                  choices = sort(Choix2),
                  selected = Choix2[1])
    }
  })
  
  output$ui_moyenne <-  renderUI({
    
    # Pour selectionner les moyennes correctement
    if (input$Maille == "Commune"){
      selectInput("moyenne",
                  "Choisissez la ou les moyennes :",
                  choices = c('Moyenne Region', 'Moyenne Departement'),
                  multiple = TRUE)
    }
    else if (input$Maille == "Departement"){
      selectInput("moyenne",
                  "Choisissez la moyenne :",
                  choices = c('Moyenne Region'))
    }
  })
  
  # On filtre selon le nom de la collectivite et l'annee
  trie <- reactive({
    if (input$Maille == "Commune"){
      out <- consommation_commune %>%
        filter(Nom.Commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out <- consommation_departement %>% 
        filter(Nom.Département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out <- consommation_region %>% 
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out <- consommation_region %>%
        filter(Année %in% input$annee)
    }
    out <- out %>%
      select(Année, CODE.GRAND.SECTEUR, Conso.totale..MWh.) %>%
      filter(Conso.totale..MWh. != "NA") %>%
      filter(Conso.totale..MWh. != 0) %>%
      group_by(Année, CODE.GRAND.SECTEUR) %>%
      summarise('Consommation_totale' = sum(Conso.totale..MWh.)) %>%
      rename(Secteur = CODE.GRAND.SECTEUR)
    out
  })
  
  trie_tot <- reactive({
    if (input$Maille == "Commune"){
      out_tot <- consommation_commune %>% 
        filter(Nom.Commune == input$commune) %>%
        filter(Nom.Département == input$departement) %>%
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_tot <- consommation_departement %>%
        filter(Nom.Département == input$departement) %>%
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_tot <- consommation_region %>%
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_tot <- consommation_region %>%
        filter(Année %in% input$annee)
    }
    out_tot <- out_tot %>%
      select(Année, Conso.totale..MWh.) %>%
      filter(Conso.totale..MWh. != "NA") %>%
      filter(Conso.totale..MWh. != 0) %>%
      summarise(sum(Conso.totale..MWh.)/1000000)
    
    out_tot 
  })
  
  output$valeur_de_la_consommation <- renderText({
    
    conso()
    
  })
  
  trie_tot2 <- reactive({
    # Si la maille est la commune
    if (input$Maille == "Commune"){
      out_tot2 <- production_commune %>% 
        filter(Nom.commune == input$commune) %>%
        filter(Nom.département == input$departement) %>%
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    # Si la maille est le departement
    else if (input$Maille == "Departement"){
      out_tot2 <- production_departement %>% 
        filter(Nom.département == input$departement) %>%
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    # Si la maille est la region
    else if (input$Maille == "Region"){
      out_tot2 <- production_region %>%
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    # Si la maille est la moyenne en France
    else{
      out_tot2 <- production_region %>%
        filter(Année %in% input$annee)
    }
    # Pour les valeurs manquantes
    out_tot2[is.na(out_tot2)] <- 0
    out_tot2 <- out_tot2 %>%
      select(Année, Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
             Energie.produite.annuelle.Eolien.Enedis..MWh.,
             Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
             Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
             Energie.produite.annuelle.Cogénération.Enedis..MWh.,
             Energie.produite.annuelle.Autres.filières.Enedis..MWh.)
    out_tot2 <- mutate(out_tot2, production_totale = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.+
                                  Energie.produite.annuelle.Eolien.Enedis..MWh.+
                                  Energie.produite.annuelle.Hydraulique.Enedis..MWh.+
                                  Energie.produite.annuelle.Bio.Energie.Enedis..MWh.+
                                  Energie.produite.annuelle.Cogénération.Enedis..MWh.+
                                  Energie.produite.annuelle.Autres.filières.Enedis..MWh.)
    out_tot2 <- out_tot2 %>% summarise(sum(production_totale)/1000000)
    
    out_tot2 
  })
  
  output$valeur_de_la_production <- renderText({
    
    prod()
    
  })
  
  triePlot <- reactive({
    outPlot <- trie() %>%
      group_by(Secteur) %>%
      summarise('total_moyen_MWh' = round(mean(Consommation_totale), 2))
    
    outPlot <- mutate(outPlot, Type = "Consommation")
    
    outPlot
  })
  
  trie2 <- reactive({
    if (input$Maille == "Commune"){
      out2 <- production_commune %>% 
        filter(Nom.commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out2 <- production_departement %>% 
        filter(Nom.département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out2 <- production_region %>% 
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out2 <- production_region %>%
        filter(Année %in% input$annee)
    }
    
    out2 <- out2 %>%
      select(Année,
             Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
             Energie.produite.annuelle.Eolien.Enedis..MWh.,
             Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
             Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
             Energie.produite.annuelle.Cogénération.Enedis..MWh.,
             Energie.produite.annuelle.Autres.filières.Enedis..MWh.,) %>%
      rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
             Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.,
             Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
             Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
             Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.,
             Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.)
    out2[is.na(out2)] <- 0
    
    out2
  })
  
  triePlot2 <- reactive({
    outPlot2 <- trie2() %>%
      group_by(Année) %>%
      summarise(Photovoltaique = sum(Photovoltaique),
                Eolien = sum(Eolien),
                Hydraulique = sum(Hydraulique),
                Bio = sum(Bio),
                Cogeneration = sum(Cogeneration),
                Autres = sum(Autres)) %>%
      summarise(Photovoltaique = round(mean(Photovoltaique),2),
                Eolien = round(mean(Eolien),2),
                Hydraulique = round(mean(Hydraulique),2),
                Bio = round(mean(Bio),2),
                Cogeneration = round(mean(Cogeneration),2),
                Autres = round(mean(Autres),2)
                )
    
    outPlot2 <- t(outPlot2)
    names <- c("Photovoltaique","Eolien","Hydraulique","Bio","Cogeneration","Autres")
    produc <- outPlot2[,1]
    outPlot2 <- data.frame(Secteur = names, total_moyen_MWh = produc)
    outPlot2 <- mutate(outPlot2, Type = "Production")
    outPlot2
  })
  

  
  
  trieTable <- reactive({
    consommation <- triePlot()
    production <- triePlot2()
    combi <- rbind(consommation, production)
    
    combi <- combi %>% filter(total_moyen_MWh != 0)
    
    combi
  })
  
  output$consommation_production <- renderPlot({
    Action_barplot()
    
  })
  
  Action_barplot <- eventReactive(input$action,{
    
    combi <- trieTable()
    
    ggplot(combi) + 
      geom_bar(stat="identity") +
      aes(x = Type ,y = total_moyen_MWh, fill = Secteur) +
      ggtitle("Consommation et production pour une annee moyenne de la collectivite")
    
  })
  
  table <- eventReactive(input$action,{
    
    out <-  trieTable()
    out
    
  })
  
  conso <- eventReactive(input$action,{
    
    out_tot <-  round(trie_tot(),2)
    out_tot <- as.character(out_tot)
    out_tot
    
  })
  
  prod <- eventReactive(input$action,{
    
    out_tot2 <-  round(trie_tot2(),2)
    out_tot2 <- as.character(out_tot2)
    out_tot2
    
  })
  
  trie_graph_industrie <- reactive({
    
    if (input$Maille == "Commune"){
      out_annee <- consommation_commune %>% 
        filter(Nom.Commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee <- consommation_departement %>% 
        filter(Nom.Département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee <- consommation_region %>% 
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee <- consommation_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee[is.na(out_annee)] <- 0
    out_annee <- out_annee %>%
      filter(CODE.GRAND.SECTEUR == "INDUSTRIE") %>%
      group_by(Année)%>%
      summarise(a = sum(Conso.totale..MWh.))
    out_annee <- mutate(out_annee, sect = 'industrie', type = 'consommation')
    out_annee
  })
  
  trie_graph_agriculture <- reactive({
    
    if (input$Maille == "Commune"){
      out_annee <- consommation_commune %>% 
        filter(Nom.Commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee <- consommation_departement %>% 
        filter(Nom.Département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee <- consommation_region %>% 
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee <- consommation_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee[is.na(out_annee)] <- 0
    out_annee <- out_annee %>%
      filter(CODE.GRAND.SECTEUR == "AGRICULTURE") %>%
      group_by(Année)%>%
      summarise(a = sum(Conso.totale..MWh.))
    out_annee <- mutate(out_annee, sect = 'agriculture', type = 'consommation')
    out_annee
  })
  
  trie_graph_inconnu <- reactive({
    
    if (input$Maille == "Commune"){
      out_annee <- consommation_commune %>% 
        filter(Nom.Commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee <- consommation_departement %>% 
        filter(Nom.Département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee <- consommation_region %>% 
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee <- consommation_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee[is.na(out_annee)] <- 0
    out_annee <- out_annee %>%
      filter(CODE.GRAND.SECTEUR == "INCONNU") %>%
      group_by(Année)%>%
      summarise(a = sum(Conso.totale..MWh.))
    out_annee <- mutate(out_annee, sect = 'inconnu', type = 'consommation')
    out_annee
  })
  
  trie_graph_petit <- reactive({
    
    if (input$Maille == "Commune"){
      out_annee <- consommation_commune %>% 
        filter(Nom.Commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee <- consommation_departement %>% 
        filter(Nom.Département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee <- consommation_region %>% 
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee <- consommation_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee[is.na(out_annee)] <- 0
    out_annee <- out_annee %>%
      filter(CODE.GRAND.SECTEUR == "PETIT_PROFESSIONNEL") %>%
      group_by(Année)%>%
      summarise(a = sum(Conso.totale..MWh.))
    out_annee <- mutate(out_annee, sect = 'petit professionnel', type = 'consommation')
    out_annee
  })
  
  trie_graph_residentiel <- reactive({
    
    if (input$Maille == "Commune"){
      out_annee <- consommation_commune %>% 
        filter(Nom.Commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee <- consommation_departement %>% 
        filter(Nom.Département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee <- consommation_region %>% 
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee <- consommation_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee[is.na(out_annee)] <- 0
    out_annee <- out_annee %>%
      filter(CODE.GRAND.SECTEUR == "RESIDENTIEL") %>%
      group_by(Année)%>%
      summarise(a = sum(Conso.totale..MWh.))
    out_annee <- mutate(out_annee, sect = 'residentiel', type = 'consommation')
    out_annee
  })
  
  trie_graph_tertiaire <- reactive({
    
    if (input$Maille == "Commune"){
      out_annee <- consommation_commune %>% 
        filter(Nom.Commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee <- consommation_departement %>% 
        filter(Nom.Département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee <- consommation_region %>% 
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee <- consommation_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee[is.na(out_annee)] <- 0
    out_annee <- out_annee %>%
      filter(CODE.GRAND.SECTEUR == "TERTIAIRE") %>%
      group_by(Année)%>%
      summarise(a = sum(Conso.totale..MWh.))
    out_annee <- mutate(out_annee, sect = 'tertiaire', type = 'consommation')
    out_annee
  })
  
  trie_graphique_interactif2_photo <- reactive({
    
    if (input$Maille == "Commune"){
      out_annee2 <- production_commune %>% 
        filter(Nom.commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee2 <- production_departement %>% 
        filter(Nom.département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee2 <- production_region %>% 
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee2 <- production_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee2[is.na(out_annee2)] <- 0
    out_annee2 <- out_annee2 %>%
      select(Année, Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.)
    out_annee2 <- out_annee2 %>%
      group_by(Année) %>%
      summarise(a = sum(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.))
    out_annee2 <- mutate(out_annee2, sect = 'photovoltaique', type = 'production')
  })
  
  trie_graphique_interactif2_eolien <- reactive({
    
    
    if (input$Maille == "Commune"){
      out_annee2 <- production_commune %>% 
        filter(Nom.commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee2 <- production_departement %>% 
        filter(Nom.département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee2 <- production_region %>% 
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee2 <- production_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee2[is.na(out_annee2)] <- 0
    out_annee2 <- out_annee2 %>%
      select(Année,Energie.produite.annuelle.Eolien.Enedis..MWh.)
    out_annee2 <- out_annee2 %>%
      group_by(Année) %>%
      summarise(a = sum(Energie.produite.annuelle.Eolien.Enedis..MWh.))
    out_annee2 <- mutate(out_annee2, sect = 'eolien', type = 'production')
  })
  
  trie_graphique_interactif2_hydro <- reactive({
    
    
    if (input$Maille == "Commune"){
      out_annee2 <- production_commune %>% 
        filter(Nom.commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee2 <- production_departement %>% 
        filter(Nom.département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee2 <- production_region %>% 
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee2 <- production_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee2[is.na(out_annee2)] <- 0
    out_annee2 <- out_annee2 %>%
      select(Année,Energie.produite.annuelle.Hydraulique.Enedis..MWh.)
    out_annee2 <- out_annee2 %>%
      group_by(Année) %>%
      summarise(a = sum(Energie.produite.annuelle.Hydraulique.Enedis..MWh.))
    out_annee2 <- mutate(out_annee2, sect = 'hydraulique', type = 'production')
  })
  
  trie_graphique_interactif2_bio <- reactive({
    
    
    if (input$Maille == "Commune"){
      out_annee2 <- production_commune %>% 
        filter(Nom.commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee2 <- production_departement %>% 
        filter(Nom.département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee2 <- production_region %>% 
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee2 <- production_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee2[is.na(out_annee2)] <- 0
    out_annee2 <- out_annee2 %>%
      select(Année,Energie.produite.annuelle.Bio.Energie.Enedis..MWh.)
    out_annee2 <- out_annee2 %>%
      group_by(Année) %>%
      summarise(a = sum(Energie.produite.annuelle.Bio.Energie.Enedis..MWh.))
    out_annee2 <- mutate(out_annee2, sect = 'bio', type = 'production')
  })
  
  trie_graphique_interactif2_co <- reactive({
    
    
    if (input$Maille == "Commune"){
      out_annee2 <- production_commune %>% 
        filter(Nom.commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee2 <- production_departement %>% 
        filter(Nom.département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee2 <- production_region %>% 
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee2 <- production_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee2[is.na(out_annee2)] <- 0
    out_annee2 <- out_annee2 %>%
      select(Année,Energie.produite.annuelle.Cogénération.Enedis..MWh.)
    out_annee2 <- out_annee2 %>%
      group_by(Année) %>%
      summarise(a = sum(Energie.produite.annuelle.Cogénération.Enedis..MWh.))
    out_annee2 <- mutate(out_annee2, sect = 'cogeneration', type = 'production')
  })
  
  trie_graphique_interactif2_autre <- reactive({
    
    
    if (input$Maille == "Commune"){
      out_annee2 <- production_commune %>% 
        filter(Nom.commune == input$commune) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Departement"){
      out_annee2 <- production_departement %>% 
        filter(Nom.département == input$departement) %>%
        filter(Année %in% input$annee)
    }
    else if (input$Maille == "Region"){
      out_annee2 <- production_region %>% 
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee)
    }
    else{
      out_annee2 <- production_region %>%
        filter(Année %in% input$annee)
    }
    
    out_annee2[is.na(out_annee2)] <- 0
    out_annee2 <- out_annee2 %>%
      select(Année,Energie.produite.annuelle.Autres.filières.Enedis..MWh.)
    out_annee2 <- out_annee2 %>%
      group_by(Année) %>%
      summarise(a = sum(Energie.produite.annuelle.Autres.filières.Enedis..MWh.))
    out_annee2 <- mutate(out_annee2, sect = 'autres', type = 'production')
  })
  
  output$graphique_interactif <- renderPlot({
    Act_evolution()
  })
  
  Act_evolution <- eventReactive(input$action,{
    
    pl_industrie <- trie_graph_industrie()
    pl_agriculture <- trie_graph_agriculture()
    pl_inconnu <- trie_graph_inconnu()
    pl_petit <- trie_graph_petit()
    pl_residentiel <- trie_graph_residentiel()
    pl_tertiaire <- trie_graph_tertiaire()
    pl2_autre <- trie_graphique_interactif2_autre()
    pl2_co <- trie_graphique_interactif2_co()
    pl2_bio <- trie_graphique_interactif2_bio()
    pl2_hydro <- trie_graphique_interactif2_hydro()
    pl2_eolien <- trie_graphique_interactif2_eolien()
    pl2_photo <- trie_graphique_interactif2_photo()
    
    data <- rbind(pl_industrie,
                  pl_agriculture,
                  pl_inconnu,
                  pl_petit,
                  pl_residentiel,
                  pl_tertiaire,
                  pl2_autre,
                  pl2_co,
                  pl2_bio,
                  pl2_hydro,
                  pl2_eolien,
                  pl2_photo)
    
    print(data)
    
    ggplot(data=data, aes(x = Année, y = a)) +
      geom_line(aes(group = sect, colour = sect), size=1.2) +
      ggtitle("Evolution de la consommation et de la production au fil des annees") +
      ylab("MWh") + xlab("Annee") +
      facet_grid(. ~ type)
    
  })
  
  trie_Consommation <- reactive({
    
    if(input$Maille == "Commune"){
      
      Consommation <- consommation_commune %>%
        filter(Nom.Région == input$region) %>%
        filter(Nom.Département == input$departement) %>%
        filter(Nom.Commune == input$commune) %>%
        filter(Année %in% input$annee) %>%
        filter(Conso.totale..MWh. != "NA") %>%
        group_by(CODE.GRAND.SECTEUR) %>%
        summarise(Conso_tot_moy_com = round(mean(Conso.totale..MWh.), 2)) %>%
        rename(Secteur = CODE.GRAND.SECTEUR)
      
      if ("Moyenne Departement" %in% input$moyenne){
        
        moyenne_departement <- consommation_commune %>%
          filter(Nom.Région == input$region) %>%
          filter(Nom.Département == input$departement) %>%
          filter(Année %in% input$annee) %>%
          filter(Conso.totale..MWh. != "NA") %>%
          group_by(CODE.GRAND.SECTEUR) %>%
          summarise(Conso_tot_moy_com_en_dep = round(mean(Conso.totale..MWh.), 2)) %>%
          select(Conso_tot_moy_com_en_dep)
        
        Consommation <- cbind(Consommation, moyenne_departement)
        
      }
      
      if ("Moyenne Region" %in% input$moyenne){
        
        moyenne_region <- consommation_commune %>%
          filter(Nom.Région == input$region) %>%
          filter(Année %in% input$annee) %>%
          filter(Conso.totale..MWh. != "NA") %>%
          group_by(CODE.GRAND.SECTEUR) %>%
          summarise(Conso_tot_moy_com_en_reg = round(mean(Conso.totale..MWh.), 2)) %>%
          select(Conso_tot_moy_com_en_reg)
        
        Consommation <- cbind(Consommation, moyenne_region)
        
      }
      
      moyenne_France <- consommation_commune %>%
        filter(Année %in% input$annee) %>% 
        filter(Conso.totale..MWh. != "NA") %>%
        group_by(CODE.GRAND.SECTEUR) %>%
        summarise(Moyenne_France = round(mean(Conso.totale..MWh.), 2)) %>%
        select(Moyenne_France)
      
      Consommation <- cbind(Consommation, moyenne_France)
      
      Consommation <- mutate(Consommation, ecart_moy_pourcent =  round(Conso_tot_moy_com*100/Moyenne_France,2))
      
    }
    
    else if(input$Maille == "Departement"){
      
      Consommation <- consommation_departement %>%
        filter(Nom.Région == input$region) %>%
        filter(Nom.Département == input$departement) %>%
        filter(Année %in% input$annee) %>%
        filter(Conso.totale..MWh. != "NA") %>%
        group_by(CODE.GRAND.SECTEUR) %>%
        summarise(Conso_tot_moy_dep = round(mean(Conso.totale..MWh.), 2)) %>%
        rename(Secteur = CODE.GRAND.SECTEUR)
      
      if ("Moyenne Region" %in% input$moyenne){
        
        moyenne_region <- consommation_departement %>%
          filter(Nom.Région == input$region) %>%
          filter(Année %in% input$annee) %>%
          filter(Conso.totale..MWh. != "NA") %>%
          group_by(CODE.GRAND.SECTEUR) %>%
          summarise(Conso_tot_moy_com_en_reg = round(mean(Conso.totale..MWh.), 2)) %>%
          select(Conso_tot_moy_com_en_reg)
        
        Consommation <- cbind(Consommation, moyenne_region)
        
      }
      
      moyenne_France <- consommation_departement %>%
        filter(Année %in% input$annee) %>% 
        filter(Conso.totale..MWh. != "NA") %>%
        group_by(CODE.GRAND.SECTEUR) %>%
        summarise(Moyenne_France = round(mean(Conso.totale..MWh.), 2)) %>%
        select(Moyenne_France)
      
      Consommation <- cbind(Consommation, moyenne_France)
      
      Consommation <- mutate(Consommation, ecart_moy_pourcent =  round(Conso_tot_moy_dep*100/Moyenne_France,2))
      
    }
    
    else if(input$Maille == "Region"){
      
      Consommation <- consommation_region %>%
        filter(Nom.Région == input$region) %>%
        filter(Année %in% input$annee) %>%
        filter(Conso.totale..MWh. != "NA") %>%
        group_by(CODE.GRAND.SECTEUR) %>%
        summarise(Conso_tot_moy_reg = round(mean(Conso.totale..MWh.), 2)) %>%
        rename(Secteur = CODE.GRAND.SECTEUR)
      
      moyenne_France <- consommation_region %>%
        filter(Année %in% input$annee) %>% 
        filter(Conso.totale..MWh. != "NA") %>%
        group_by(CODE.GRAND.SECTEUR) %>%
        summarise(Moyenne_France = round(mean(Conso.totale..MWh.), 2)) %>%
        select(Moyenne_France)
      
      Consommation <- cbind(Consommation, moyenne_France)
      
      Consommation <- mutate(Consommation, ecart_moy_pourcent = round(Conso_tot_moy_reg*100/Moyenne_France,2))
      
    }
    
    else{
      Consommation <- consommation_region %>%
        filter(Année %in% input$annee) %>%
        filter(Conso.totale..MWh. != "NA") %>%
        group_by(CODE.GRAND.SECTEUR) %>%
        summarise(Moyenne_France = round(mean(Conso.totale..MWh.), 2)) %>%
        rename(Secteur = CODE.GRAND.SECTEUR)
    }
    
  })
  
  Action_Consommation <- eventReactive(input$action,{
    
    out_Consommation <- trie_Consommation()
    
  })
  
  output$Consommation <- renderDataTable({
    
    Action_Consommation()
    
  })
  
  trie_Production <- reactive({
    
    if(input$Maille == "Commune"){
      
      Production <- production_commune %>%
        filter(Nom.région == input$region) %>%
        filter(Nom.département == input$departement) %>%
        filter(Nom.commune == input$commune) %>%
        filter(Année %in% input$annee) %>%
        select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
               Energie.produite.annuelle.Eolien.Enedis..MWh.,
               Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
               Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
               Energie.produite.annuelle.Cogénération.Enedis..MWh.,
               Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
        rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
        rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
        rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
        rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
        rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
        rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        summarise(Photovoltaique = round(mean(Photovoltaique),2),
                  Eolien = round(mean(Eolien),2),
                  Hydraulique = round(mean(Hydraulique),2),
                  Bio = round(mean(Bio),2),
                  Cogeneration = round(mean(Cogeneration),2),
                  Autres = round(mean(Autres),2))
      
      Production <- t(Production)
      colnames(Production)[1] <- "prod_moy_com"
      
      
      if ("Moyenne Departement" %in% input$moyenne){
        
        moyenne_departement <- production_commune %>%
          filter(Nom.région == input$region) %>%
          filter(Nom.département == input$departement) %>%
          filter(Année %in% input$annee) %>%
          select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                 Energie.produite.annuelle.Eolien.Enedis..MWh.,
                 Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                 Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                 Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                 Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
          filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
          rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
          rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
          rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
          rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
          rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
          rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
          summarise(Photovoltaique = round(mean(Photovoltaique),2),
                    Eolien = round(mean(Eolien),2),
                    Hydraulique = round(mean(Hydraulique),2),
                    Bio = round(mean(Bio),2),
                    Cogeneration = round(mean(Cogeneration),2),
                    Autres = round(mean(Autres),2))
        
        moyenne_departement <- t(moyenne_departement)
        colnames(moyenne_departement)[1] <- "prod_moy_com_en_departement"
        
        Production <- cbind(Production, moyenne_departement)
        
      }
      
      if ("Moyenne Region" %in% input$moyenne){
        
        moyenne_region <- production_commune %>%
          filter(Nom.région == input$region) %>%
          filter(Année %in% input$annee) %>%
          select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                 Energie.produite.annuelle.Eolien.Enedis..MWh.,
                 Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                 Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                 Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                 Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
          filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
          rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
          rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
          rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
          rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
          rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
          rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
          summarise(Photovoltaique = round(mean(Photovoltaique),2),
                    Eolien = round(mean(Eolien),2),
                    Hydraulique = round(mean(Hydraulique),2),
                    Bio = round(mean(Bio),2),
                    Cogeneration = round(mean(Cogeneration),2),
                    Autres = round(mean(Autres),2))
        
        moyenne_region <- t(moyenne_region)
        colnames(moyenne_region)[1] <- "prod_moy_com_en_reg"
        
        Consommation <- cbind(Production, moyenne_region)
        
      }
      
      moyenne_France <- production_commune %>%
        filter(Année %in% input$annee) %>%
        select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
               Energie.produite.annuelle.Eolien.Enedis..MWh.,
               Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
               Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
               Energie.produite.annuelle.Cogénération.Enedis..MWh.,
               Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
        rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
        rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
        rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
        rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
        rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
        rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        summarise(Photovoltaique = round(mean(Photovoltaique),2),
                  Eolien = round(mean(Eolien),2),
                  Hydraulique = round(mean(Hydraulique),2),
                  Bio = round(mean(Bio),2),
                  Cogeneration = round(mean(Cogeneration),2),
                  Autres = round(mean(Autres),2))
      
      moyenne_France <- t(moyenne_France)
      colnames(moyenne_France)[1] <- "Moyenne_France"
      
      Production <- cbind(Secteur = rownames(Production), Production, moyenne_France)
      
      Production <- mutate(data.frame(Production),
                          ecart_moy_pourcent = round(as.numeric(prod_moy_com)*100/as.numeric(Moyenne_France),2))
      
    }
    
    else if(input$Maille == "Departement"){
      
      Production <- production_departement %>%
        filter(Nom.région == input$region) %>%
        filter(Nom.département == input$departement) %>%
        filter(Année %in% input$annee) %>%
        select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
               Energie.produite.annuelle.Eolien.Enedis..MWh.,
               Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
               Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
               Energie.produite.annuelle.Cogénération.Enedis..MWh.,
               Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
        rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
        rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
        rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
        rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
        rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
        rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        summarise(Photovoltaique = round(mean(Photovoltaique),2),
                  Eolien = round(mean(Eolien),2),
                  Hydraulique = round(mean(Hydraulique),2),
                  Bio = round(mean(Bio),2),
                  Cogeneration = round(mean(Cogeneration),2),
                  Autres = round(mean(Autres),2))
      
      Production <- t(Production)
      
      colnames(Production)[1] <- "prod_moy_departement"
      
      if ("Moyenne Region" %in% input$moyenne){
        
        moyenne_region <- production_departement %>%
          filter(Nom.région == input$region) %>%
          filter(Année %in% input$annee) %>%
          select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                 Energie.produite.annuelle.Eolien.Enedis..MWh.,
                 Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                 Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                 Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                 Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
          filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
          filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
          rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
          rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
          rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
          rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
          rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
          rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
          summarise(Photovoltaique = round(mean(Photovoltaique),2),
                    Eolien = round(mean(Eolien),2),
                    Hydraulique = round(mean(Hydraulique),2),
                    Bio = round(mean(Bio),2),
                    Cogeneration = round(mean(Cogeneration),2),
                    Autres = round(mean(Autres),2))
        
        moyenne_region <- t(moyenne_region)
        
        colnames(moyenne_region)[1] <- "prod_moy_dep_en_reg"
        
        Production <- cbind(Production, moyenne_region)
        
      }
      
      moyenne_France <- production_departement %>%
        filter(Année %in% input$annee) %>%
        select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
               Energie.produite.annuelle.Eolien.Enedis..MWh.,
               Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
               Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
               Energie.produite.annuelle.Cogénération.Enedis..MWh.,
               Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
        rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
        rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
        rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
        rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
        rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
        rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        summarise(Photovoltaique = round(mean(Photovoltaique),2),
                  Eolien = round(mean(Eolien),2),
                  Hydraulique = round(mean(Hydraulique),2),
                  Bio = round(mean(Bio),2),
                  Cogeneration = round(mean(Cogeneration),2),
                  Autres = round(mean(Autres),2))
      
      moyenne_France <- t(moyenne_France)
      colnames(moyenne_France)[1] <- "Moyenne_France"
      
      Production <- cbind(Secteur = rownames(Production), Production, moyenne_France)
      
      Production <- mutate(data.frame(Production), 
                          ecart_moy_pourcent = round(as.numeric(prod_moy_departement)*100/as.numeric(Moyenne_France),2))
      
    }
    
    else if(input$Maille == "Region"){
      
      Production <- production_region %>%
        filter(Nom.région == input$region) %>%
        filter(Année %in% input$annee) %>%
        select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
               Energie.produite.annuelle.Eolien.Enedis..MWh.,
               Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
               Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
               Energie.produite.annuelle.Cogénération.Enedis..MWh.,
               Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
        rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
        rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
        rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
        rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
        rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
        rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        summarise(Photovoltaique = round(mean(Photovoltaique),2),
                  Eolien = round(mean(Eolien),2),
                  Hydraulique = round(mean(Hydraulique),2),
                  Bio = round(mean(Bio),2),
                  Cogeneration = round(mean(Cogeneration),2),
                  Autres = round(mean(Autres),2))
      
      Production <- t(Production)
      
      colnames(Production)[1] <- "prod_moy_reg"
      
      moyenne_France <- production_region %>%
        filter(Année %in% input$annee) %>%
        select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
               Energie.produite.annuelle.Eolien.Enedis..MWh.,
               Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
               Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
               Energie.produite.annuelle.Cogénération.Enedis..MWh.,
               Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
        rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
        rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
        rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
        rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
        rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
        rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        summarise(Photovoltaique = round(mean(Photovoltaique),2),
                  Eolien = round(mean(Eolien),2),
                  Hydraulique = round(mean(Hydraulique),2),
                  Bio = round(mean(Bio),2),
                  Cogeneration = round(mean(Cogeneration),2),
                  Autres = round(mean(Autres),2))
      
      moyenne_France <- t(moyenne_France)
      
      rownames(moyenne_France) <- c(1,2,3,4,5,6,7,8,9,10,11,12)
      
      colnames(moyenne_France)[1] <- "Moyenne_France"
      
      Production <- cbind(Secteur = rownames(Production), Production, moyenne_France)
      
      Production <- mutate(data.frame(Production),
                          ecart_moy_pourcent = round(as.numeric(prod_moy_reg)*100/as.numeric(Moyenne_France),2))
      
    }
    
    else{
      Production <- production_region %>%
        filter(Année %in% input$annee) %>%
        select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
               Energie.produite.annuelle.Eolien.Enedis..MWh.,
               Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
               Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
               Energie.produite.annuelle.Cogénération.Enedis..MWh.,
               Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
        filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
        rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
        rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
        rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
        rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
        rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
        rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
        summarise(Photovoltaique = round(mean(Photovoltaique),2),
                  Eolien = round(mean(Eolien),2),
                  Hydraulique = round(mean(Hydraulique),2),
                  Bio = round(mean(Bio),2),
                  Cogeneration = round(mean(Cogeneration),2),
                  Autres = round(mean(Autres),2))
      
      Production <- t(Production)
      
      Production <- cbind(Secteur = rownames(Production), Production)
      
      colnames(Production)[2] <- "Moyenne_France"
    }
    
    Production
  })
  
  Action_Production <- eventReactive(input$action,{
    
    out_Production <- trie_Production()
    
  })
  
  output$Production <- renderDataTable({
    
    Action_Production()
    
  })
  
  output$Download <- downloadHandler(
    
    filename = function() {
      paste("Consommation", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(trie_Consommation(), file, row.names = FALSE)
    }
  )
  
  output$Download2 <- downloadHandler(
    
    filename = function() {
      paste("Production", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(trie_Production(), file, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)


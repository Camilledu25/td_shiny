# libraries
library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(devtools)
library(ggplot2) 

#Sample data 
#dat <- data.frame(dens = c(rnorm(100), rnorm(100, 10, 5)) 
#                  , lines = rep(c("a", "b"), each = 100)) 
#Plot. 
#ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5) 

#Sample data 
#dat <- data.frame(dens = c(rnorm(100), rnorm(100, 10, 5)) 
#                  , lines = rep(c("a", "b"), each = 100)) 
#Plot. 
ggplot(consos2, aes(x = conso_moyenne_residentiel_mwh_, fill = annee)) + geom_density(alpha = 0.2)
consos2=consos %>% select(conso_moyenne_residentiel_mwh_,annee)
consos2$annee<-as.factor(consos2$annee)
class(consos2$conso_moyenne_residentiel_mwh_)
#plot(density(consos$conso_moyenne_residentiel_mwh_))


# Preparation des données -------------------------------------------------

consos <- readRDS('data/consos_clean.RDS')
consos_quali <- consos%>%select(annee,nom_departement,nom_region)
consos_quanti <- consos%>%select(-annee,-nom_departement,-nom_region,-code_departement,-code_region)
##Consos mailles régionales pour l onglet regions



# ui ----------------------------------------------------------------------


# Define UI for application that draws a histogram
ui <- navbarPage(
  'Analyses des consommations electriques',
  
  tabPanel('Mon département',
           id = 'departements',
  


  sidebarLayout(
    sidebarPanel(
      
      # Choix du département 
      #choix parmi toutes les possibilités 
      selectInput("dep",
                  "Choisissez votre departement:",
                  choices = levels(consos$nom_departement),
                  selected = 'Doubs')
    ),
    
    # Choix de l'année 
    selectInput("annee",
                "Choisissez l'annee:",
                choices = sort(unique(consos$annee)),
                multiple=TRUE,
                selected = 2017)
  ),
    
    mainPanel(
      ##affichage du nom du departement
      h3(textOutput('nom_dep')),
      
      h3(textOutput('nom_annee')),
      
      ####TODO: remplacer par la table par un datatable 
      tableOutput('ma_table'),
      plotOutput('repartition'),
      ## évolution des consos par secteur au cours du temps
      plotOutput('evolution'),
      ## répartition des consos par secteur et année
      plotOutput('boxplot_conso_moyenne')
      
    )
    
     ), ###fin du premier onglet
  tabPanel("densité",
           
  selectInput("quali",
              "Choisissez une variable qualitative:",
               choices = colnames(consos_quali),
               selected = 'annee')
  , 
  
  selectInput("quanti",
              "Choisissez une variable quantitative:",
              choices = colnames(consos_quanti),
              selected = 'conso_moyenne_residentiel_mwh_'),
  mainPanel(
    plotOutput('graph_densité')
  )
),


  tabPanel("cartographie")
  #####TODO: rajouter les onglets suivants :
  #####Analyse des determinants de la conso
  #####Cartographie
  
)



# Server ------------------------------------------------------------------


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$nom_dep <- renderText({
    ##TODO: modifier pour afficher le nom du departement!!!! ok
    input$dep
  })
  
  output$nom_annee <- renderText({
    input$annee
  })
  
  # Cette fonction filtre le jeu de données entier
  # pour ne garder que ce qui est intéressant
  

  filtre <- reactive({
    ##TODO: rajouter aussi un filtre sur les annees
    consos %>% 
      filter(nom_departement == input$dep)%>%filter(annee %in% input$annee)
  })
  
  
  get_departement_region <- reactive({
    
    ##recuperer la region
    region <- consos %>% 
      filter(nom_departement == input$dep) %>%
      filter(annee %in% input$annee)  %>%
      distinct(annee, nom_region)
    
    ##recuperer tous les autres departemetns de la meme region 
    consos_region <- consos %>%
      inner_join(region, by = c('annee',"nom_region"))
    
    ##selectionner seulement l'annee, le dep et les consos moyennes
    consos_region <- consos_region %>%
      select(annee, nom_departement, contains('conso_moyenne'))
    
    
    print(head(consos_region))
    consos_region
    
  })
  
  get_table_quali_quanti_choisi<- reactive({
    consos %>% select(input$quanti,input$quali)
    
  })
  
  ##Creation de la table a afficher
  ##TODO : remplacer par un datatable (dans server et ui)
  ##TODO: prendre toute la table et pas les six premieres lignes 
   output$ma_table <- renderTable({
   out <-  filtre() %>%
     select(annee,  conso_totale_residentiel_mwh_,
             conso_totale_professionnel_mwh_,
             conso_totale_agriculture_mwh_,
             conso_totale_tertiaire_mwh_,
             conso_totale_autres_mwh_)
   print(out)
   out
  } )
   
   
   output$repartition <- renderPlot({
     
     df_filtre <- filtre() %>%
       select(annee,  conso_totale_residentiel_mwh_,
              conso_totale_professionnel_mwh_,
              conso_totale_agriculture_mwh_,
              conso_totale_tertiaire_mwh_,
              conso_totale_autres_mwh_) %>%
       tidyr::pivot_longer(-c("annee"))
     
     
     ggplot(df_filtre) +
       geom_bar(stat = 'identity') +
       aes(y  = value, x = annee, fill = name)
     
   })    
  
   output$evolution <- renderPlot({
     
     df <- filtre() %>%
       select(annee,  conso_totale_residentiel_mwh_,
              conso_totale_professionnel_mwh_,
              conso_totale_agriculture_mwh_,
              conso_totale_tertiaire_mwh_,
              conso_totale_autres_mwh_)%>%
        tidyr::pivot_longer(-c("annee"))
     
     
     fig= ggplot(df) +
       aes(y  = value, x = annee, color = name)+
       geom_line()
     
     fig
     
   })
   
   output$boxplot_conso_moyenne <- renderPlot({
     
     
     df <- get_departement_region() %>%
       pivot_longer(-c("annee", "nom_departement")) %>%
       mutate(annee = as.character(annee),
              name = str_replace(name, pattern = 'conso_moyenne_', rep = '') %>%
                str_replace(pattern = 'mwh_', rep = '')
       )  
     
     
     
     ggplot(df) +
       geom_boxplot() + 
       facet_wrap(~ name  , scales = 'free') +
       aes(y = value,   x = annee,  fill = annee)+
       theme(legend.position = 'bottom' ) + 
       ggtitle('Le titre') + 
       ylab('les ordonneees') + 
       xlab('les abscisse')
     
     
   })   
   output$graph_densité <- renderPlot({
     
     
     df=get_table_quali_quanti_choisi()
     
     df= df%>%
       mutate(
         df[,1]<-as.numeric(df[,1]),
         df[,2]<-as.factor(df[,2])
       )


     
     ggplot(df, aes(x = df[,1], fill = df[,2])) + 
       geom_density(alpha = 0.2)    
     
   })
}



# Run the application 
shinyApp(ui = ui, server = server)



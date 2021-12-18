library(shiny)
library(shinythemes)


library(lubridate)
library(tidyverse)
library(ggpubr)

shinyUI(fluidPage( 
    
    theme = shinytheme("flatly"),
    #themeSelector(),
    
    navbarPage(title="AGBO et GNING",id="nav",

               tabPanel("Mon Espace ullule ",
                        sidebarLayout(
                            
                            sidebarPanel( 
                                
                                selectInput("choix","Choisir l'evolution de",choices = c("Nombre total de campagnes crées","Montant moyen de campagne financée","Durée médiane de campagne"),selected ="Montant moyen de campagne financée" ),
                                br(),
                                

                                # br(),
                                
                                selectInput("moncategory","Choisir au moins une catégorie",choices =categories_principales ,selected = "Musique",multiple = TRUE),
                                
                                br(),
                                
                                
                            
                                actionButton("submit","Aplliquer",type="primary"),
                                
                                downloadButton("dl","Télécharger")
                                

                                

                                
                                
                                
                                
                            ),
                            
                            mainPanel(
                                
                                uiOutput("evo")
                                
                                
                                
                                
                            )
                        )
               )
               
    ))
    
    
    
)


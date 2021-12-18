library(shiny)
library(shinythemes)
library(plotly)
library(lubridate)
library(tidyverse)
library(data.table)
library(ggpubr)

source('global.R')

shinyServer(
    function(input,output)
    {
        
        data_top <- data
        data_perimeter <- data
        data_perimeter <- eventReactive(input$submit,{data %>% filter(categories %in% input$moncategory)},ignoreNULL=FALSE)
        data_top <- eventReactive(input$submit,{data %>% filter(categories %in% input$moncategory) %>% arrange(desc(montant)) %>% 
                select(id,category,montant,year_start)},ignoreNULL=FALSE)
        
            
        
        output$Evolution<-renderPlotly(
            
            {
                if(input$choix=="Nombre total de campagnes crées"){
                    
                    nombre_de_campagnes_par_mois <- data_perimeter() %>% 
                        group_by(month_start,year_start) %>% 
                        summarise(nbre_campagnes=sum(is_cancelled==FALSE)) %>% 
                        mutate(date= as.Date(paste(year_start,month_start,"01",sep="-"),"%Y-%m-%d")) %>% 
                        arrange(year_start)
                    p <- ggplot(data=nombre_de_campagnes_par_mois,aes(x=date,y=nbre_campagnes))+
                        geom_line(color="blue")+
                        theme_bw() +
                        ggtitle("Nombre de campagnes par mois")+
                        labs(title=paste("Evolution  des campagnes crées par mois",input$moncategory),x="Mois",y="nombre de campagnes")
                    ggplotly(p)
                    }else if(input$choix=="Montant moyen de campagne financée"){
                        montant_moyen_de_campagnes_financees <- data_perimeter() %>% 
                            filter(goal_raised==TRUE) %>% 
                            group_by(month_start,year_start) %>% 
                            summarise(montant_moyen=mean(amount_raised)) %>% 
                            mutate(date= as.Date(paste(year_start,month_start,"01",sep="-"),"%Y-%m-%d")) %>% 
                            arrange(year_start)
                        p <- ggplot(data=montant_moyen_de_campagnes_financees,aes(x=date,y=montant_moyen))+
                            geom_line(color="blue")+
                            theme_bw() +
                            labs(x = "Mois", y = "Montant moyen") +
                            ggtitle("Montant moyen de campagnes financées")
                        ggplotly(p)
                        
                    }else{
                        
                        
                        duree_median_des_campagnes <- data_perimeter() %>% filter(  goal_raised==TRUE) %>% 
                            group_by(month_start,year_start) %>% 
                            summarise(duree_median=median(nombre_de_jours)) %>% 
                            mutate(date= as.Date(paste(year_start,month_start,"01",sep="-"),"%Y-%m-%d")) %>%
                            arrange(year_start)
                        p <- ggplot(data=duree_median_des_campagnes,aes(x=date,y=duree_median))+
                            geom_line(color="blue")+
                            theme_bw() +
                            labs(x = "Mois", y = "Durée_median") +
                            ggtitle("Durée médiane des campagnes")
                        ggplotly(p)
                        
                        
                    }
       }
       )
            
            
            
        
        
        
        output$Top<-renderDataTable(
            
            {
              
                Top10_campagnes_plus_financees <- data_perimeter() %>% 
                    arrange(desc(montant)) %>% 
                    select(id,category,montant,year_start)
                Top10_campagnes_plus_financees %>% 
                arrange(desc(montant)) %>% 
                    select(id,category,montant,year_start) 
            })
            
        
        
            output$dl <- downloadHandler(filename = function(){"top_campagnes.csv"},
                                         content=function(fname){
                                             write.csv(data_top(),fname)
                                         })
            

        
        
        
       

        
        output$evo<-renderUI({
            tabsetPanel(type="tab",
                        tabPanel("Evolution ",plotlyOutput("Evolution")),
                        tabPanel("Liste des campagnes les plus financées ",dataTableOutput("Top"))
                        
                        
                        
            )
            })
        
        
        
    }
    )






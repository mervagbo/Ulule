
  

library(rmarkdown)
library(shiny)
library(shinythemes)
library(dplyr)
library(lubridate)
library(ggpubr)
library(tidyverse)
library(readr)
library(skimr)
library(dplyr)
library(purrr)
library(lubridate)
library(ggplot2)
library(data.table)
library(stringr)
library(forcats)
library(tidyr)
library(stringi)
library(scales)
library(ggplotlyExtra)
library(plotly)


# Importation et prétraitement des données 

data <- read.csv('data_ulule_2019.csv', row.names=1)
paged_table(data)





colnames(data)


## Supression des variables n'ayant aucun interet



data <- data %>% select(-c("absolute_url","finished.1","percent","city","lang","comments_count","news_count","...1","region"))



## Supression des valeurs manquantes et dupliquées



data <- data %>% drop_na() %>% distinct()




## On va exclure du périmètre les campagnes annulées.



data<-data %>% filter(is_cancelled =="FALSE")




## On va garder ici que les 8 pays qui ont le plus de campagnes

#methode dplyr pour choisir les 8 premiers pays qui ont le plus de campagnes 
pays <- data %>%
  group_by(country) %>% 
  summarise(nbre_campagne=sum(is_cancelled==FALSE)) %>% 
  arrange(desc(nbre_campagne)) %>%
  slice(1:8) %>%
  select(country)

pays <- as.vector(pays$country)



#On va garder seulement les 8 pays qui ont le plus de campagnes 
data<-data %>%filter(country %in% pays) 


## On va exclure le mois le plus récent ou les données seraient eventuellement incomplètes

most_recent_date <-
  data %>% select(date_start) %>% arrange(desc(date_start)) %>% slice(1) %>% mutate(month = month(date_start), year = year(date_start))

month_to_eliminate <-
  data %>% filter(
    month(date_start) == most_recent_date$month &
      year(date_start) == most_recent_date$year
  )

data <- data %>% anti_join(month_to_eliminate)




## Convertir les devises  en EURO

table(data$currency)




#Convertir les devises en euro
USD_EUR<-0.88
CAD_EUR<-0.69
CHF_EUR<-0.96
GBP_EUR<-1.17

data <- data%>%
  mutate(montant = case_when(currency == "USD" ~as.double( amount_raised*USD_EUR),
                             currency == "CAD" ~ as.double(amount_raised*CAD_EUR),
                             currency == "CHF" ~as.double(amount_raised*CHF_EUR),
                             currency == "GBP" ~ as.double(amount_raised*GBP_EUR),
                             currency == "EUR" ~as.double( amount_raised)
                             
  ))




## Ajout de la variable nombre de jours des campagnes


data <- data %>% 
  mutate( nombre_de_jours= day(as.period(difftime(date_end,date_start))))





## On va créer une nouvelle variable categories regroupant les 6 principales catégories 


## Liste des 6 principales catégories

categories_principales <- data %>%
  group_by(category) %>% 
  summarise(nbre_campagne=sum(is_cancelled==FALSE)) %>% 
  arrange(desc(nbre_campagne)) %>%
  slice(1:6) %>%
  select(category)

categories_principales <- as.vector(categories_principales$category)
categories_principales <- ifelse(categories_principales=="Film et vid<e9>o","Film et vidéo",categories_principales)


## creaction d'une fonction
`%!in%` <- Negate(`%in%`)

## Ajout d'une nouvelle variable categories
data<-data %>%
  mutate(categories = case_when(category %in% categories_principales~category,
                                category %!in% categories_principales~"autre"
                                
  ))




# Divisuon des dates de debut en mois et annees
data <-  data %>%
  mutate(
    year_start=year(date_start), 
    month_start=month(date_start)
  )



# Valeurs des deux dernières années 
deux_dernieres_annees <- data %>% 
  select(year_start) %>% 
  distinct() %>% 
  arrange(desc(year_start)) %>%
  slice(1:2) %>%
  select(year_start)

deux_dernieres_annees <- as.vector(deux_dernieres_annees$year_start)
deux_dernieres_annees


data <- data %>% filter( year_start %in% deux_dernieres_annees) 
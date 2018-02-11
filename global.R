## global
library(shiny)
library(leaflet)
library(dplyr)
library(plotly)

# load data
sf_map <-read.csv('./data/sf_listings.csv')
sf_map <- sf_map[ , c("neighbourhood","latitude","longitude","room_type","price","reviews_per_month")]


# variables
neighbourhood <- unique(sf_map$neighbourhood)

room_type <- unique(sf_map$room_type)

groupColors <- colorFactor(c("#E03A3C", "#009DDC","#62BB47"),
                           domain = c("Entire home/apt", "Private room","Shared room"))
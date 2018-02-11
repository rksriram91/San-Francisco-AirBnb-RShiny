

library(shiny)
library(shinythemes)


# Define UI for application that draws a histogram
shinyUI(
  navbarPage(title = "Airbnb San Francisco Visualization", 
             id ="nav",
             
             theme = shinytheme("darkly"), #https://rstudio.github.io/shinythemes/
             
             
             ##### Overview ##########        
             tabPanel("Overview",
                      br(),
                      br(),
                      br(),
                      #img(src = "airbnb_overview.jpg", height = 600, weight =700, align="center")
                      #use Shiny's HTML tag functions to center the image
                      #https://stackoverflow.com/questions/34663099/how-to-center-an-image-in-a-shiny-app
                      HTML('<center><img src= "Airbnb.jpg", height = 600, weight =700 ></center>')
             ),
             
             ##### Map ##########      
             tabPanel("SF Map",
                      div(class="outer",
                          
                          
                          leafletOutput(outputId = "map", width = "100%", height = "1000px"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                                        top = 80, left = "auto", right = 20, bottom = "auto",
                                        width = 320, height = "auto",
                                        h2("Airbnb - SanFrancisco"),
                                        checkboxGroupInput(inputId = "select_neighbourhood", label = h4("neighbourhood"), 
                                                           choices = neighbourhood, selected = neighbourhood ),
                                        
                                        h6("The map information is based on May 02, 2017 dataset from"),
                                        h6(a("Inside Airbnb", href = "http://insideairbnb.com/get-the-data.html", target="_blank"))
                          ),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                                        top = 30, left = 20, right = "auto", bottom = 80,
                                        width = 320, height = 420,
                                        h2("Airbnb - SanFrancisco"),
                                       
                                        checkboxGroupInput(inputId = "select_room", label = h4("Room Type"), 
                                                           choices = room_type, selected = 'Private room'),
                                        sliderInput(inputId = "slider_price", label = h4("Price"), min = 30, max = 1850, step = 20,
                                                    pre = "$", sep = ",", value = c(30, 300)),
                                        sliderInput(inputId = "slider_rating", label = h4("Reviews_per_month"), min = 0, max = 14,step =0.5,
                                                    value = c(0,14)),
                                        h6("The map information is based on May 02, 2017 dataset from"),
                                        h6(a("Inside Airbnb", href = "http://insideairbnb.com/get-the-data.html", target="_blank"))
                          ),
                          
                          # Results: count_room, avgprice
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE, draggable = TRUE, 
                                        top = 620, left = 20, right = "auto" , bottom = "auto",
                                        width = 320, height = "auto",
                                        plotlyOutput(outputId = "count_room",height = 150),
                                        plotlyOutput(outputId = "avgprice", height = 150))
                          
                      )),
             
             ##### Listings ##########               
             tabPanel("Listings, Neighbourhoods and Price Changes",    
                      div(class="outer",
                        plotlyOutput(outputId = "graph1", width="75%", height =750),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE, draggable = TRUE, 
                                             top = "auto", left = "auto", right = "60", bottom = "40",
                                             width = 320, height = "auto",
                                             h3("Listings by Boroughs and Room Type"),
                                             br(),
                                             br(),
                                             sliderInput(inputId = "tab2_price", h4("Price/Night"), min = 30, max = 1850, value = c(30, 300),step=30),
                                             sliderInput(inputId = "tab2_rating", h4("Rating Score"), min = 0, max = 14, value = c(6,7)),
                                             br(),
                                             br())
                      )),
             
             ##### References ##########      
             navbarMenu("References",
                        tabPanel("Inside Airbnb",
                                 h3("Inside Airbnb", a("Link", href="http://insideairbnb.com/get-the-data.html"))),
                        
                        tabPanel("Airbnb Business Model",
                                 h3("Airbnb Business Model", a("Link", href="http://bmtoolbox.net/stories/airbnb/")))
             ) 

))

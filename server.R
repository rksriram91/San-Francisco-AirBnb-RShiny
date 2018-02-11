library(shiny)
library(leaflet)


# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
  
  
# reactivate map info

  mapdf <- reactive({
   sf_map %>%
     filter(neighbourhood %in% input$select_neighbourhood & 
              room_type %in% input$select_room &
              price >= input$slider_price[1] &
              price <= input$slider_price[2] &
              reviews_per_month >= input$slider_rating[1] &
              reviews_per_month <= input$slider_rating[2] 
 ) 
  })

  
  # create the map
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Esri.WorldStreetMap") %>%
      addLegend(position = "bottomleft", pal = groupColors, values = room_type, opacity = 10, title = "Room Type") %>% 
      setView(lng = -122.4194, lat = 37.77, zoom = 12)
  })
  
  # observe an event
  observe({ #require a trigger to call the observe function
    proxy <- leafletProxy("map",data = mapdf()) %>% #don't forget ()
      clearMarkerClusters() %>% 
      clearMarkers() %>%
      # circle
      addCircleMarkers(lng = ~longitude, lat = ~latitude, radius = 2, color = ~groupColors(room_type),
                       group = "CIRCLE",
                       popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                      'Room Type:', room_type,'<br/>',
                                      'Price:', price,'<br/>',
                                      'Number of Reviews:', reviews_per_month, '<br/>'
                                      )) %>% 
      # cluster
      addCircleMarkers(lng = ~longitude, lat = ~latitude, clusterOptions = markerClusterOptions(),
                       group = "CLUSTER",
                       popup = ~paste('<b><font color="Black">','Listing Information','</font></b><br/>',
                                      'Room Type: ', room_type, '<br/>',
                                      'Price:', price,'<br/>',
                                      'Number of Reviews:', reviews_per_month,'<br/>')) %>% 
      # circle/ cluster panel
      addLayersControl(
        baseGroups = c("CIRCLE","CLUSTER"),
        options = layersControlOptions(collapsed = FALSE)
      ) 
  })
  
  ## reactivate count dataframe for map graph1 
  countdf <- reactive({
    mapdf() %>%
      group_by(., room_type) %>%
      summarise(., count_type = n())
  })
  
  #map graph1 
  output$count_room <- renderPlotly({
    plot_ly(data = countdf(), x = ~room_type, y = ~count_type, type = "bar", color = ~room_type,
            colors = c('#E03A3C','#009DDC','#62BB47'),
            hoverinfo='text',
            text = ~count_type) %>%
      layout(xaxis = list(title = "", showticklabels = FALSE),
             yaxis = list(title = "count"), showlegend = FALSE,
             annotations = list(x = ~room_type, y = ~count_type, text = ~paste(round(count_type/sum(count_type),2)*100,'%'),
                                xanchor = 'center', yanchor = 'bottom',
                                showarrow = FALSE)) %>% 
      config(displayModeBar = FALSE)
  })
  
  ## reactivate price dataframe for map graph2
  pricedf <- reactive({
    mapdf() %>% 
      group_by(., room_type) %>% 
      summarise(., avg_price = round(mean(price),2))
  })
  
  #map graph2 avgprice
  output$avgprice <- renderPlotly({
    plot_ly(data = pricedf(), x = ~room_type, y = ~avg_price, type = "bar", color = ~room_type,
            colors = c('#E03A3C','#009DDC','#62BB47'),
            hoverinfo='text',
            text = ~avg_price) %>% 
      layout(xaxis = list(title = "", showticklabels = FALSE), 
             yaxis = list(title = "price"), showlegend = FALSE,
             annotations = list(x = ~room_type, y = ~avg_price, text = ~paste('$',avg_price),
                                xanchor = 'center', yanchor = 'bottom', 
                                showarrow = FALSE)) %>% 
      config(displayModeBar = FALSE)
  })
  
  
  ##### Listings, Boroughs and Price Changes #######################
  ## reactivate dataframe for listings grapgh
  graph1df <- reactive({
    sf_map %>%
      select(neighbourhood,room_type,price,reviews_per_month) %>% 
      filter(price >= input$tab2_price[1] &
               price <= input$tab2_price[2] &
               reviews_per_month >= input$tab2_rating[1] &
               reviews_per_month <= input$tab2_rating[2]) %>% 
      group_by(neighbourhood,room_type) %>% 
      summarise(n=n())
  })
  
  # listings grapgh
  output$graph1 <- renderPlotly({
    t <- list(size = 9)
    plot_ly(data = graph1df(), x = ~n, y = ~room_type, type = "bar", color = ~neighbourhood,
            colors = c('#800080','#009DDC','#E03A3C','#62BB47','#FFA500'),
            orientation = 'h', showlegend = TRUE) %>%
      layout(xaxis = list(title = "count"),
             yaxis = list(title = ""), 
             barmode = 'dodge', font = t)
  })
  
 
  
})

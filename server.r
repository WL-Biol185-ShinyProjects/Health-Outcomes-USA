library(shiny)
library(leaflet)
library(dplyr)


function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  

  observe({
    leafletProxy("map") %>%
    setView(lng = -93.85, lat = 37.45, zoom = 4)
    
  })
  
  
  ## Data Explorer ###########################################

}
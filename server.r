library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)

d_new
d_circles <- d_new

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  renderPlot({
    print(nrow(d_circles$measure_id))
    d_circles %>% filter(measure_id == input$outcome)
    
    print(nrow(d_circles$measure_id))
    
    leafletProxy("map", data = d_circles) %>%
      clearShapes() %>%
      addTiles() %>%
      addCircles(~longitude, ~latitude, radius=10,
                 stroke=FALSE, fillOpacity=0.4)
   })
  

  
    
  
  # changing the color and size of the circles on top of the map
  # (that show the prevalence of the health outcomes)
 # observe({
  #  colorBy <- input$color
  
  #  sizeBy <- input$sizes
    
   
  #    colorData <- d_new[[colorBy]]
   #   pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    
    
     # radius <- d_new[[sizeBy]] / max(d_new[[sizeBy]]) * 30000
    
    
    # adds circles as markers on top of the existing map
   # leafletProxy("map", data = d_new) %>%
    #  clearShapes() %>%
     # addTiles() %>%
    #  addCircles(~longitude, ~latitude, radius=3000,
           #      stroke=FALSE, fillOpacity=0.4)
    # %>%
    #  addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
     #           layerId="colorLegend")
#  })
  # layerId=~county,
  
  # Show a popup at the given location
#  showOutcomePopup <- function(county, latitude, longitude) {  # may need to change lat and lng inputs
 #   selectedOutcome <- d_new[d_new$county == county,]
        
#    content <- as.character(tagList(
 #     tags$h4("Percent:", selectedOutcome$Data_Value),
  #    tags$strong(HTML(sprintf("%s, %s",
    #                           selectedOutcome$CountyName, selectedOutcome$StateAbbr
   #   ))), tags$br(),
#      sprintf("Adult population: %s", selectedOutcome$TotalPopulation), tags$br(),
 #     sprintf("Measure: %s", selectedOutcome$Measure), tags$br(),
  #    sprintf("Data Source: %s", selectedOutcome$DataSource),
   # ))
#    leafletProxy("map") %>% addPopups(longitude, latitude, content, layerId = county)
 # }
  
  # When map is clicked, show a popup with city info
 # observe({
  #  leafletProxy("map") %>% clearPopups()
   # event <- input$map_shape_click
#    if (is.null(event))
 #     return()
    
  #  isolate({
   #   showOutcomePopup(event$id, event$latitude, event$longitude)
#    })
 # })
  
  
  ## Data Explorer ###########################################
  # Click the health outcome and display a graph showing counts of each outcome in each state.
  


}
  

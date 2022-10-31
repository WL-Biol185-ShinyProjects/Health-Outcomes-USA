library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)

d_clean


function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  
  # changing the color and size of the circles on top of the map
  # (that show the prevalence of the health outcomes)
  observe({
    colorBy <- input$color
    sizeBy <- input$sizes
    
   
      colorData <- d_cleans[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    
    
      radius <- d_clean[[sizeBy]] / max(d_clean[[sizeBy]]) * 30000
    
    
    # adds circles as markers on top of the existing map
    leafletProxy("map", data = d_clean) %>%
      clearShapes() %>%
      # will need longitude/latitude or some sort of location marker here
      
      addCircles(~longitude, ~latitude, radius=radius, layerId=~county,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showOutcomePopup <- function(county, latitude, longitude) {  # may need to change lat and lng inputs
    selectedOutcome <- d_clean[d_clean$county == county,]
        
    content <- as.character(tagList(
      tags$h4("Percent:", selectedOutcome$Data_Value),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedOutcome$CountyName, selectedOutcome$StateAbbr
      ))), tags$br(),
      sprintf("Adult population: %s", selectedOutcome$TotalPopulation), tags$br(),
      sprintf("Measure: %s", selectedOutcome$Measure), tags$br(),
      sprintf("Data Source: %s", selectedOutcome$DataSource),
    ))
    leafletProxy("map") %>% addPopups(longitude, latitude, content, layerId = county)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showOutcomePopup(event$id, event$latitude, event$longitude)
    })
  })
  
  
  ## Data Explorer ###########################################
  
  function(input, output){
    
    observeEvent(input$states, {
      d_clean$states <- states(100)
    })
    
    observeEvent(input$counties, {
      d_clean$county <- counties(100)
    })  
    
    output$plot <- renderPlot({
      
    })
  }
  
}
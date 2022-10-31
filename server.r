library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)

d_outcome


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
    
   
      colorData <- d_outcomes[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    
    
      radius <- d_outcome[[sizeBy]] / max(d_outcome[[sizeBy]]) * 30000
    
    
    # adds circles as markers on top of the existing map
    leafletProxy("map", data = d_outcome) %>%
      clearShapes() %>%
      # will need longitude/latitude or some sort of location marker here
      # and may need to change zipcode to fips or the county
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zipcode,
                 stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=pal, values=colorData, title=colorBy,
                layerId="colorLegend")
  })
  
  # Show a popup at the given location
  showOutcomePopup <- function(zipcode, lat, lng) {  # may need to change lat and lng inputs
    selectedOutcome <- allzips[allzips$zipcode == zipcode,]
        # will need to change zipcode and allzips to d_outcome?
    content <- as.character(tagList(
      tags$h4("Percent:", selectedOutcome$Data_Value),
      tags$strong(HTML(sprintf("%s, %s",
                               selectedOutcome$CountyName, selectedOutcome$StateAbbr
      ))), tags$br(),
      sprintf("Adult population: %s", selectedOutcome$TotalPopulation), tags$br(),
      sprintf("Measure: %s", selectedOutcome$Measure), tags$br(),
      sprintf("Data Source: %s", selectedOutcome$DataSource),
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showOutcomePopup(event$id, event$lat, event$lng)
    })
  })
  
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectizeInput(session, "cities", choices = cities,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectizeInput(session, "zipcodes", choices = zipcodes,
                         selected = stillSelected, server = TRUE)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  
}
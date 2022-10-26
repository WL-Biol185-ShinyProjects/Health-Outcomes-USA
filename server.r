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
  
  # Don't need to return set of zips in bounds right now
  # A reactive expression that returns the set of zips that are
  # in bounds right now
 # zipsInBounds <- reactive({
  #  if (is.null(input$map_bounds))
   #   return(d_outcomes[FALSE,])
  #  bounds <- input$map_bounds
   # latRng <- range(bounds$north, bounds$south)
  #  lngRng <- range(bounds$east, bounds$west)
    
  #  subset(zipdata,
  #        latitude >= latRng[1] & latitude <= latRng[2] &
  #          longitude >= lngRng[1] & longitude <= lngRng[2])
#  })
  
  ## Put our plots (Chloropleths?)
  
  # Precalculate the breaks we'll need for the two histograms
 # centileBreaks <- hist(plot = FALSE, allzips$centile, breaks = 20)$breaks
  
 # output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
  #  if (nrow(zipsInBounds()) == 0)
   #   return(NULL)
    
  #  hist(zipsInBounds()$centile,
   #      breaks = centileBreaks,
    #     main = "SuperZIP score (visible zips)",
     #    xlab = "Percentile",
      #   xlim = range(allzips$centile),
       #  col = '#00DD00',
        # border = 'white')
  # })
  
 # output$scatterCollegeIncome <- renderPlot({
    # If no zipcodes are in view, don't plot
  #  if (nrow(zipsInBounds()) == 0)
  #    return(NULL)
    
  #  print(xyplot(income ~ college, data = zipsInBounds(), xlim = range(allzips$college), ylim = range(allzips$income)))
  # })
  
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  
  # changing the color and size of the circles on top of the map
  # (that show the prevalence of the health outcomes)
  observe({
    colorBy <- input$color
    sizeBy <- input$sizes
    
    if (colorBy == "Year") {
      colorData <- d_outcomes[[colorBy]]
      pal <- colorBin("viridis", colorData, 7, pretty = FALSE)
    }
    
    if (sizeBy == "MeasureId") {
      # Radius is treated specially in the "MeasureId" case because
      # the values are categorical instead of continuous.
      radius <- ifelse(d_outcome$Data_Value >= (100 - input$threshold), 30000, 3000)
    } else {
      radius <- d_outcome[[sizeBy]] / max(d_outcome[[sizeBy]]) * 30000
    }
    
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
      sprintf("Measure: %s", selectedOutcome$Measure),
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
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Score >= input$minScore,
        Score <= input$maxScore,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df, outputId = "ziptable")
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
}
library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)
library(tidyverse)

df <- readRDS("d_new.RData")
d_circles <- df
df_plot <- df

function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet("map", d_circles) %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  #output$circles <- renderPlot({
  observe({
    input_outcome <- tolower(input$outcome)
    input_state <- tolower(input$state)
    

    
    d_circles <- d_circles %>% 
      filter(d_circles["measure_id_char"] == input_outcome) %>%
      #filter(d_circles["state_name"] == input_state)
    
    
    
    leafletProxy("map", data = d_circles) %>%
      clearShapes() %>%
      addTiles() %>%
      addCircles(lng = ~longitude, 
                 lat = ~latitude, 
                 stroke=FALSE, 
                 fillOpacity=0.4)
  })
  

  showOutcomePopup <- function(latitude, longitude) {
    
     input <- tolower(input$outcome)
     d_tags <- df %>%
       filter(df["measure_id_char"] == input)
     
       content <- as.character(tagList(
         tags$h4("(", latitude, ",", longitude, ")"), # want tags that will display the "data_value" (percent)
         tags$br(),
      sprintf("map")
    ))
      
    leafletProxy("map", data = d_tags) %>%
      addPopups(longitude, latitude, content)
  }

  observeEvent(input$map_shape_click, {
    
    click <- input$map_shape_click
    if(is.null(click)){
      return()
    }
    isolate({
      showOutcomePopup(click$lat, click$lng)
    })
  })
    
  
  
  ## State Predictor #########################################
    
  output$state_predictor <- renderLeaflet({
    leaflet("state_predictor", df) %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
    
  })

   # })

  ## Data Explorer ###########################################
  # Click the health outcome and display a graph showing counts of each outcome in each state.
  output$plot <- renderPlot({
    print('hi')
    df_plot %>%
      filter(df['measure_id_char'] == tolower(input$outcome)) %>%
      dplyr::count(df_plot, county.x) %>%
      df_plot$county.x <- as.factor(df_plot$county.x) %>%
      histogram(n)
  })
  
}


  
  

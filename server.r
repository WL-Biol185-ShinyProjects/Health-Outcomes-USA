library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)
library(tidyverse)

df <- readRDS("d_new.RData")
d_circles <- df

function(input, output, session) {
  
  ## TAB 1 Interactive US Map ###########################################
  # use "input$outcome1"
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet("map", d_circles) %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  #output$circles <- renderPlot({
  observe({
    input <- tolower(input$outcome1)
    
    # print(input)
    # print(nrow(d_circles["measure_id_char"]))
    
    d_circles <- d_circles %>% 
      filter(d_circles["measure_id_char"] == input)
    
    
    # print(nrow(d_circles["measure_id_char"]))
    # print(d_circles[1:10, "measure_id_char"])
    
    leafletProxy("map", data = d_circles) %>%
      clearShapes() %>%
      addTiles() %>%
      addCircles(lng = ~longitude, 
                 lat = ~latitude, 
                 stroke=FALSE, 
                 fillOpacity=0.4)
  })
  

  showOutcomePopup <- function(latitude, longitude) {
     input <- tolower(input$outcome1)
    d_popup <- df %>% 
      filter(df["measure_id_char"] == input)

    #content <- paste("(", latitude, ",", longitude, "),", ) - works with this tag
     
    content <- as.character(tagList(
      tags$h4("(", latitude, ",", longitude, ")"), # want tags that will display the "data_value" (percent)
      tags$br(),
      sprintf("map")
    ))
      
    leafletProxy("map", data = d_popup) %>%
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
    
  
  
  ## TAB 2 State Predictor #########################################
    #use "input$state2" and "input$predictor2"
  output$state_predictor <- renderLeaflet({
    leaflet("state_predictor", df) %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
    
  })

   # })

  ## TAB 3 Data Explorer ###########################################
  # use "input$outcome3" and "input$predictor3"
  # Click the health outcome and display a graph showing counts of each outcome in each state.
  output$plot1 <- renderPlot({
    
    outcome_input <- tolower(input$outcome3)
    predictor_input <- tolower(input$predictor3)
    
   # print(outcome_input)
    #print(predictor_input)
    
   # df_plot <- df
    
    #df_plot %>%
     # filter(measure_id_char == outcome_input)
    
   # ggplot(df_plot, aes(x = df_plot[,2], y = measure_id_char)) + geom_line()
   # df_plot %>%
    #  group_by(measure_id_char)
    
     # ggplot(df_plot) + geom_point(aes(x = pct_white))
    
    # .data[[input]]
    #print(df$pct_white)
   # print(predictor_input)
    #print(outcome_input) 
    
  })
  # x: socioeconomic
  # y: health outcome
  
  output$plot2 <- renderPlot({ # different outcomes by predictor
    #df_plot <- df
    #outcome_input <- tolower(input$outcome3)
    #predictor_input <- input$predictor3
    #print(outcome_input)
    #print(class(outcome_input))
    #print(predictor_input)
    
    #df_plot %>%
      #filter(df_plot["measure_id_char"] == outcome_input)
      #ggplot(aes(x = measure_id_char, y = df_plot$predictor_input)) + geom_boxplot()
    
    #ggplot(df_plot, aes(x = measure_id_char, y = med_inc)) + geom_boxplot()
  })
}


  
  



  

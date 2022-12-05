library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(shiny)

df <- read.csv("d_new.csv")
d_circles <- df

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
    input <- tolower(input$outcome)
    
    # print(input)
    
    # print(nrow(d_circles["measure_id_char"]))
    
    d_circles <- d_circles %>% 
      filter(d_circles["measure_id_char"] == input)
    
    
    #  print(nrow(d_circles["measure_id_char"]))
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
     input <- tolower(input$outcome)
    d_popup <- df %>% 
      filter(df["measure_id_char"] == input)

     content <- as.character(tagList(
      tags$h4("Percent:", d_popup$data_value),

  
  
  #  showOutcomePopup <- function(county, latitude, longitude) {
   # input <- tolower(input$outcome)
    #d_popup <- d_popup %>% 
  #  filter(d_circles["measure_id_char"] == input)
    
   # content <- as.character(tagList(
    #  tags$h4("Percent:", d_popup$data_value),

     # tags$strong(HTML(sprintf("%s, %s",
      #          d_popup$county_name, d_popup$state_abb
    #  ))),
     # tags$br(),
      #sprintf("Adult population: %s", d_popup$tot_pop), tags$br(),
      #sprintf("Year: %s", d_popup$year), tags$br(),
      #sprintf("Data Source: %s", d_circles$DataSource),
     ))
    
    #content <- "map"
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
    
   # observe({
     # leafletProxy("map") %>% clearPopups()
      #event <- input$map_shape_click
    #  if (is.null(event))
     #   return()
      
    #  isolate({
     #   showOutcomePopup(event$lat, event$lng)
      #})
  #  })
    
    
  output$state_predictor <- renderLeaflet({
    leaflet("state_predictor", df) %>%
      addTiles() %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
    
  })
  
  
  ## Data Explorer ###########################################
  # Click the health outcome and display a graph showing counts of each outcome in each state.
  


}
  

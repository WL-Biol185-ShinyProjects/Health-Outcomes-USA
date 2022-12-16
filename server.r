library(leaflet)
library(shiny)
library(tidyverse)
library(geojsonio)

df <- readRDS("d_new.RData")
d_circles <- df

stateCoordinates <- read.csv("stateCoords.csv")


function(input, output, session) {
  
  ## TAB 1 Interactive US Map ###########################################
  # use "input$outcome1"
  
  
  output$map <- renderLeaflet({
    leaflet("map", d_circles) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  observe({
    input_state1 <- input$state1 
    
    d_circles <- d_circles %>%
      filter(d_circles["state_name"] == input_state1)
    
    leafletProxy("map", data = d_circles) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = ((max(d_circles$longitude) + 
                        min(d_circles$longitude)) / 2),
              lat = ((max(d_circles$latitude) + 
                        min(d_circles$latitude)) / 2), zoom = 6)
    
    input_outcome1 <- input$outcome1 
    
    d_circles <- d_circles %>% 
      filter(d_circles["measure_id"] == input_outcome1) #%>%
    
    
    leafletProxy("map", data = d_circles) %>%
      clearShapes() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircles(lng = ~longitude, 
                 lat = ~latitude, 
                 stroke=FALSE, 
                 fillOpacity=0.4,
      )
    
  })
  
  
  ## TAB 2 Educational Attainment by County #########################################
  #use "input$state2" 
  
  #Output function for statewide 
  output$highschool_education <- renderLeaflet({
    
    selectedState <- input$state2
    
    #filtering data by educational attainment, grouping by county name
    map_df <- df %>%
      group_by(county_name, state_name) %>%
      summarize(percent_ed = median(higher_ed), na.rm =TRUE)
    
    #importing geo spatial data
    stateGeo <- geojson_read("states.geo.json", what = "sp")
    
    countyGeo <- geojson_read("counties.json", what = "sp")
    
    #adding state names to the counties table 
    stateNames <- as.character(stateGeo@data$NAME)
    names(stateNames) <- as.character(stateGeo@data$STATE)
    countyGeo@data$stateName <- stateNames[as.character(countyGeo@data$STATE)]
    
    #joining df with json data
    countyGeo@data <- left_join(countyGeo@data, map_df, by = c("stateName" = "state_name",
                                                               "NAME" = "county_name"))
    
    #select state specified by user
    statePolygon <- which(countyGeo@data$stateName != selectedState)
    stateLength <- length(statePolygon)
    counter <- 0
    
    #filter out polygons of other states
    for (i in 1:stateLength){
      countyGeo@polygons[[statePolygon[i]-counter]] <- NULL
      counter <- counter + 1
    }
    counter <- 0
    
    #grab state specific values for setview
    currentLong <- stateCoordinates$lon[stateCoordinates$state == selectedState]
    currentLat <- stateCoordinates$lat[stateCoordinates$state == selectedState]
    currentZoom <- stateCoordinates$zoom[stateCoordinates$state == selectedState]
    
    #prepping colors for chloropleth
    pal <- colorBin("RdYlGn", domain = countyGeo@data$percent_ed)
    
    #create leaflet visualization
    leaflet(countyGeo) %>%
      addPolygons(fillColor = ~pal(percent_ed),
                  weight = 1,
                  color = "white",
                  highlightOptions = highlightOptions(weight = 5),
                  label= ~percent_ed,
                  fillOpacity = 0.7) %>%
      
      setView(currentLong, currentLat, currentZoom) %>%
      
      addLegend("bottomright", pal = pal, values = ~percent_ed, title = "Percent 25 and Older with a Highschool Diploma/GED", 
                opacity = .7)
    
    
  })
  
  
  ## TAB 3 Median Income by County #########################################
  #use "input$state3" 
  
  #Output function for statewide 
  output$median_income <- renderLeaflet({
    
    selectedState <- input$state3
    
    #filtering data by educational attainment, grouping by county name
    map_df2 <- df %>%
      group_by(county_name, state_name) %>%
      summarize(avg_income = median(med_inc), na.rm =TRUE)
    
    #importing geo spatial data
    stateGeo <- geojson_read("states.geo.json", what = "sp")
    
    countyGeo <- geojson_read("counties.json", what = "sp")
    
    #adding state names to the counties table 
    stateNames <- as.character(stateGeo@data$NAME)
    names(stateNames) <- as.character(stateGeo@data$STATE)
    countyGeo@data$stateName <- stateNames[as.character(countyGeo@data$STATE)]
    
    #joining df with json data
    countyGeo@data <- left_join(countyGeo@data, map_df2, by = c("stateName" = "state_name",
                                                                "NAME" = "county_name"))
    
    #select state specified by user
    statePolygon <- which(countyGeo@data$stateName != selectedState)
    stateLength <- length(statePolygon)
    counter <- 0
    
    #filter out polygons of other states
    for (i in 1:stateLength){
      countyGeo@polygons[[statePolygon[i]-counter]] <- NULL
      counter <- counter + 1
    }
    counter <- 0
    
    #grab state specific values for setview
    currentLong <- stateCoordinates$lon[stateCoordinates$state == selectedState]
    currentLat <- stateCoordinates$lat[stateCoordinates$state == selectedState]
    currentZoom <- stateCoordinates$zoom[stateCoordinates$state == selectedState]
    
    #prepping colors for chloropleth
    pal <- colorBin("RdYlGn", domain = countyGeo@data$avg_income)
    
    #create leaflet visualization
    leaflet(countyGeo) %>%
      addPolygons(fillColor = ~pal(avg_income),
                  weight = 1,
                  color = "white",
                  highlightOptions = highlightOptions(weight = 5),
                  label= ~avg_income,
                  fillOpacity = 0.7) %>%
      setView(currentLong, currentLat, currentZoom) %>%
      addLegend("bottomright", pal = pal, values = ~avg_income, title = "Median Income", 
                opacity = .7)
    
    
    
    
  })
  
  
  
  ## TAB 4 Data Explorer ###########################################
  # use "input$outcome4" and "input$predictor4"
  # Click the health outcome and display a graph showing counts of each outcome in each state.
  
  
  output$plot4 <- renderPlot({ # different outcomes by predictor 
    outcome_input <- input$outcome4
    predictor_input <- input$predictor4
    #print(outcome_input)
    #print(predictor_input)
    
    df_plot <- df %>%
      select(measure_id, med_inc, higher_ed)
    
    if(predictor_input == "med_inc"){
      df_plot %>%
        filter(df_plot["measure_id"] == outcome_input) %>%
        ggplot(aes(x = measure_id, y = med_inc)) + 
        geom_boxplot()
    } else if(predictor_input == "higher_ed"){
      df_plot %>%
        filter(df_plot["measure_id"] == outcome_input) %>%
        ggplot(aes(x = measure_id, y = higher_ed)) + 
        geom_boxplot()
    } else{
      print("Choose an outcome and predictor")
    }
    
    #df_plot <- df %>%
    # select(measure_id, med_inc, higher_ed) %>%
    #filter(df["measure_id"] == outcome_input) %>%
    #gather(key = "predictor", value = "predictor_value", 2:3)
    #print(df_plot$predictor)
    #filter(df_plot["predictor"] == predictor_input) %>%
    #ggplot(df_plot, aes(x = measure_id, y = predictor_value)) + 
    #geom_boxplot() #+ 
    #labs(x = outcome_input, y = predictor_input)
    #ggplot(df_plot, aes(x = measure_id_char, y = med_inc)) + geom_boxplot() 
  })
}
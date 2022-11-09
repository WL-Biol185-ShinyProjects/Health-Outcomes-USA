library(leaflet)


# Sets up navbar/title
navbarPage("US Health Outcomes", id="nav",
           
              tabPanel("Interactive map", #first tab
                    div(class="outer",
                    # leafletOutput
                    
                    selectInput("outcome", "Outcome", vars),
                  
                    leafletOutput("map"), #set height to a number
              ),
              
            ),

# Making dragable drop down to make exploration selections
   #   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
    #    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
     #   width = 330, height = "auto",

      #  h2("Health explorer"),

      #  selectInput("color", "year", vars),
    #    selectInput("size", "Outcome", vars, selected = "adultpop"),
    #  ),

      
tabPanel("Interactive Map",
         selectInput("state", "State", states),
         selectInput("outcome", "Outcome", outcomes),
         selectInput("predictor", "Socioeconomic Predictor", predictors),
         leafletOutput("state_outcome"),
         leafletOutput("state_predictor"),
         ),

# making tab to select data exploration on top
tabPanel("Data explorer",
         selectInput("outcome", "Outcome", vars),
         selectInput("predictor", "Socioeconomic Predictor", predictors),
         plotOutput("plot"),
        )

   )

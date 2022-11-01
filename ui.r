library(leaflet)


# Sets up navbar/title
navbarPage("US Health Outcomes", id="nav",
           
              tabPanel("Interactive map", #first tab
                    div(class="outer",
                    # leafletOutput
                    leafletOutput("map"), #set height to a number
              ),

            ),

# Making dragable drop down to make exploration selections
   #   absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
    #    draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
     #   width = 330, height = "auto",

      #  h2("Health explorer"),

       # selectInput("color", "year", vars),
    #    selectInput("size", "Outcome", vars, selected = "adultpop"),
    #  ),

      
      
    

# making tab to select data exploration on top
tabPanel("Data explorer",
         fluidRow(
           actionButton("arthritis", "Arthritis"),
           actionButton("bp_high", "High Blood Pressure"), 
           actionButton("cancer", "Cancer"),
           actionButton("asthma", "Asthma"),
           actionButton("chd", "CHD"),
           actionButton("copd", "COPD"),
           actionButton("depression", "Depression"),
           actionButton("diabetes", "Diabetes"),
           actionButton("high_chol", "High Cholesterol"),
           actionButton("kidney", "Kidney"),
           actionButton("Obesity", "Obesity"),
           actionButton("Stroke", "Stroke"),
           actionButton("TeethLost", "TeethLost"),
           hr(),
           plotOutput("plot")
           ),
        )

   )

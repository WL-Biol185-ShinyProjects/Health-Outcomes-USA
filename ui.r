library(leaflet)

# Choices for drop-downs
# "Name of outcome" = "var_name"
vars <- c(
  "Arthritis" = "Arthritis",
  "BpHigh" = "BpHigh",
  "Cancer" = "Cancer",
  "Casthma" = "Casthma",
  "CHD" = "CHD",
  "COPD" = "COPD",
  "Depression" = "Depression",
  "Diabetes" = "Diabetes",
  "HighChol" = "HighChol",
  "Kidney" = "Kidney",
  "Obesity" = "Obesity",
  "Stroke" = "Stroke",
  "TeethLost" = "TeethLost",
  "Year" = "Year",
)

# Sets up navbar/title
navbarPage("US Health Outcomes", id="nav",
           
              tabPanel("Interactive map", #first tab
                    div(class="outer",
                    # leafletOutput
                    leafletOutput("map"), #set height to a number
              ),
           
              tabPanel("Data explorer", #second tab
              ),
            )
)
# Making dragable drop down to make exploration selections
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("Health explorer"),

        selectInput("color", "Year", vars),
        selectInput("size", "Outcome", vars, selected = "adultpop"),
      )

      
      
    

# making tab to select data exploration on top
tabPanel("Data explorer",
         fluidRow(
           column(3,
                  selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
           ),
           column(3,
                  conditionalPanel("input.states",
                                   selectInput("counties", "Counties", c("All counties"=""), multiple=TRUE)
                  )
           ),
        ),
)
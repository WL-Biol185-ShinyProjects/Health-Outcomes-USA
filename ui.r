library(leaflet)

# Choices for drop-downs
# "Name of outcome" = "var_name"
vars <- c(
  "Arthritis" = "var_name"
)


navbarPage("US Health Outcomes", id="nav",
           
              tabPanel("Interactive map",
                    div(class="outer",
                    # leafletOutput
                    leafletOutput("map"),
              ),
           
              tabPanel("Data explorer",
              ),
            )
)
# Making dragable drop down to make exploration selections
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",

        h2("ZIP explorer"),

        selectInput("color", "Color", vars),
        selectInput("size", "Size", vars, selected = "adultpop"),
        conditionalPanel("input.color == 'superzip' || input.size == 'superzip'",
          # Only prompt for threshold when coloring or sizing by superzip
          numericInput("threshold", "SuperZIP threshold (top n percentile)", 5)
        ),

        plotOutput("histCentile", height = 200),
        plotOutput("scatterCollegeIncome", height = 250)
      )

      
      
    

# making tab to select data exploration on top
tabPanel("Data explorer",
         fluidRow(
           column(3,
                  selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
           ),
           column(3,
                  conditionalPanel("input.states",
                                   selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
                  )
           ),
           column(3,
                  conditionalPanel("input.states",
                                   selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
                  )
           )
         ),
         fluidRow(
           column(1,
                  numericInput("minScore", "Min score", min=0, max=100, value=0)
           ),
           column(1,
                  numericInput("maxScore", "Max score", min=0, max=100, value=100)
           )
         ),
         hr(),
         DT::dataTableOutput("ziptable")
)
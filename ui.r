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
 #        fluidRow(
  #         column(2,
   #               selectInput("states", "States", c("All states"="", structure(state_abb, names=state_name), "Washington, DC"="DC"), multiple=TRUE)
    #       ),
     #      column(2,
      #            conditionalPanel("input_states",
       #                            selectInput("counties", "Counties", c("All counties"=""), multiple=TRUE)
        #          )
         #  ),
  #      ),

   )
)
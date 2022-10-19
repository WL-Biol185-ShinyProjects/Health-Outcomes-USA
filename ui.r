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
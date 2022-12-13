library(leaflet)

# Choices for drop-downs
outcomes <- c(
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
  "TeethLost" = "TeethLost"
)

states <- c(
  'Alabama',
  'Alaska',
  'Arizona',
  'Arkansas',
  'California',
  'Colorado',
  'Connecticut',
  'Delaware',
  'District of Columbia',
  'Florida',
  'Georgia',
  'Hawaii',
  'Idaho',
  'Illinois',
  'Indiana',
  'Iowa',
  'Kansas',
  'Kentucky',
  'Louisiana',
  'Maine',
  'Maryland',
  'Massachusetts',
  'Michigan',
  'Minnesota',
  'Mississippi',
  'Missouri',
  'Montana',
  'Nebraska',
  'Nevada',
  'New Hampshire',
  'New Jersey',
  'New Mexico',
  'New York',
  'North Carolina',
  'North Dakota',
  'Ohio',
  'Oklahoma',
  'Oregon',
  'Pennsylvania',
  'Rhode Island',
  'South Carolina',
  'South Dakota',
  'Tennessee',
  'Texas',
  'Utah',
  'Vermont',
  'Virginia',
  'Washington',
  'West Virginia',
  'Wisconsin',
  'Wyoming'
)

predictors <- c(
  'median income' = 'med_inc',
  'pct 25 and older with a highschool diploma/GED or higher' = 'higher_ed',
  'pct population white' = 'pct_white',
  'pct population black' = 'pct_black',
  'pct population native' = 'pct_native',
  'pct population asian' = 'pct_asian'
)

# Sets up navbar/title
navbarPage("US Health Outcomes", id="nav",
           
           tabPanel("Welcome",
                    div(class = "outer"),
                    "Welcome to the US Health Outcomes Explorer!",
                    ),
           
           tabPanel("Health Outcomes by State", #first tab
                    div(class="outer",
                    
                    selectInput("outcome", "Outcome", outcomes),
                    selectInput("state", "State", states),
                  
                    leafletOutput("map"),
                    #plotOutput("circles")
              ),
              
            ),

           tabPanel("Interactive State Map",
                    selectInput("state", "State", states),
                    selectInput("outcome", "Outcome", outcomes),
                    selectInput("predictor", "Socioeconomic Predictor", predictors),
                    leafletOutput("state_outcome"),
                    leafletOutput("state_predictor"),
            ),

# making tab to select data exploration on top
tabPanel("Data explorer",
         selectInput("outcome", "Outcome", outcomes),
         selectInput("predictor", "Socioeconomic Predictor", predictors),
         plotOutput("plot"),
        )

   )

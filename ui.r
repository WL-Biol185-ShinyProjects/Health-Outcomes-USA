library(leaflet)

# Choices for drop-downs
outcomes <- c(
  "BpHigh" = "BpHigh",
  "Depression" = "Depression",
  "Diabetes" = "Diabetes",
  "HighChol" = "HighChol",
  "Obesity" = "Obesity",
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
)

# Sets up navbar/title
navbarPage("US Health Outcomes", id="nav",
           tabPanel("Welcome",
                    div(class = "outer"),
                    tags$h2("Welcome to the US Health Outcomes Explorer!"),
                    
                    tags$h4("This health outcomes explorer provides an interactive visualization of health data
                            for thirteen different health outcomes and six socioeconomic predictors."),
                    
                    tags$h5("Health data provided by Centers for Disease Control and Prevention, 
                    National Center for Chronic Disease Prevention and Health Promotion, 
                    Division of Population Health, from the PLACES: Local Data for Better Health, 
                            Census Tract Data 2022 release."),
                    
                    tags$a(href = "https://chronicdata.cdc.gov/d/cwsq-ngmh?category=500-Cities-Places&view_name=PLACES-Local-Data-for-Better-Health-Census-Tract-D", 
                           "Link to CDC data"),
                    tags$h5("Socioeconomic predictor data provided by  *****."),
                    tags$a(href = "https://chronicdata.cdc.gov/d/cwsq-ngmh?category=500-Cities-Places&view_name=PLACES-Local-Data-for-Better-Health-Census-Tract-D", 
                           "Link to NH GIS data")
                    
           ),
           
           tabPanel("Health Outcomes by State", #Tab 1
                    div(class="outer",
                        
                        selectInput("outcome1", "Outcome", outcomes),
                        
                        leafletOutput("map"),
                        #plotOutput("circles")
                    ),
                    
           ),
           
           tabPanel("Educational Attainment by County", # Tab 2
                    selectInput("state2", "State", states),
                    leafletOutput("highschool_education"),
           ),
           
           tabPanel("Median Income by County", # Tab 3
                    selectInput("state3", "State", states),
                    leafletOutput("median_income"),
           ),
           tabPanel("Demographic vs. Outcomes", # Tab 4
                    selectInput("outcome4", "Outcome", outcomes),
                    selectInput("predictor4", "Socioeconomic Predictor", predictors),
                    plotOutput("plot4")
           )
           
)
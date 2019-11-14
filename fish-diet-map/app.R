
## Sample Cleaning script

# assign resused directory to a variable
dir <- "/Users/jaxinewolfe/Documents/RShiny/SERC-rshiny-example/fish-diet-map/"
# set working directory
setwd(dir)

# install and load libraries
# install.packages("htmltools")
# install.packages("leaflet")

# define R packages to require
libs <- c("tidyverse", "htmltools", "shiny", "leaflet")
# load libraries
lapply(libs, require, character.only = TRUE)


# Load in fish diet data
isotope <- read_csv("https://raw.githubusercontent.com/WHOIGit/nes-lter-fish-diet-isotope/master/fish-isotope/nes-lter-fish-stable-isotope-2013-2015.csv")

## RShiny App ----
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Map of Samples"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select the random distribution type ----
      selectInput("variable", "Cruise:",
                   c("Cruise 201302" = "201302",
                     "Cruise 201402" = "201402",
                     "Cruise 201404" = "201404",
                     "Cruise 201504" = "201504")),
      selectInput("fish", "Fish Species:",
                  c("All" = "All",
                    "Atlantic herring" = "Atlantic herring",
                    "Atlantic mackerel" = "Atlantic mackerel",
                    "Atlantic butterfish" = "Atlantic butterfish",
                    "Alewife" = "Alewife",
                    "Blueback herring" = "Blueback herring")),
      actionButton(inputId = "Run", "Run")
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output:
      leafletOutput(outputId = "mapPlot")
      )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  observeEvent(input$Run, {
    if (input$fish == "All") {
      df <- isotope %>%
        filter(cruise == input$variable) %>%
        rename(lat = decimalLatitude, lng = decimalLongitude) %>%
        select(lat, lng, vernacularName)
    } else {
      df <- isotope %>%
        filter(cruise == input$variable & vernacularName == input$fish) %>%
        rename(lat = decimalLatitude, lng = decimalLongitude) %>%
        select(lat, lng, vernacularName)
    }
    
    # Generate a plot of the data
  
    output$mapPlot <- renderLeaflet({
      # define palette 
      pal <- colorFactor(palette = "Set1", 
                         df$vernacularName)
      
      # construct map
      leaflet(df) %>%
        addProviderTiles(providers$Esri.OceanBasemap) %>%
        addCircles(color = ~pal(vernacularName)) %>%
        addLegend("bottomright", pal = pal, values = ~vernacularName, 
                  title = "Legend",
                  opacity = 1)
    })
  })
}


# run app
shinyApp(ui, server)

# runApp("my_app", display.mode = "showcase")
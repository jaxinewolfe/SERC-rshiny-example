
## Sample Cleaning script

# assign resused directory to a variable
dir <- "/Users/jaxinewolfe/Documents/RShiny/SERC-rshiny-example/"
# set working directory
setwd(dir)

# install and load libraries
# install.packages("htmltools")

# define R packages to require
libs <- c("tidyverse", "htmltools", "shiny", "maps")
# load libraries
lapply(libs, require, character.only = TRUE)


# Load in fish diet data
dietdata <- read_csv("https://raw.githubusercontent.com/WHOIGit/nes-lter-fish-diet-isotope/master/fish-diet/nes-lter-fish-diet-2013-2015.csv")

## Mapping sample locations
# nes <- map_data("state") %>% filter(long > -77)
# 
# # Justin's given coordinates
# ggplot() +
#   geom_polygon(data = nes, mapping = aes(x = long, y = lat, group = group),
#                fill = NA, color = "grey50") +
#   geom_point(diet_final, mapping = aes(x = Longitude, y = Latitude, color = Region),
#              size = 1) + 
#   coord_fixed(1.3) +
#   theme_classic()


## RShiny App ----
# Define UI for random distribution app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Prey Species Richness Per Cruise"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Select the random distribution type ----
      selectInput("variable", "Cruise:",
                   c("Cruise 201302" = "201302",
                     "Cruise 201402" = "201402",
                     "Cruise 201404" = "201404",
                     "Cruise 201504" = "201504"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output:
      plotOutput(outputId = "richnessPlot")
      )
  )
)

# Define server logic for random distribution app ----
server <- function(input, output) {
  
  # Generate a plot of the data
  output$richnessPlot <- renderPlot({
    
    # summarize prey richness
    richness <- dietdata %>%
      group_by(cruise, station, vernacularName) %>%
      summarise(totalCount = sum(preyCount),
                meanCount = mean(preyCount),
                # count unique prey items per cruise-station-fish
                preyRichness = length(unique(scientificName_preyTaxon)))
    
    ggplot(richness %>% filter(cruise == input$variable),
           aes(vernacularName, preyRichness, fill = vernacularName)) +
      geom_boxplot(alpha = 0.5) +
      labs(x = "Fish Species", y = "Prey Richness") +
      theme_classic() +
      theme(legend.position="none")
  })
}


# run app
shinyApp(ui, server)

# runApp("my_app", display.mode = "showcase")
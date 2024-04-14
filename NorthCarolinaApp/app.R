library(shiny)
library(leaflet)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("North Carolina Election Data"),
    leafletOutput("map"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("county", "Choose a County to View:",
                    choices = election_data$County,
                    selected = election_data$County[1]),
        
        sliderInput("year",
                    "Election Year:",
                    min = 2000,
                    max = 2024,
                    value = 4)
      ),

        mainPanel(
           plotOutput("distPlot")
        )
)
)

# Define server logic 
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() 
  })
  
  observe({
    county_picked <- election_data[election_data$County == input$county,]
    lat <- county_picked$Latitude
    long <- county_picked$Longitude
    
    leafletProxy("map") |>
      setView(lng = long, lat = lat, zoom = 8)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(leaflet)
library(rjson)
library(jsonlite)

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
                    value = 2020)
      ),

        mainPanel(
           plotOutput("distPlot") #Placeholder, not actually using a histogram
        )
)
)

#Change datatype to numeric
election_data$Lat <- as.numeric(as.character(election_data$Lat))
election_data$Long <- as.numeric(as.character(election_data$Long))
# Define server logic 
server <- function(input, output, session) {
  geojson_data <- reactive ({
    geojson <- readLines("data for app/nc_counties.geojson")
    geojson <- paste(geojson, collapse = "")
    fromJSON(geojson, flatten = TRUE)
  })
  
  observe({
    updateSelectInput(session, "county", choices = election_data$County)
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      setView(lng = -79.0, lat = 35.5, zoom = 10) |>
      addGeoJSON(geojson = geojson_data(), weight = 3, color = "black", fillColor = "blue")
  })
  
  observe({
    print(input$county)
    county_picked <- election_data[election_data$County == input$county, ]
      
    if(nrow(county_picked)> 0){
    lat <- county_picked$Lat
    long <- county_picked$Long
    selected_feature <- geojson_data()$features[which(geojson_data()$features$properties$NAME == input$county)]
    
    
    leafletProxy("map", session) |>
      clearShapes() |>
      addGeoJSON(geojson = selected_feature)
    } else {
    print ("No data found")
  }
})
}
# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)
library(leaflet)
library(rjson)
library(jsonlite)
library(scales)
library(RColorBrewer)





# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("North Carolina Election Data"),
    leafletOutput("map"),
    
    sidebarLayout(
      sidebarPanel(
        selectInput("county", "Choose a County to View:",
                    choices = election_data3$County,
                    selected = election_data3$County[1]),
        
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
election_data3$Lat <- as.numeric(as.character(election_data3$Lat.y))
election_data3$Long <- as.numeric(as.character(election_data3$Long.y))
# Define server logic 
server <- function(input, output, session) {
    geojson_data <- reactive({
    geojson_stuff <- readLines("data for app/nc_counties.geojson")
    geojson <- paste(geojson_stuff, collapse = "")
    geojson <- jsonlite::fromJSON(geojson, flatten = TRUE)
    if (!is.null(geojson) && !is.null(election_data3)) {
      geojson$features$properties$Votes_REP <- election_data3$Votes_REP[match(geojson$features$properties$NAME, election_data3$County)]
    } else {
      print("Geojson or election_data is null.")
    }
    geojson
})
  
  scaling_fill <- reactive({
    max_votes <- max(election_data3$Votes_REP, na.rm = TRUE)
    min_votes <- min(election_data3$Votes_REP, na.rm = TRUE)
    rescale(election_data3$Votes_REP, to = c(0,1), from = c(min_votes, max_votes))
  })
  
  colors_scaled <- colorRampPalette(c("lightpink", "red"))
  
  
  observe({
    updateSelectInput(session, "county", choices = election_data3$County)
  })
  
  output$map <- renderLeaflet({
    if (is.null(geojson_data())) return(NULL)  # Prevent leaflet from rendering if geojson_data is NULL
    leaflet() %>%
      addTiles() %>%
      setView(lng = -79.0, lat = 35.5, zoom = 8) %>%
      addGeoJSON(
        geojson = geojson_data(),
        weight = 3,
        color = "black",
        fillColor = ~colors_scaled(100)[as.integer(scaling_fill() * 100)]
      )
  })
  
  observe({
    print(input$county)
    county_picked <- election_data3[election_data2$County == input$county, ]
      
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
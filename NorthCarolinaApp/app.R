library(shiny)
library(leaflet)
library(rjson)
library(jsonlite)
library(scales)
library(RColorBrewer)
library(readr)
library(tidyverse)

#Load Data
election_data_combined <- read_csv("NorthCarolinaApp/data for app/election_data_wide_geo.csv")
counties_geojson <- read_json("NorthCarolinaApp/data for app/nc_counties.geojson")

#Change datatype to numeric
election_data_combined$Lat <- as.numeric(as.character(election_data_combined$Lat))
election_data_combined$Long <- as.numeric(as.character(election_data_combined$Long))

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("North Carolina Election Data"),
    leafletOutput("map"),
    sidebarLayout(
      sidebarPanel(
        selectInput("county", "Choose a County to View:",
                    choices = election_data_combined$County,
                    selected = election_data_combined$County[1]),
        sliderInput("year",
                    "Election Year:",
                    min = 2000,
                    max = 2024,
                    value = 2020),
    
      selectInput("political", "Choose a Political Party: ",
                  choices = election_data$Choice_Party,
                  selected = election_data$Choice_Party[1])
      ),

        mainPanel(
           plotOutput("distPlot") #Placeholder, not actually using a histogram
        )
)
)

# Define server logic 
server <- function(input, output, session) {
  #Base Map in the Beginning
  output$map <- renderLeaflet({
    pal_colors <- colorQuantile("Purples", election_data_combined$Votes_REP, n = 5)
    leaflet(data = counties_geojson) |>
      addTiles() |>
      addPolygons(
        fillColor = ~pal_colors(Votes_REP),
        fillOpacity = 0.5,
        color = "black",
        weight = 2,
       popup = ~paste("Republican Votes: ", Votes_REP),
        label = ~paste("Republican Votes: ", Votes_REP)
      ) |>
      setView(lng = -79.0, lat =35.5, zoom =7)
  })
  observe({
    req(election_data_combined) #checking if null
    selected_county <- filter(election_data_combined, County == input$county)
  
  if(nrow(selected_county) > 0)
  output$map <- renderLeaflet({
    pal_colors <- colorNumeric("Purples", domain = election_data_combined$Votes_REP)
    leaflet(data = selected_county) |>
    addTiles() |> 
      addCircleMarkers(
        lng = ~Long,
        lat = ~Lat,
        radius = 8,
        color = ~pal_colors(Votes_REP),
        fillOpacity = 0.5,
        popup = ~paste("Republican Votes: ", Votes_REP)
      ) |>
      setView(lng = selected_county$Long, lat = selected_county$Lat, zoom = 8)
  })
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
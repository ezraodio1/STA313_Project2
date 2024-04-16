library(shiny)
library(leaflet)
library(rjson)
library(jsonlite)
library(scales)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(sf)


#Load Data
election_data_combined <- read_csv("data for app/election_data_wide_geo.csv")
counties_geo <- st_read("data for app/nc_counties.geojson")

counties_geo <- counties_geo |>
mutate(County = str_to_upper(County))

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
                  choices = c("Rep" = "Rep", "Dem" = "Dem"),
                  selected = "Rep")
      ),

        mainPanel(
           plotOutput("distPlot") #Placeholder, not actually using a histogram
        )
)
)


server <- function(input, output, session) {
  picking_political <- reactive({
    if (input$political == "Rep") {
      votes <- election_data_combined$Votes_REP
      domain <- na.omit(votes)
      if (length(domain) == 0) domain <- c(0, 1)  
      list(votes = votes, palette = colorQuantile("Reds", domain, n = 5))
    } else {
      votes <- election_data_combined$Votes_DEM
      domain <- na.omit(votes)
      if (length(domain) == 0) domain <- c(0, 1) 
      list(votes = votes, palette = colorQuantile("Blues", domain, n = 5))
    }
  })
  
  #Base Map in the Beginning
  output$map <- renderLeaflet({
    #matching counties for dem and rep
    matched_index <- match(counties_geo$County, election_data_combined$County)
    matched_numbervotes <- picking_political()$votes[matched_index]
    
    leaflet(data = counties_geo) |>
      addTiles() |>
      addPolygons(
        fillColor = ~picking_political()$palette(matched_numbervotes),
        fillOpacity = 0.8,
        color = "black",
        weight = 2,
       popup = ~paste(County, "<br>", input$political, "Votes: ", matched_numbervotes)
      ) |>
      setView(lng = -79.0, lat =35.5, zoom =7)
  })
  
  #observe({
  #  selected_county <- counties_geo[counties_geo$County == input$county]
  #  county_centroid <- st_centroid(st_geometry(selected_county))
    
    # Pulling the long/lat
  #  lng <- st_coordinates(county_centroid)[1, 1]
  #  lat <- st_coordinates(county_centroid)[1, 2]
    
    # Use leafletProxy to update the existing leaflet map
  #  leafletProxy("map", data = selected_county) %>%
  #   setView(lng = lng, lat = lat, zoom = 10)
#})
}
# Run the application 
shinyApp(ui = ui, server = server)
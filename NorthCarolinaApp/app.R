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
                  choices = c("None" = "", election_data_combined$County),
                  selected = ""),
      #EZRA: Got rid of slider - it seemed redundant
      # sliderInput("year",
      #             "Election Year:",
      #             min = 2000,
      #             max = 2024,
      #             value = 2020),
      
      selectInput("political", "Choose a Political Party: ",
                  choices = c("Rep" = "Rep", "Dem" = "Dem"),
                  selected = "Rep"),
      selectInput("year",
                  "Election Year:",
                  choices = c("2000", "2004", "2008", "2012", "2016", "2020"),
                  selected = "2020")
    ),
    
    
    mainPanel(
      plotOutput("distPlot") #Placeholder, not actually using a histogram
    )
  )
)


server <- function(input, output, session) {
  picking_political <- reactive({
    election_data_for_year <- election_data_combined |>
      filter(Year == as.numeric(input$year))
    
    if (input$political == "Rep") {
      election_data_for_year <- election_data_for_year |>
        select(County, Votes_REP)
      names(election_data_for_year)[names(election_data_for_year) == "Votes_REP"] <- "Votes"
    } else {
      election_data_for_year <- election_data_for_year |>
        select(County, Votes_DEM)
      names(election_data_for_year)[names(election_data_for_year) == "Votes_DEM"] <- "Votes"
    }
    
    election_data_for_year
  })
  
  output$map <- renderLeaflet({
    leaflet(data = counties_geo) |>
      addTiles() |>
      setView(lng = -79.0, lat =35.5, zoom = 7)
  })
  
  observe({
    df <- picking_political()
    
    matched_index <- match(counties_geo$County, df$County)
    matched_votes <- df$Votes[matched_index]
    selected_county <- input$county
    
    palette <- colorQuantile(if (input$political == "Rep") "Reds" else "Blues", na.omit(matched_votes), n = 5)
    
    leafletProxy("map", data = counties_geo) |>
      clearShapes() |>
      addPolygons(
        fillColor = ~palette(matched_votes),
        fillOpacity = ~ifelse(County == selected_county, 1, 0.5),
        color = ~ifelse(County == selected_county, "black", "gray"),
        opacity = 1,
        smoothFactor = 0,
        weight = ~ifelse(County == selected_county, 3, 1),
        popup = ~paste(County, "<br>", input$political, "Votes: ", matched_votes)
      )
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
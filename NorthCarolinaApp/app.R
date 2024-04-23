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

election_data_combined 

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
 
  combining_rep_dem <- reactive({
    election_data_combined |>
      filter(Year == as.numeric(input$year)) |>
      select(County, rep_per_capita, dem_per_capita, Votes_REP, Votes_DEM) |>
      mutate(politicalparty = (dem_per_capita - rep_per_capita) / (dem_per_capita + rep_per_capita))
  })
    
  output$map <- renderLeaflet({
    leaflet(data = counties_geo) |>
      addTiles() |>
      setView(lng = -79.0, lat =35.5, zoom = 7)
  })
  
  observe({
    df <- combining_rep_dem()
    
    matched_index <- match(counties_geo$County, df$County)
    matched_political <- df$politicalparty[matched_index]
    selected_county <- input$county
    matched_votes_rep <- df$Votes_REP[matched_index]
    matched_votes_dem <- df$Votes_DEM[matched_index]
    
    palette <- colorNumeric(c("red", "blue"), domain = range(matched_political, na.rm = TRUE))
    leafletProxy("map", data = counties_geo) |>
      clearShapes() |>
      addPolygons(
        fillColor = ~palette(matched_political),
        fillOpacity = ~ifelse(County == selected_county, 1, 0.7),
        color = ~ifelse(County == selected_county, "black", "gray"),
        opacity = 1,
        smoothFactor = 0,
        weight = ~ifelse(County == selected_county, 3, 1),
        popup = ~paste(County, "<br>", input$political,
                       "Republican Votes: ", matched_votes_rep, "<br>",
                       "Democrat Votes: ", matched_votes_dem, "<br>")
      )
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
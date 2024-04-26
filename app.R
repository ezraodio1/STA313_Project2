#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(rjson)
library(jsonlite)
library(scales)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(sf)

# load data --------------------------------------------------------------------

election_data_combined <- read_csv("data/election_data_wide_geo.csv")
counties_geo <- st_read("data/nc_counties.geojson")

counties_geo <- counties_geo |>
  mutate(County = str_to_upper(County))

election_data_combined <- election_data_combined |>
  mutate(County = str_to_upper(County))


# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("North Carolina Election Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("electionCounty", "Choose a County to View:",
        choices = c("None" = "", election_data_combined$County),
        selected = ""
      ),
      selectInput("year",
        "Election Year:",
        choices = c("2000", "2004", "2008", "2012", "2016", "2020"),
        selected = "2020"
      ),
      width = 3
    ),
    mainPanel(
      leafletOutput("map_election"),
      width = 9
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  combining_rep_dem <- reactive({
    election_data_combined |>
      filter(Year == as.numeric(input$year)) |>
      select(County, rep_per_capita, dem_per_capita, Votes_REP, Votes_DEM) |>
      mutate(politicalparty = Votes_REP / (Votes_REP + Votes_DEM))
  })

  combined_data <- reactive({
    merge(counties_geo, combining_rep_dem(), by = "County")
  })

  palette <- colorNumeric(c("blue", "white", "red"), domain = c(0, 1))

  # Render map with selected year
  output$map_election <- renderLeaflet({
    leaflet(data = combined_data()) |>
      addTiles() |>
      setView(lng = -79.0, lat = 35.5, zoom = 7) |>
      addPolygons(
        fillColor = ~ palette(politicalparty),
        fillOpacity = 0.5, # Set default fill opacity
        color = "gray",
        popup = ~ paste(
          "County: ", County, "<br>",
          "Republican Votes: ", Votes_REP, "<br>",
          "Democrat Votes: ", Votes_DEM, "<br>"
        ),
        layerId = ~County # Important: Assign a unique layer ID for each county
      )
  })

  # Keep track of county that was previously selected
  last_selected <- reactiveVal(NULL)

  # Highlight selected county on map
  observeEvent(input$electionCounty, {
    new_selection <- input$electionCounty
    old_selection <- last_selected()

    # Ensure selections are valid before proceeding
    if (!is.null(old_selection) && old_selection != "" && any(combined_data()$County == old_selection)) {
      # Reset the previously selected county to its original state
      leafletProxy("map_election") |>
        removeShape(layerId = old_selection) |>
        addPolygons(
          data = combined_data() |> filter(County == old_selection),
          fillColor = ~ palette(politicalparty),
          fillOpacity = 0.5,
          color = "gray",
          weight = 1,
          popup = ~ paste(
            "County: ", County, "<br>",
            "Republican Votes: ", Votes_REP, "<br>",
            "Democrat Votes: ", Votes_DEM, "<br>"
          ),
          layerId = ~County
        )
    }

    if (new_selection != "" && any(combined_data()$County == new_selection)) {
      # Apply new highlight to the selected county
      leafletProxy("map_election") |>
        addPolygons(
          data = combined_data() |> filter(County == new_selection),
          fillColor = "green",
          fillOpacity = 1,
          color = "black",
          weight = 3,
          popup = ~ paste(
            "Highlighted County: ", County, "<br>",
            "Republican Votes: ", Votes_REP, "<br>",
            "Democrat Votes: ", Votes_DEM, "<br>"
          ),
          layerId = ~County
        )
    }

    last_selected(new_selection) # Update the last selected county
  })



  observe({
    df <- combining_rep_dem()
    matched_political <- df$politicalparty

    palette <- colorNumeric(c("red", "white", "blue"), domain = range(matched_political, na.rm = TRUE))
    leafletProxy("map_election", data = counties_geo) |>
      addLegend("bottomright",
        pal = palette, values = ~matched_political,
        title = "Republican to Democrat Scale Per Capita",
        opacity = 0.7
      )
  })
}


# Run the application
shinyApp(ui = ui, server = server)

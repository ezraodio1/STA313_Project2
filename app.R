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

election_data_combined
glimpse(counties_geo)

counties_geo <- counties_geo |>
  mutate(County = str_to_upper(County))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
  titlePanel("North Carolina Election Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("county", "Choose a County to View:",
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
      leafletOutput("map"),
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
      mutate(politicalparty = Votes_REP/(Votes_REP + Votes_DEM))
  })
  
  palette <- colorNumeric(c("blue", "white", "red"), domain = c(0, 1))
  # render map without selected county, just selected year
  output$map <- renderLeaflet({
    
    
    leaflet(data = counties_geo) |>
      addTiles() |>
      setView(lng = -79.0, lat = 35.5, zoom = 7) |>
      addPolygons(
        fillColor = ~palette(combining_rep_dem()$politicalparty),
        fillOpacity = 0.9,
        color = "gray",
        popup = ~ paste(
          County, "<br>",
          "Republican Votes: ", combining_rep_dem()$Votes_REP, "<br>",
          "Democrat Votes: ", combining_rep_dem()$Votes_DEM, "<br>"
        )
      )
  })
  
  last_selected <- reactiveVal(NULL)
  
  observeEvent(input$county, {
    new_selection <- input$county
    old_selection <- last_selected()
    last_selected(new_selection)  # Update the last selected county
    
    df <- combining_rep_dem()
    
    # Update the previously selected county to remove the highlight
    if (!is.null(old_selection) && old_selection != "") {
      leafletProxy("map") |>
        addPolygons(
          data = counties_geo[counties_geo$County == old_selection, ],
          fillColor = palette(df$politicalparty[match(old_selection, df$County)]),
          fillOpacity = 0.9,
          color = "gray",
          popup = ~ paste(
            "County: ", County, "<br>",
            "Republican Votes: ", df$Votes_REP[match(County, df$County)], "<br>",
            "Democrat Votes: ", df$Votes_DEM[match(County, df$County)], "<br>"
          )
        )
    }
    
    # Apply new highlight to the selected county
    if (new_selection != "") {
      leafletProxy("map") |>
        addPolygons(
          data = counties_geo[counties_geo$County == new_selection, ],
          fillColor = "green",
          fillOpacity = 1,
          color = "black",
          weight = 3,
          popup = ~paste(
            "Highlighted County: ", County, "<br>",
            "Republican Votes: ", df$Votes_REP[match(County, df$County)], "<br>",
            "Democrat Votes: ", df$Votes_DEM[match(County, df$County)], "<br>"
          )
        )
    }
  })
  observe({
    df <- combining_rep_dem()
    matched_political <- df$politicalparty
    
    palette <- colorNumeric(c("red", "white", "blue"), domain = range(matched_political, na.rm = TRUE))
    leafletProxy("map", data = counties_geo) |>
      addLegend("bottomright",
                pal = palette, values = ~matched_political,
                title = "Republican to Democrat Scale Per Capita",
                opacity = 0.7
      )
  })
}
  

# Run the application 
shinyApp(ui = ui, server = server)

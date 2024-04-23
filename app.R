# load packages ----------------------------------------------------------------

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

# Change datatype to numeric - MCR: PROBABLY DON'T NEED
# election_data_combined$Lat <- as.numeric(as.character(election_data_combined$Lat))
# election_data_combined$Long <- as.numeric(as.character(election_data_combined$Long))

# UI ---------------------------------------------------------------------------

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

# server -----------------------------------------------------------------------

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

  # observe({
  #   df <- combining_rep_dem()
  # 
  #   matched_index <- match(counties_geo$County, df$County)
  #   matched_political <- df$politicalparty[matched_index]
  #   selected_county <- input$county
  #   matched_votes_rep <- df$Votes_REP[matched_index]
  #   matched_votes_dem <- df$Votes_DEM[matched_index]
  # 
  #   # Colorblind friendly colors: https://davidmathlogic.com/colorblind/#%23D81B60-%231E88E5-%23FFC107-%23004D40
  #   # Do we need to make this colorblind friendly? Hard to see the difference between red and blue if so
  #   leafletProxy("map", data = counties_geo |> filter("County" == selected_county)) |>
  #     addPolygons(
  #       fillColor = "green"
  #     )
  #   
  #   # palette <- colorNumeric(c("red", "white", "blue"), domain = range(matched_political, na.rm = TRUE))
  #   # leafletProxy("map", data = counties_geo) |>
  #   #   clearShapes() |>
  #   #   addPolygons(
  #   #     fillColor = ~ palette(matched_political),
  #   #     fillOpacity = ~ ifelse(County == selected_county, 1, 0.5),
  #   #     color = ~ ifelse(County == selected_county, "black", "gray"),
  #   #     opacity = 1,
  #   #     smoothFactor = 0,
  #   #     weight = ~ ifelse(County == selected_county, 3, 1),
  #   #     popup = ~ paste(
  #   #       County, "<br>", input$political,
  #   #       "Republican Votes: ", matched_votes_rep, "<br>",
  #   #       "Democrat Votes: ", matched_votes_dem, "<br>"
  #   #     )
  #   #   )
  # })

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

# run app ----------------------------------------------------------------------

shinyApp(ui = ui, server = server)

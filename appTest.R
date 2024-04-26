library(shiny)
library(leaflet)
library(rjson)
library(jsonlite)
library(scales)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(sf)


# Load Data ---------------------------------------------------------------------
election_data_combined <- read_csv("data/election_data_wide_geo.csv",
  show_col_types = FALSE
)
counties_geo <- st_read("data/nc_counties.geojson")

counties_geo <- counties_geo |>
  mutate(County = str_to_upper(County))

ACS2000_cleaned <- read_csv("data/ACS2000_cleaned.csv", show_col_types = FALSE)
ACS2004_cleaned <- read_csv("data/ACS2004_cleaned.csv", show_col_types = FALSE)
ACS2008_cleaned <- read_csv("data/ACS2008_cleaned.csv", show_col_types = FALSE)
ACS2012_cleaned <- read_csv("data/ACS2012_cleaned.csv", show_col_types = FALSE)
ACS2016_cleaned <- read_csv("data/ACS2016_cleaned.csv", show_col_types = FALSE)
ACS2020_cleaned <- read_csv("data/ACS2020_cleaned.csv", show_col_types = FALSE)

ACS_all <- rbind(
  ACS2000_cleaned,
  ACS2004_cleaned,
  ACS2008_cleaned,
  ACS2012_cleaned,
  ACS2016_cleaned,
  ACS2020_cleaned
)

ACS_all_pivot <- pivot_longer(
  ACS_all,
  cols = starts_with("Age"),
  names_to = "Age_Category",
  values_to = "Count"
) |>
  separate(
    col = geo_point_2d,
    into = c("Lat", "Long"),
    sep = ", "
  )

ACS <- ACS_all_pivot |>
  select(fips, County, Year, Race, Sex, Lat, Long, Age_Category, Count)

ACS$Lat <- as.numeric(as.character(ACS$Lat))
ACS$Long <- as.numeric(as.character(ACS$Long))

# Define UI for application ----------------------------------------------------
ui <- fluidPage(
  titlePanel("North Carolina Election Data"),
  sidebarLayout(
    sidebarPanel(
      h4("Election Filters"),
      selectInput("electionCounty", "Choose a County to View:",
        choices = c("None" = "", sort(unique(election_data_combined$County))),
        selected = ""
      ),
      selectInput("political", "Choose a Political Party:",
        choices = c("Rep" = "Rep", "Dem" = "Dem"),
        selected = "Rep"
      ),
      selectInput("year",
        "Election Year:",
        choices = c("2000", "2004", "2008", "2012", "2016", "2020"),
        selected = "2020"
      ),
      hr(),
      h4("ACS Filters"),
      selectInput("ACScounty", "Choose a County to View:",
        choices = c("None" = "", sort(unique(ACS$County))),
        selected = ""
      ),
      selectInput("Sex", "Sex:", choices = c("All", sort(unique(ACS$Sex)))),
      selectInput("Race", "Race:", choices = c("All", sort(unique(ACS$Race)))),
      selectInput("Age_Category", "Age Category:",
        choices = c("All", sort(unique(ACS$Age_Category)))
      )
    ),
    mainPanel(
      leafletOutput("map_election"),
      leafletOutput("map2")
    )
  )
)

# define server logic ----------------------------------------------------------
server <- function(input, output, session) {
  # MAP 1: Election data -------------------------------------------------------

  # calculate political orientation of each county based on selected year
  combining_rep_dem <- reactive({
    election_data_combined |>
      filter(Year == as.numeric(input$year)) |>
      select(County, Votes_REP, Votes_DEM) |>
      mutate(politicalparty = Votes_REP / (Votes_REP + Votes_DEM))
  })

  # combine political orientation with geographic data
  combined_data <- reactive({
    merge(counties_geo, combining_rep_dem(), by = "County")
  })

  # create palette to color counties by political orientation
  palette <- colorNumeric(c("blue", "white", "red"), domain = c(0, 1))

  # render map based on selected year
  output$map_election <- renderLeaflet({
    leaflet(data = combined_data()) |>
      addTiles() |>
      setView(lng = -79.0, lat = 35.5, zoom = 7) |>
      addPolygons(
        fillColor = ~ palette(politicalparty),
        fillOpacity = 0.43,
        color = "gray",
        popup = ~ paste(
          "County: ", County, "<br>",
          "Republican Votes: ", Votes_REP, "<br>",
          "Democrat Votes: ", Votes_DEM, "<br>"
        ),
        layerId = ~County
      )
  })

  # keep track of county that was previously selected
  last_selected <- reactiveVal(NULL)

  # listen for changes to selected county
  observeEvent(input$electionCounty, {
    new_selection <- input$electionCounty
    old_selection <- last_selected()

    # reset previously selected county if applicable
    if (!is.null(old_selection) && old_selection != "" &&
      any(combined_data()$County == old_selection)) {
      leafletProxy("map_election") |>
        removeShape(layerId = old_selection) |>
        addPolygons(
          data = combined_data() |> filter(County == old_selection),
          fillColor = ~ palette(politicalparty),
          fillOpacity = 0.43,
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
  
    # highlight selected county if valid
    if (new_selection != "" && any(combined_data()$County == new_selection)) {
      leafletProxy("map_election") |>
        addPolygons(
          data = combined_data() |> filter(County == new_selection),
          fillColor = ~ palette(politicalparty),
          fillOpacity = 1,
          color = "black",
          weight = 6,
          popup = ~ paste(
            "Highlighted County: ", County, "<br>",
            "Republican Votes: ", Votes_REP, "<br>",
            "Democrat Votes: ", Votes_DEM, "<br>"
          ),
          layerId = ~County
        )
    }

    last_selected(new_selection)
  })

  # MAP 2: ACS Data ------------------------------------------------------------
  output$map2 <- renderLeaflet({
    leaflet(data = counties_geo) |>
      addTiles() |>
      setView(lng = -79.0, lat = 35.5, zoom = 7)
  })

  picking_political <- reactive({
    election_data_for_year <- election_data_combined |>
      filter(Year == as.numeric(input$year))

    if (input$political == "Rep") {
      election_data_for_year <- election_data_for_year |>
        select(County, Votes_REP)
      names(election_data_for_year)[names(election_data_for_year)
      == "Votes_REP"] <- "Votes"
    } else {
      election_data_for_year <- election_data_for_year |>
        select(County, Votes_DEM)
      names(election_data_for_year)[names(election_data_for_year)
      == "Votes_DEM"] <- "Votes"
    }

    election_data_for_year
  })


  observe({
    df <- picking_political()


    matched_index_election <- match(counties_geo$County, df$County)
    matched_votes_election <- df$Votes[matched_index_election]
    selected_county_election <- input$electionCounty

    palette <- colorQuantile(if (input$political == "Rep") "Reds" else "Blues",
      na.omit(matched_votes_election),
      n = 5
    )

    leafletProxy("map1", data = counties_geo) |>
      clearShapes() |>
      addPolygons(
        fillColor = ~ palette(matched_votes_election),
        fillOpacity = ~ ifelse(County == selected_county_election, 1, 0.5),
        color = ~ ifelse(County == selected_county_election, "black", "gray"),
        opacity = 1,
        smoothFactor = 0,
        weight = ~ ifelse(County == selected_county_election, 3, 1),
        popup = ~ paste(
          County, "<br>", input$political, "Votes: ",
          matched_votes_election
        )
      )
  })


  ACS_filtered <- reactive({
    ACS_year <- ACS |>
      filter(Year == as.numeric(input$year))

    if (input$Sex != "All") {
      ACS_year <- ACS_year |>
        filter(Sex == input$Sex)
    }

    if (input$Race != "All") {
      ACS_year <- ACS_year |>
        filter(Race == input$Race)
    }

    if (input$Age_Category != "All") {
      ACS_year <- ACS_year |>
        filter(Age_Category == input$Age_Category)
    }

    ACS_year
  })


  observe({
    df <- ACS_filtered()

    matched_index_ACS <- match(counties_geo$County, df$County)
    matched_votes_ACS <- df$Votes[matched_index_ACS]
    selected_county_ACS <- input$ACScounty

    palette1 <- colorQuantile("Blues", na.omit(df$Count), n = 5)

    leafletProxy("map2", data = counties_geo) |>
      clearShapes() |>
      addPolygons(
        fillColor = ~ palette1(df$Count[matched_index_ACS]),
        fillOpacity = ~ ifelse(County == selected_county_ACS, 1, 0.5),
        color = ~ ifelse(County == selected_county_ACS, "black", "gray"),
        opacity = 1,
        smoothFactor = 0,
        weight = ~ ifelse(County == selected_county_ACS, 3, 1),
        # popup = ~paste(County, "<br>", "Population: ", Count)
      )
  })
}

shinyApp(ui = ui, server = server)

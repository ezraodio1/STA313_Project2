library(shiny)
library(leaflet)
library(rjson)
library(jsonlite)
library(scales)
library(RColorBrewer)
library(readr)
library(tidyverse)
library(sf)


# Load Data --------------------------------------------------------------------
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
  select(County, Year, Race, Sex, Lat, Long, Age_Category, Count) |>
  mutate(County = toupper(County))

county_pops <- ACS |>
  group_by(County, Year) |>
  reframe(countyPop = sum(Count, na.rm = TRUE))

ACS <- merge(ACS, county_pops, by = c("County", "Year"))

ACS$Lat <- as.numeric(as.character(ACS$Lat))
ACS$Long <- as.numeric(as.character(ACS$Long))

# Define UI for application ----------------------------------------------------
ui <- fluidPage(
  #titlePanel("North Carolina Election Data"),
  sidebarLayout(
    sidebarPanel(
      h4("Election Filters"),
      selectInput("electionCounty", "Choose a County to View:",
                  choices = c("None" = "", sort(unique(election_data_combined$County))),
                  selected = ""
      ),
      # selectInput("political", "Choose a Political Party:",
      #             choices = c("Rep" = "Rep", "Dem" = "Dem"),
      #             selected = "Rep"
      # ),
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
      selectInput("Sex", "Sex:", 
                  choices = c("All", sort(unique(ACS$Sex))),
                  selected = "All"
      ),
      selectInput("Race", "Race:", 
                  choices = c("All", sort(unique(ACS$Race))),
                  selected = "All"
      ),
      selectInput("Age_Category", "Age Category:",
                  choices = c("All", sort(unique(ACS$Age_Category))),
                  selected = "All"
      )
    ),
    mainPanel(
      h4("Election Data Map", style = "text-align: center;"),
      leafletOutput("map_election", height = "350px"),
      #div(style = "margin-top: 20px;"),
      h4("ACS Data Map", style = "text-align: center;"),
      leafletOutput("map_ACS", height = "350px")
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
  
  # filter ACS data based on selected filters
  filter_pops <- reactive({
    data <- ACS |>
      filter(Year == as.numeric(input$year))
    
    if (!is.null(input$Race) && input$Race != "All") {
      data <- data |>
        filter(Race == input$Race)
    }
    
    if (!is.null(input$Sex) &&input$Sex != "All") {
      data <- data |>
        filter(Sex == input$Sex)
    }
    
    if (!is.null(input$Age_Category) &&input$Age_Category != "All") {
      data <- data |>
        filter(Age_Category == input$Age_Category)
    }
    
    data <- data |>
      group_by(County) |>
      reframe(popProp = sum(Count, na.rm = TRUE) / first(countyPop))
    
    data <- merge(counties_geo, data, by = "County", all.x = TRUE)
    
    data$popProp[is.na(data$popProp)] <- 0
    
    data
  })
  
  # output ACS map based on selected filters
  output$map_ACS <- renderLeaflet({
    pop_palette <- colorNumeric("Greens", domain = filter_pops()$popProp)
    
    leaflet(data = filter_pops()) |>
      addTiles() |>
      setView(lng = -79.0, lat = 35.5, zoom = 7) |>
      addPolygons(
        fillColor = ~ pop_palette(popProp),
        fillOpacity = 1, 
        color = "gray"
      )
  })
}

shinyApp(ui = ui, server = server)

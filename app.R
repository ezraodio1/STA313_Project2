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

ACS_all_long <- pivot_longer(
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

ACS <- ACS_all_long |>
  select(County, Year, Race, Sex, Lat, Long, Age_Category, Count) |>
  mutate(
    County = toupper(County),
    Lat = as.numeric(Lat),
    Long = as.numeric(Long)
  )

county_pops <- ACS |>
  group_by(County, Year) |>
  reframe(countyPop = sum(Count, na.rm = TRUE))

ACS <- merge(ACS, county_pops, by = c("County", "Year"))

# Define UI for application ----------------------------------------------------
ui <- fluidPage(
  # titlePanel("North Carolina Election Data"),
  tabsetPanel(
    tabPanel("Home", fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Global Filters"),
          selectInput("electionCounty", "Choose a County to Highlight:",
                      choices = c("None" = "", sort(unique(election_data_combined$County))),
                      selected = ""
          ),
          selectInput("year",
                      "Election Year:",
                      choices = c("2000", "2004", "2008", "2012", "2016", "2020"),
                      selected = "2020"
          ),
          hr(),
          h4("ACS Filters"),
          # selectInput("ACScounty", "Choose a County to View:",
          #   choices = c("None" = "", sort(unique(ACS$County))),
          #   selected = ""
          # ),
          selectInput("sex", "Sex:",
                      choices = c("All", sort(unique(ACS$Sex))),
                      selected = "All"
          ),
          selectInput("race", "Race:",
                      choices = c("All", sort(unique(ACS$Race))),
                      selected = "All"
          ),
          selectInput("ageCategory", "Age Category:",
                      choices = c("All", sort(unique(ACS$Age_Category))),
                      selected = "All"
          )
        ),
        mainPanel(
          h4("Election Data Map", style = "text-align: center;"),
          leafletOutput("map_election", height = "350px"),
          # div(style = "margin-top: 20px;"),
          h4("ACS Data Map", style = "text-align: center;"),
          leafletOutput("map_ACS", height = "350px")
        )
      )
    )),
    tabPanel("Write-Up", fluidPage(
      titlePanel("Write-Up")
    )),
    tabPanel("Data Table", fluidPage(
      titlePanel("Data Table")
    )),
    tabPanel("Animated Plot", fluidPage(
      titlePanel("NC Over Time")
    ))
  )
)

# define server logic ----------------------------------------------------------
server <- function(input, output, session) {
  # Map 1: Election data -------------------------------------------------------

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
  
  # reactive expression for fillOpacity
  # fillOpacityCalc <- reactive({
  #   if (is.null(input$electionCounty) || input$electionCounty == "") {
  #     return(rep(1, nrow(combined_data()))) 
  #   } else {
  #     return(ifelse(combined_data()$County == input$electionCounty, 1, 0.33))
  #   }
  # })

  # render map based on selected year
  output$map_election <- renderLeaflet({
    leaflet(data = combined_data()) |>
      addTiles() |>
      setView(lng = -79.1, lat = 35.2, zoom = 7) |>
      addPolygons(
        fillColor = ~ palette(politicalparty),
        fillOpacity = 1,
        color = "gray",
        popup = ~ paste(
          "County: ", County, "<br>",
          "Republican Votes: ", scales::comma(Votes_REP), "<br>",
          "Democrat Votes: ", scales::comma(Votes_DEM), "<br>",
          "% Votes for GOP: ", scales::percent(politicalparty, accuracy = 1)
        ),
        layerId = ~County
      ) |>
      addLegend("bottomright", 
                pal = palette, 
                values = c(0, 1), 
                title = "% Votes for GOP",
                labFormat = labelFormat(
                  transform = function(x) x * 100, suffix = "%")
                )
  })

  # keep track of county that was previously selected
  lastSelected <- reactiveVal(NULL)

  # listen for changes to selected county
  observeEvent(input$electionCounty, {
    newSelection <- input$electionCounty
    oldSelection <- lastSelected()

    # reset previously selected county if applicable
    if (!is.null(oldSelection) && oldSelection != "" &&
      any(combined_data()$County == oldSelection)) {
      leafletProxy("map_election") |>
        removeShape(layerId = oldSelection) |>
        addPolygons(
          data = combined_data() |> filter(County == oldSelection),
          fillColor = ~ palette(politicalparty),
          fillOpacity = 1,
          color = "gray",
          weight = 1,
          popup = ~ paste(
            "County: ", County, "<br>",
            "Republican Votes: ", scales::comma(Votes_REP), "<br>",
            "Democrat Votes: ", scales::comma(Votes_DEM), "<br>",
            "% Votes for GOP: ", scales::percent(politicalparty, accuracy = 1)
          ),
          layerId = ~County
        )
    }

    # highlight selected county if valid
    if (newSelection != "" && any(combined_data()$County == newSelection)) {
      leafletProxy("map_election") |>
        addPolygons(
          data = combined_data() |> filter(County == newSelection),
          fillColor = "yellow",
          fillOpacity = 1,
          color = "black",
          weight = 6,
          popup = ~ paste(
            "Highlighted County: ", County, "<br>",
            "Republican Votes: ", scales::comma(Votes_REP), "<br>",
            "Democrat Votes: ", scales::comma(Votes_DEM), "<br>",
            "% Votes for GOP: ", scales::percent(politicalparty, accuracy = 1)
          ),
          layerId = ~County
        )
    }

    lastSelected(newSelection)
  })

  # Map 2: ACS Data ------------------------------------------------------------

  # filter ACS data based on selected filters
  filter_pops <- reactive({
    data <- ACS |>
      filter(Year == as.numeric(input$year))

    if (!is.null(input$race) && input$race != "All") {
      data <- data |>
        filter(Race == input$race)
    }

    if (!is.null(input$sex) && input$sex != "All") {
      data <- data |>
        filter(Sex == input$sex)
    }

    if (!is.null(input$ageCategory) && input$ageCategory != "All") {
      data <- data |>
        filter(Age_Category == input$ageCategory)
    }
    
    data <- data |>
      group_by(County, Year) |>  # Ensure grouping includes Year if it's relevant
      summarise(
        TotalCount = sum(Count, na.rm = TRUE),
        countyPop = first(countyPop)  # Ensure countyPop is maintained correctly
      ) |>
      ungroup() |>
      mutate(
        popProp = if_else(countyPop > 0, TotalCount / countyPop, 0)
      )

    data <- merge(counties_geo, data, by = "County", all.x = TRUE)

    data$popProp[is.na(data$popProp)] <- 0

    data
  })

  # output ACS map based on selected filters
  output$map_ACS <- renderLeaflet({
    pop_palette <- colorNumeric("Greens", domain = filter_pops()$popProp)

    leaflet(data = filter_pops()) |>
      addTiles() |>
      setView(lng = -79.1, lat = 35.2, zoom = 7) |>
      addPolygons(
        fillColor = ~ pop_palette(popProp),
        fillOpacity = 1,
        color = "gray",
        popup = ~ paste(
          "County: ", County, "<br>",
          "County Pop.: ", scales::comma(countyPop), "<br>",
          "% of Pop.: ", scales::percent(popProp, accuracy = 0.01)
        ),
      ) |>
      addLegend("bottomright", 
                pal = pop_palette, 
                values = ~popProp,
                title = "% of Population",
                opacity = 1.0,
                labFormat = labelFormat(
                  transform = function(x) x * 100, suffix = "%")
                )
  })
}

shinyApp(ui = ui, server = server)

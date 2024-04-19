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

ACS2000_cleaned <- read_csv("data for app/ACS2000_cleaned.csv")
ACS2004_cleaned <- read_csv("data for app/ACS2004_cleaned.csv")
ACS2008_cleaned <- read_csv("data for app/ACS2008_cleaned.csv")
ACS2012_cleaned <- read_csv("data for app/ACS2012_cleaned.csv")
ACS2016_cleaned <- read_csv("data for app/ACS2016_cleaned.csv")
ACS2020_cleaned <- read_csv("data for app/ACS2020_cleaned.csv")

ACS_all <- rbind(ACS2000_cleaned, 
                 ACS2004_cleaned,
                 ACS2008_cleaned,
                 ACS2012_cleaned,
                 ACS2016_cleaned,
                 ACS2020_cleaned)

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

#Change datatype to numeric
election_data_combined$Lat <- as.numeric(as.character(election_data_combined$Lat))
election_data_combined$Long <- as.numeric(as.character(election_data_combined$Long))

ACS$Lat <- as.numeric(as.character(ACS$Lat))
ACS$Long <- as.numeric(as.character(ACS$Long))

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("North Carolina Election Data"),
  leafletOutput("map1"),
  leafletOutput("map2"),
  sidebarLayout(
    sidebarPanel(
      h4("Election Filters"),
      selectInput("electioncounty", "Choose a County to View:",
                  choices = c("None" = "", election_data_combined$County),
                  selected = ""),
      selectInput("political", "Choose a Political Party: ",
                  choices = c("Rep" = "Rep", "Dem" = "Dem"),
                  selected = "Rep"),
      selectInput("year",
                  "Election Year:",
                  choices = c("2000", "2004", "2008", "2012", "2016", "2020"),
                  selected = "2020"),
      hr(),
      h4("ACS Filters"),
      selectInput("ACScounty", "Choose a County to View:",
                  choices = c("None" = "", ACS$County),
                  selected = ""),
      selectInput("Sex", "Sex:", choices = c("All", sort(unique(ACS$Sex)))),
      selectInput("Race", "Race:", choices = c("All", sort(unique(ACS$Race)))),
      selectInput("Age_Category", "Age Category:", choices = c("All", sort(unique(ACS$Age_Category))))
    ),
    
    
    mainPanel(
      plotOutput("electiondist"),
      plotOutput("ACSdist")
    )
  )
)


server <- function(input, output, session) {
  output$map1 <- renderLeaflet({
    leaflet(data = counties_geo) |>
      addTiles() |>
      setView(lng = -79.0, lat =35.5, zoom = 7)
  })
  
  output$map2 <- renderLeaflet({
    leaflet(data = counties_geo) |>
      addTiles() |>
      setView(lng = -79.0, lat =35.5, zoom = 7)
  })
  
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

  
  observe({
    df <- picking_political()
    
    matched_index_election <- match(counties_geo$County, df$County)
    matched_votes_election <- df$Votes[matched_index]
    selected_county_election <- input$electioncounty
    
    palette <- colorQuantile(if (input$political == "Rep") "Reds" else "Blues", na.omit(matched_votes_election), n = 5)
    
    leafletProxy("map1", data = counties_geo) |>
      clearShapes() |>
      addPolygons(
        fillColor = ~palette(matched_votes_election),
        fillOpacity = ~ifelse(County == selected_county_election, 1, 0.5),
        color = ~ifelse(County == selected_county_election, "black", "gray"),
        opacity = 1,
        smoothFactor = 0,
        weight = ~ifelse(County == selected_county_election, 3, 1),
        popup = ~paste(County, "<br>", input$political, "Votes: ", matched_votes_election)
      )
  })


ACS_filtered <- reactive({
  ACS_year <- ACS |>
    filter(Year == as.numeric(input$Year))
  
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
      fillColor = ~palette(Count),
      fillOpacity = ~ifelse(County == selected_county_ACS, 1, 0.5),
      color = ~ifelse(County == selected_county_ACS, "black", "gray"),
      opacity = 1,
      smoothFactor = 0,
      weight = ~ifelse(County == selected_county_ACS, 3, 1),
      popup = ~paste(County, "<br>", "Population: ", Count)
    )
})

}

shinyApp(ui = ui, server = server)
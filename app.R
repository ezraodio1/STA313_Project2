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
library(DT)
#install.packages("shinyWidgets")
library(shinyWidgets)
#install.packages("shinythemes")
library(shinythemes)

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
  theme = shinytheme("superhero"),
  # titlePanel("North Carolina Election Data"),
  tabsetPanel(
    tabPanel("Home", fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Global Filters"),
          selectInput("electionCounty", "Choose a County to Highlight:",
            choices = c(
              "None" = "",
              sort(unique(election_data_combined$County))
            ),
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
      titlePanel("Write-Up"),
      h3("Introduction"),
      p("Our team had a keen interest in United States politics especially with the 2024 National Election coming up. North Carolina was the best state to investigate as students of a North Carolina University and as North Carolina is usually considered a swing state. We are very interested in understanding the breakdown of votes and demographics by county to understand which counties are especially determining the state's outcome every election. We will be examining the breakdown of Democratic vs. Republican votes by county over the last six elections as well as the breakdown of different demographics in those counties. We decided to focus on sex, age and race for the specific demographics and we are only examining votes for Presidential elections. Our app presents users with dynamic maps, tables, and an animated visualization and is intended for policymakers and the general public to have an accessible tool that allows them to analyze North Carolina's political trends, and especially how demographic trends coorrelate with voting patterns throughout the previous six elections."),
      h3("Data"),
      p("In order to study these breakdowns we found 12 data sets - 6 for the affiliation of votes by county and 6 for the demographic breakdown of voters by county. Each election year from 2000-2020 has two data sets. We found the 6 data sets for affiliation from the North Carolina Board of Elections and for the census data we used dataset from the Office of State Budget and Management. For the election data we used variables like Total Votes, Choice Party and County and from the ACS data we used age_category, sex, and race. We also used a dataset that had the latitude and longitude coordinates of each county in North Carolina as well as a 6 datasets that had the population by county for each of the 6 years we were examining. We used census data for the population per county for accuracy, thus the only updates in population are made every 10 years instead of 4 years per election."),
      h3("Approach"),
      p("Our first step once finding the datasets was to clean them and combine the individual 6 datasets for the election into 1 data set and the individual 6 datasets for the ACS data into one dataset. In order to clean the election data we had to read in the individual text files, rename the columns, filter for only Presidential results and then filter for only the variables we wanted. We also need to add another column that had the year of the election. Once we did this for all 6 of the text files turned into csv files we combined them all together into election_data, and then joined with another dataset called separate_geo that contained the latitude and longitude coordinates for each county and called this new data election_data_wide_geo. We then created another dataset called combined_population by combining the 6 datasets we found with the population by county for each of the 6 elections years into a single data set with the population for each year and then we joined combined_population with election_data_wide_geo and created two new columns that contained the democratic votes per capita and the republican votes."),
      p("Next we cleaned the ACS data which just included reading in each csv for each year and filtering for the variables we wanted and then binding all 6 of those datasets into one ASC_all."),
      p("Now that we had our cleaned data, in order to show these breakdowns by county we will use a Shiny app with two leaflet maps. The first map allows the user to filter by election and county. It shows the breakdown of Democratic vs. Republican votes by population for the specific election chosen and colors from a red to purple scale. It also highlights the the county chosen, so it is easier to view specific counties. When you click on a county an overview of the number of Republican and Democratic votes is shown."),
      p("The second leaflet map shows voting demographics for the same elections and allows to filter by either sex, race, age or a combination of the three. The map also colors by the filters chosen by percentage of population on a green to white scale. The filters allow users to determine what could be causing the changes in voting affiliation, whether that's race, age, population, or all three combined."),
      p("We chose to use a shiny app with two leaflet maps because it allows easy examination of the differences from year to year, county to county and from specific filters. We also added an animation at the end that shows the county breakdown of dem votes vs. rep votes over the 6 elections to get a better idea of how the individual counties changed. Layering the shiny app and leaflet maps allows the user to zoom in and zoom out as they please to see the rest of the United States' map."),
      h3("Going to add the rest when finalized")
    )),
    tabPanel("ACS: Data Table", fluidPage(
      titlePanel("ACS Data Table"),
      dataTableOutput("data_table")
    )),
    tabPanel("Election Votes: Data Table", fluidPage(
      titlePanel("Election Votes"),
      dataTableOutput("election_data_table")
    )),
    tabPanel("Animated Plot", fluidPage(
      titlePanel("NC Over Time"),
      img(src = "nc_political.gif", alt = "Animated Election Map")
    ))
  )
)

# define server logic ----------------------------------------------------------
server <- function(input, output, session) {
  
  
# ACS data table ---------------------------------------------------------------
  user_filtered <-reactive({
    data <- ACS_all_long |>
      filter(
        if(input$race != "All") Race == input$race else TRUE,
        if(input$sex != "All") Sex == input$sex else TRUE,
        if(input$ageCategory != "All") Age_Category == input$ageCategory else TRUE
      ) |>
      select(County, Race, Sex, Age_Category, Count)
    data
  })
    
output$data_table <- renderDataTable({
  datatable(
    user_filtered(),
    options = list(
      pageLength = 50,
      autoWidth = TRUE,
      searching = TRUE,
      lengthMenu = c(10, 50, 100)
    ),
    filter = "top",
    rownames = FALSE,
  ) |>
    formatStyle(
      columns = c('County', 'Race', 'Sex', 'Age_Category', 'Count'),
      fontWeight = 'bold'
    ) 
  })
  
# Election data table ----------------------------------------------------------

user_filtered2 <-reactive({
  data <- election_data_combined |>
    select(County, Year, Votes_DEM, Votes_REP, Population)
  data
})

output$election_data_table <- renderDataTable({
  datatable(
    user_filtered2(),
    options = list(
      pageLength = 50,
      autoWidth = TRUE,
      searching = TRUE,
      lengthMenu = c(10, 50, 100)
    ),
    filter = "top",
    rownames = FALSE,
  ) |>
    formatStyle(
      columns = c('County', 'Year', 'Votes_DEM', 'Votes_REP', 'Population'),
      fontWeight = 'bold'
    ) 
})
    
# Map 1: Election data ---------------------------------------------------------
  
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
          transform = function(x) x * 100, suffix = "%"
        )
      )
  })

  # keep track of county that was previously selected
  lastSelectedElection <- reactiveVal(NULL)
  lastSelectedACS <- reactiveVal(NULL)

  # listen for changes to selected county
  observeEvent(input$electionCounty, {
    newSelection <- input$electionCounty
    oldSelection <- lastSelectedElection()

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

    lastSelectedElection(newSelection)
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
      group_by(County, Year) |> # Ensure grouping includes Year if it's relevant
      summarise(
        TotalCount = sum(Count, na.rm = TRUE),
        countyPop = first(countyPop) # Ensure countyPop is maintained correctly
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
    
    # create palette to color ACS map
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
        layerId = ~County
      ) |>
      addLegend("bottomright",
        pal = pop_palette,
        values = ~popProp,
        title = "% of Population",
        opacity = 1.0,
        labFormat = labelFormat(
          transform = function(x) x * 100, suffix = "%"
        )
      )
  })
  
  observeEvent(input$electionCounty, {
    newSelection <- input$electionCounty
    oldSelection <- lastSelectedACS()
    
    # create palette to color ACS map
    pop_palette <- colorNumeric("Greens", domain = filter_pops()$popProp)
    
    if (!is.null(oldSelection) && oldSelection != "" &&
        any(filter_pops()$County == oldSelection)) {
      leafletProxy("map_ACS") |>
        removeShape(layerId = oldSelection) |>
        addPolygons(
          data = filter_pops() |> filter(County == oldSelection),
          fillColor = ~ pop_palette(popProp),
          fillOpacity = 1,
          color = "gray",
          weight = 1,
          popup = ~ paste(
            "County: ", County, "<br>",
            "County Pop.: ", scales::comma(countyPop), "<br>",
            "% of Pop.: ", scales::percent(popProp, accuracy = 0.01)
          ),
          layerId = ~County
        )
    }
    
    if (newSelection != "" && any(filter_pops()$County == newSelection)) {
      leafletProxy("map_ACS") |>
        addPolygons(
          data = filter_pops() |> filter(County == newSelection),
          fillColor = "yellow",
          fillOpacity = 1,
          color = "black",
          weight = 6,
          popup = ~ paste(
            "Highlighted County: ", County, "<br>",
            "County Pop.: ", scales::comma(countyPop), "<br>",
            "% of Pop.: ", scales::percent(popProp, accuracy = 0.01)
          ),
          layerId = ~County
        )
    }
    
    lastSelectedACS(newSelection)
  })
  
  # reset selected county any time a filter is changed
  # observeEvent(c(input$year, input$race, input$sex, input$ageCategory, input$year), {
  #   updateSelectInput(session, "electionCounty", selected = "")
  # })
}

shinyApp(ui = ui, server = server)

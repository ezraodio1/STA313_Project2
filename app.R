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

#combine ACS data sets
ACS_all <- rbind(
  ACS2000_cleaned,
  ACS2004_cleaned,
  ACS2008_cleaned,
  ACS2012_cleaned,
  ACS2016_cleaned,
  ACS2020_cleaned
)

#pivot ACS_all dataset for easier filtering
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

#Add column that calculates county population
county_pops <- ACS |>
  group_by(County, Year) |>
  reframe(countyPop = sum(Count, na.rm = TRUE))

ACS <- merge(ACS, county_pops, by = c("County", "Year"))

# Define UI for application ----------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tabsetPanel(
    tabPanel("Home", fluidPage(
      div(
        style = "display: flex; justify-content: space-between; align-items: center;",  # Flexbox for layout
        h1("Team Spring", style = "flex-grow: 1;"),  # Title grows to take available space
        img(src = "logo.png", style = "height: 90px; width: auto;")  # Controlled size for image
      ),
      h3("Overiew of app"),
      h4("Interactive Maps"),
      p("The interactive maps tap displays two maps of North Carolina by county. The filters on the left hand side display different filters for the map. The global filters work on both maps. When you select a county, nothing changes with the how the data is filtered, but that county will highlight in yellow on both maps. When you select a year both the election data map and ACS map will filter for only data from that year. The election data map will show percentage of votes for the specified year, and the ACS map will show population data for the specified year. The next set of filters only work for the ACS map and are labeled ACS filters. Here you have the option to select race, sex, or age. You can select from only one of these filters or all of these filters. For example if you select just female then the data on the ACS map will show the percentage population that is female by county. If you select female, black, and Age 18 to 19 then the ACS map would show the percentage of population that is female, black, and 18 to 19."),
      h5("Election Data Map"),
      p("The Election Data Map displays the percentage of votes per GOP (Republican Party). For example if a county displays as 40% votes per GOP this mean that within that county 40% of their votes were republican, while 60% of their votes democrat. The counties that are colored more purple mean that they had a higher percentage of democratic votes and the counties that are colored red mean that they had a higher percentage of republican votes. "),
      h5("ACS Map"),
      p("The ACS Map displays the percentage of the population in the county for any given filters selected. When the map first appears, since no filters are selected each county shows 100% population. However, when filters are selected the counties will show the corresponding percentage of population. A county colored closer to white shows a smaller percentage of the population and a county colored closer to green shows a larger percentage of the population."),
      h4("Write-up"),
      p("The write-up tab includes our approach, methods, and anlysis for the project."),
      h4("ACS Data Table"),
      p("The ACS data table tab includes a data set with all of the ACS data broken down to the most specific county of population. Here you have the ability to look at specific counts of population. You can filter by any or all of county, race, sex, age, and population. For example if you wanted to see what counties have a female population greater than 100,000, you would select female for age and move the toggle for count from (0 to max) to (100,000 to max)."),
      h4("Election Votes Data Table"),
      p("The Election Votes data table tab includes a data set with all of the Election data. Here you have the ability to view the democrat, republican, and total count of votes by county. You have the option to filter by county, year, max and min democratic votes, max and min republican votes, and max and min total votes. For example if you wanted to view which counties for each year had more than 100,000 democratic votes but less than 500,000 people you could move the Votes_DEM toggle to (100,000 to max) and the Population toggle to (0 to 500,000). Note that population includes children under 18 not just the voting population. "),
      h4("Animaed Plot"),
      p("The Animated Plots tab shows two different animated plots. The first plot shows the change in % votes of GOP (Republican Party) over the past 6 elections by county. The second plot shows the change in population distribution of the last 6 election years."))),
    tabPanel("Interactive Maps", fluidPage(
      sidebarLayout(
        sidebarPanel(
          #Add filters that work for both maps
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
          #Add ACS specific filters
          h4("ACS Filters"),
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
          h4("ACS Data Map", style = "text-align: center;"),
          leafletOutput("map_ACS", height = "350px")
        )
      )
    )),
    #Add write up in a tab in app
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
      h3("Analysis"),
      p("A trend that we were following throughout the making of this project was how strongly race correlated with election votes. For example, in 2000, counties that had the most significant votes for the democratic party were also counties that had large Black populations. Specific counties include: Warren (32% Dem votes for the GOP and 26.12% Black Population), NorthHampton (33% Dem votes for the GOP and 28.58% Black Population), and Hertford (30% Dem votes for the GOP and 28.21% Black Population). Over time, we noticed the coorelation between race and democrat votes tapering off in these counties and lessening. For example, in the counties we mentioned, in 2020, Warren had 37% Dem votes for the GOP but only a 24% Black Population, NorthHampton had 40% Dem votes for the GOP but only a 27.16% Black population, and lastly, Hertford had 33% Dem votes for the GOP and a 28.27% Black Population."),
      p("Additionally, we noticed increasing political polarization in North Carolina over time. Looking at the animated plot of Political Distribution in North Carolina over time, we can notice the 'blue' and 'red' becoming of a darker hue in many of the same counties over time. Indicating that counties become either more 'red' or more 'blue' distancing from a neutral stance. There is data to support that political polarization has been on the 'up' since the election of 2016 (Donald Trump vs Hillary Clinton) and it was interesting to see the North Carolina data support this conclusion as especially in 2016 and 2020 the intensity of the hues indicating republican or democrat votes increased."),
      h3("Conclusion"),
      p("In conclusion, our Shiny app allows users to view electoral trends alongside demographic changes in North Carolina over the past six elections. By integrating the two, users can get a holistic sense of the causes behind the electoral trends in North Carolina and the reasons behind the 'swing state' nature of NC. Our findings underscored the importance of race, sex, and age [need to add more to prove this in the analysis section] have had on election data, but has highlighted the significant polarization occurring in the state."),
      p("By creating three different facets to display information (leaflet plots, data tables, and animated visualizations) we are allowing the user to understand the intricacies behind the changing voter trends in North Carolina as well as enabling policymakers to make educated predictions for the 2024 National Election."),
      p("We hope that this project allows all North Carolinian's and politics enthusiasts to understand the complex voter dynamics that occur in the state and contributed to more informed policy discussions in the near future."),
      h3("Sources"),
      p("Election Data: https://www.ncsbe.gov/results-data/election-results/historical-election-results-data#by-precinct"),
      p("Demographics Data: https://www.osbm.nc.gov/facts-figures/population-demographics"),
      p("Layering a Shiny App onto Leaflet: https://rstudio.github.io/leaflet/articles/shiny.html"),
      p("Data tables in Shiny: https://shiny.posit.co/r/articles/build/datatables/"),
      p("Allowing the User to Select the # of Rows in the Datatable: https://forum.posit.co/t/getting-the-user-selected-entry-of-a-data-table-in-shiny-to-actually-make-a-scatterplot/52419")
    )),
    #Add tab for ACS data table
    tabPanel("ACS: Data Table", fluidPage(
      titlePanel("ACS Data Table"),
      dataTableOutput("data_table")
    )),
    #Add tab for election data table
    tabPanel("Election Votes: Data Table", fluidPage(
      titlePanel("Election Votes"),
      dataTableOutput("election_data_table")
    )),
    #Add tab for animated plot
    tabPanel("Animated Plots", fluidPage(
      titlePanel("North Carolina Over Time"),
      div(
        style = "display: flex; justify-content: center; align-items: center; flex-wrap: wrap; gap: 20px;",  # Added gap for spacing
        # First div with vertical layout for the first map
        div(
          style = "display: flex; flex-direction: column; align-items: center; margin: 10px; max-width: 450px;",
          h3("Animated Election Map"),
          tags$a(href = "nc_political.gif", target = "_blank",
                 img(src = "nc_political.gif", style = "width: 100%; height: auto; cursor: pointer;")
          ),
          p("Distribution of % votes for GOP by county over the past 6 elections")
        ),
        
        div(
          style = "display: flex; flex-direction: column; align-items: center; margin: 10px; max-width: 450px;",
          h3("Animated Population Map"),
          tags$a(href = "nc_population.gif", target = "_blank",
                 img(src = "nc_population.gif", style = "width: 100%; height: auto; cursor: pointer;")
          ),
          p("Distribution of population by county over the past 6 elections years")
        )
      ),
      p("Click the plot and zoom in to view it in a larger format.")
      )
    ))
  )


# define server logic ----------------------------------------------------------
server <- function(input, output, session) {
  
  
# ACS data table ---------------------------------------------------------------
  #create reactive filters for ACS
  user_filtered <-reactive({
    data <- ACS_all_long |>
      filter(
        if(input$race != "All") Race == input$race else TRUE,
        if(input$sex != "All") Sex == input$sex else TRUE,
        if(input$ageCategory != "All") Age_Category == input$ageCategory else TRUE
      ) |>
      mutate(Count = as.integer(Count)) |>
      select(County, Race, Sex, Age_Category, Count)
    data
  })
    
  #create reactive filters for data table for ACS
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

#create reactive filters for election (this is just county and year)
user_filtered2 <-reactive({
  data <- election_data_combined |>
    mutate(Year = as.integer(Year)) |>
    mutate(Votes_DEM = as.integer(Votes_DEM)) |>
    mutate(Votes_REP = as.integer(Votes_REP)) |>
    mutate(Population = as.integer(Population)) |>
    select(County, Year, Votes_DEM, Votes_REP, Population)
  data
})

#create reactive filters for data table for election
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
  
  # listen for changes to selected county
  observeEvent(input$electionCounty, {
    newSelection <- input$electionCounty
    oldSelection <- lastSelectedACS()
    
    # create palette to color ACS map
    pop_palette <- colorNumeric("Greens", domain = filter_pops()$popProp)
    
    # reset previously selected county if applicable
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
    
    # highlight selected county if valid
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
  
}

shinyApp(ui = ui, server = server)

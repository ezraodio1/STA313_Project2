library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(gganimate)
library(tidyverse)
library(scales)

# load and prepare the geographic data
counties_geo <- st_read("data/nc_counties.geojson") |>
  mutate(County = toupper(as.character(County)))

# load and prepare the ACS data
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

# calculate county populations
county_pops <- ACS |>
  group_by(County, Year) |>
  reframe(countyPop = sum(Count, na.rm = TRUE))

# create data set to be used for plot
full_data <- left_join(counties_geo, county_pops, by = "County")

#create static plot
nc_population <- ggplot(full_data, aes(fill = countyPop)) +
  geom_sf(aes(geometry = geometry), size = 0.1) +
  theme_minimal() +
  scale_fill_gradient(
    low = "white", high = "darkgreen",
    name = "County Population",
    labels = scales::comma
  ) +
  facet_wrap(~Year)

# create animated plot
nc_population_animated <- nc_population +
  facet_null() +
  transition_manual(frames = Year) +
  labs(
    title = "North Carolina Population Distribution in {current_frame}",
    fill = "County Population"
  ) +
  theme(legend.position = "right")

anim_save("www/nc_population.gif", nc_population_animated,
          renderer = gifski_renderer()
)
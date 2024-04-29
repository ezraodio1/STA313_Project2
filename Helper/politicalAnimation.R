library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(gganimate)

# load and prepare geographic data
counties_geo <- st_read("data/nc_counties.geojson") |>
  mutate(County = toupper(as.character(County)))

# load and prepare election data
election_data_combined <- read_csv("data/election_data_wide_geo.csv",
  show_col_types = FALSE
) |>
  mutate(County = toupper(County)) |>
  mutate(Year = as.integer(Year))

# create data set to be used for plot
full_data <- left_join(counties_geo, election_data_combined, by = "County")

# calculate political party representation
full_data$politicalparty <- full_data$Votes_REP /
  (full_data$Votes_REP + full_data$Votes_DEM)

# create the static plot
nc_political <- ggplot(full_data, aes(fill = politicalparty)) +
  geom_sf(aes(geometry = geometry), size = 0.1) +
  theme_minimal() +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0.5,
    limit = c(0, 1), space = "Lab",
    name = "Republican Vote Proportion"
  ) +
  facet_wrap(~Year)

# create the animated plot
nc_political_animated <- nc_political +
  facet_null() +
  transition_manual(frames = Year) +
  labs(
    title = "Political Distribution in {current_frame}"
  ) +
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0.5,
    limit = c(0, 1), space = "Lab",
    name = "Republican Vote Proportion"
  )

# save the gif
anim_save("www/nc_political.gif", nc_political_animated,
  renderer = gifski_renderer()
)
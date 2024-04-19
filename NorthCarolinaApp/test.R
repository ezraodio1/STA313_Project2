

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

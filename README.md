# project-2
Data sources:

https://www.ncsbe.gov/results-data/election-results/historical-election-results-data#by-precinct

https://www.osbm.nc.gov/facts-figures/population-demographics

We are using 12 data sets, 6 from ACS and 6 for registration data. Each of the 6 ACS and 6 election data sets are the same with the exception of year.

Helper Function: includes our data cleaning files we used to clean election data sets, ACS data sets and combine geo county data with the election data. In the helper function data folder it contains all 
of the txt. and csv files we pulled directly from the websites we are using. 

Within the data folder the follow data sets are included:
NCCounties2.csv has data on the counties in NC
- `name`: Name of county
- `lat`: Latitude 
- `lon`: Longitude

The ACS data sets are called ACS[year]_cleaned. For the ACS data these are the columns and their meaning:
- `fips`: Federal Information Processing Standards code that uniquely identifies counties.
- `County`: Name of the county.
- `Year`: Year
- `Race`: Racial demographics.
- `Sex`: Gender (Male, Female).
- `Age 0 to 2`: Population aged between 0 to 2 years.
- `Age 3 to 4`: Population aged between 3 to 4 years.
- ... (Continue for all age brackets)
- `Total`: Total county population.
- `medage`: Median age of the population.
- `Age 0 to 17`: Population aged between 0 to 17 years.
- `Age 18 to 24`: Population aged between 18 to 24 years.
- `Age 25 to 44`: Population aged between 25 to 44 years.
- `Age 45 to 64`: Population aged between 45 to 64 years.
- `Age 65 Plus`: Population aged 65 and above.
- `geom`: Geometry information for mapping.
- `geo_point_2d`: Geographic point in 2D.

The election data sets are combined and called election-data, which contains 3 columns:
- `County`: North Carolina County
- `Choice_Party`: The political party
- `Total_Votes`: Total votes for that specific county and political party.

The election data sets are combined with geo data and called election_data_geo_wide, which contains columns:
- `County`: North Carolina County
- `Year`: Year of election
- `Votes_DEM`: Number of votes from that county toward democratic candidate 
- `Votes_Rep`: Number of votes from that county toward democratic candidate 
- `Lat`: Latitude 
- `Long`: Longitude 
- `Population`: Population in that county at year of election 
- `dem_per_capita`: Democratic per capita (number of democratic votes divided by total pop)
- `rep_per_capita`: Republican per capita (number of republican votes divided by total pop)

NCDOT_County_Boundaries contains data on county location the only things we are interested in are
- `FIPS`: Federal Information Processing Standards code, a unique identifier for counties across the United States. The code combines state and county information into one five-digit number.
- `CountyName`: The name of the county in North Carolina.
- `UpperCountyName`: The name of the county formatted in uppercase
- `SapCountyId`: An identifier used in SAP systems for county data management 
- `DOTDistrictID`: An identifier for the Department of Transportation district that encompasses the county
- `DOTDivisionID`: An identifier for a specific administrative division within the North Carolina Department of Transportation that the county belongs to
- `SAP_CNTY_NBR`: A different or specific SAP system identifier for the county.
- `CNTY_NBR`: A numeric identifier for the county, used internally for various administrative purposes.
- `DSTRCT_NBR`: The district number within North Carolina in which the county is located
- `DIV_NBR`: The division number within the Department of Transportation that the county is associated with.
- `NAME`: The formal or legally recognized name of the county, which may include additional descriptors.
- `SHPNumber`: An identifier associated with the shapefile record for the county, which is part of a geospatial vector data format
- `ShapeSTArea`: The calculated area of the county based on its geometric representation in the shapefile
- `ShapeSTLength`: The length of the perimeter of the county as calculated from the geometric data in the shapefile

www contain:
nc_politcal.gif is an animation of the count of political votes by county in North Carolina over the past 6 elections
nc_population.gif is an animation of the population by county in North Carolina over the past 6 elections
logo.png is the stat 313 course logo

proposal.qmd contains our project proposal

Writeup.qmd contain our project write up

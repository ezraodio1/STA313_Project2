After cleaning our data sets we are left with two data sets one called ACSall 
and the other is election_data

ACSall is a merged dataset of the 6 individual asc csv for 2000,2004,2008,2012,
2016 and 2020 with the same variables as before:
For the ACS data these are the columns and their meaning
- `fips`: Federal Information Processing Standards code that uniquely identifies counties.
- `County`: Name of the county.
- `Region`: The region in which the county is located.
- `COG`: Council of Governments region.
- `MSA`: Metropolitan Statistical Area.
- `Year`: The year of the data - 2000.
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
- `Estimate/Projection`: Type of data (estimate or projection).
- `Vintage`: Year of the vintage.
- `geom`: Geometry information for mapping.
- `geo_point_2d`: Geographic point in 2D.

election_data is also a merged data set with all the 6 individual election data
that contains 1000 observations and 4 variables:
- `Couunty`: North Carolina County
- `Choice_Party`: The political party
- `Total_Votes`: Total votes for that specific county and political party.
- `Year`: Year of election 
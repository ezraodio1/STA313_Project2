# project-2

Data sources:

https://www.ncsbe.gov/results-data/election-results/historical-election-results-data#by-precinct

https://www.osbm.nc.gov/facts-figures/population-demographics

We are using 12 data sets, 6 from ACS and 6 for registration data. Each of the 6 ACS and 6 election data sets are the same with the exception of year.

Datacleaning_ACS includes our code for the data cleaning we did to all the ACS data sets.

Datacleaning_votes includes our code for the data cleaning we did to all the election data sets.

politicalAnimation includes code for the animation we created that shows the change in political party votes over the last 6 elections by county in NC.

populationAnimation includes code for the animation we created that shows the change in population over the last 6 elections by county in NC.

NCCounties2.csv has data on the counties in NC
- `name`: Name of county
- `lat`: Latitude 
- `lon`: Longitude

The ACS data sets are called ACS[year]. For the ACS data these are the columns and their meaning
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
Our clean datasets after Datacleaning_ACS are called ACS[year].

The election data sets are called [year].txt. There are 6 of them: one for each election year. Each data set contains 3 columns:
- `Couunty`: North Carolina County
- `Choice_Party`: The political party
- `Total_Votes`: Total votes for that specific county and political party.
Our clean datasets after Datacleaning_votes are called County_totals[year].


After cleaning our data sets we are left with:
1. 6 different ACS data sets labeled ACS[year], this is because the datsets were too large to merge and push.
2. election-data which contains the combined, cleaned County_total data sets
2. election_data_wide_geo which contains the election data joined with the county geo information

election_data is also a merged data set with all the 6 individual election data
that contains 1000 observations and 4 variables:
- `Couunty`: North Carolina County
- `Choice_Party`: The political party
- `Total_Votes`: Total votes for that specific county and political party.
- `Year`: Year of election 
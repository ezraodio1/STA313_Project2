# project-2
Data sources:

https://www.ncsbe.gov/results-data/election-results/historical-election-results-data#by-precinct

https://www.osbm.nc.gov/facts-figures/population-demographics

We are using 12 data sets, 6 from ACS and 6 for registration data. We provided 
one dataset for each of those categories and the only thing that changes is 
the year. 

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

The election data sets are called Election[year]_summary. There are 6 of them: one for each election year. Each data set contains 3 columns:
- `Couunty`: North Carolina County
- `Choice_Party`: The political party
- `Total_Votes`: Total votes for that specific county and political party.
# usda-hackathon-2024
R Shiny app for visualizing 2022 Ag Census data

## Motivation
[AMS Grants Division](https://www.ams.usda.gov/services/grants) needs to utilize AgCensus data to develop funding models for noncompetitive grants programs, such as [RFSI](https://www.ams.usda.gov/services/grants/rfsi) and [SCBG](https://www.ams.usda.gov/services/grants/scbgp), but there is no easy way to pull data, calculate model, and report results to stakeholders. ​

## Goals
We aimed to reduce the burden of manually pulling, calculating, and reporting on necessary data for funding. Our priorities included displaying information on sales data on both state and county levels, specifically breaking down sales into individual commodities as a percentage of their state total. We were also interested in retrieving socio-demographic data to be used in conjunction with sales to identify potential funding targets. 

We hope our work can provide insight to external State stakeholders who ask how we calculate funding and make our funding processes more transparent.

## Technical Approach
Targeted data was pulled using reproducible scripts from the AgCensus via the [NASS QuickStats API​ ](https://quickstats.nass.usda.gov/). This data was queried and cleaned to provide the most accurate numbers.

Regarding the Shiny App itself, the backend processes user inputs with individual R functions that update the query to collected data and retrieves desired data. The front end uses a simple responsive design with 4 main components: user inputs, a map built with leaflet, a table built with reactable, and a plot initially built with plotly. All three are dynamic Javascript based visualizations to ensure quicker and easier interaction between components. 

## Example Uses
Coming Soon

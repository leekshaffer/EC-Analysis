# EC-Analysis

Created: September 5, 2025

Shareable Link: <https://bit.ly/Elec-Analysis>

Metrics and visualizations of the distortion in U.S. electoral populations by various demographic characteristics

A manuscript describing the methods and results is coming soon.

An interactive R Shiny app to interactively display results of the analysis is available at: <https://bit.ly/Elec-Weights>.

Or by using the app.R file in the RShinyApp folder in this repository.

For re-use, the analysis files are in order in the R folder beginning with "01-census_import.R". 
Note that you will need a Census API key to download the Census data. To begin with the data already imported, you can begin at "03-analysis.R".
Any changes to later-numbered files do not require re-running earlier-numbered files.

Census data accessed using tidycensus package (Kyle Walker and Matt Herman, v1.7.3, https://walker-data.com/tidycensus/) and a Census API key (see U.S. Census Bureau, "Developers", https://www.census.gov/data/developers.html)
Historic apportionment/electoral vote data from https://history.house.gov/Institution/Apportionment/Apportionment/, accessed Sep. 28, 2025.

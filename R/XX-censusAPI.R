library(tidycensus)
library(tidyverse)

# census_api_key("a775030cabc684a6421fb484c5cdec194cae9fa8",
#                install=TRUE, overwrite=TRUE)

acs_yrs <- c("06", "08", "10", "12", "14",
             "16", "18", "19", "22", "23")

for (yr in acs_yrs) {
  ## Note: 2020 ACS was "experimental estimates" not accessible in API;
  ## 2024 not available as of 9/3/25
  assign(x=paste0("v",yr),
         value=load_variables(as.double(paste0("20",yr)),
                              "acs1"))
}
v00sf1 <- load_variables(2000, "sf1")
v10sf1 <- load_variables(2010, "sf1")
v20dhc <- load_variables(2020, "dhc")

save(list=c("v00sf1","v10sf1","v20dhc"),
     file="int/DecennialCensusVars.Rda")

save(list=paste0("v", acs_yrs),
     file="int/ACSVars.Rda")

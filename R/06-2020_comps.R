library(tidyverse)
source("R/00-key_values.R")

## Load Mapping data frame:
Yr <- 2020
Type <- "Census"
load(paste0("res/",Type,"/Map_States_", Yr, ".Rda"))

## Add demographics:
Dat <- file_usm
for (Name in Names) {
  if (paste(Name, Yr, sep="_") %in% names(Cols_Specs)) {
    Cols_Use <- (ColOrders[[Name]])[Cols_Specs[[paste(Name, Yr, sep="_")]]]
  } else {
    Cols_Use <- ColOrders[[Name]]
  }
  
  load(paste0("int/", Type, "/", Name, "_", Yr, ".Rda"))
  Add <- get(paste(Name, Yr, sep="_"))$State %>%
    dplyr::rename(fips=GEOID) %>%
    dplyr::mutate(across(all_of(Cols_Use),
                         ~.x/Total)) %>%
    dplyr::select(fips,all_of(Cols_Use))
  Dat <- Dat %>%
    left_join(Add, by=join_by(fips))
}

summary(lm(EC_Weight ~ Urban, data=Dat))
summary(lm(Senate_Weight ~ Urban, data=Dat))
summary(lm(House_Weight ~ Urban, data=Dat))

assign(x=paste("Data", "States", Yr, sep="_"),
       value=Dat)
save(list=paste("Data", "States", Yr, sep="_"),
     file=paste0("res/",Type,"/Data_States_",Yr,".Rda"))
save()
library(tidyverse)

Names <- c("Pop_RE", "Pop_AgeCat", "Pop_Sex", 
           "Pop_UR", "HH_RO")
Titles <- c("Race/Ethnicity", "Age Category", "Sex", 
            "Urban/Rural", "Housing Status")
names(Titles) <- Names

## The order in which categories will appear for each Name;
### the first category will be used as the referent for relative weights
ColOrders <- list(Pop_RE=c("White", "Hispanic", "Black", "Asian", 
                           "Multiple", "AIAN", "NHOPI", "Other"),
                  Pop_AgeCat=c("0-17", "18-39", "40-64", "65+"),
                  Pop_Sex=c("Female", "Male"),
                  Pop_UR=c("Rural", "Urban", "Urban Cluster", "Urbanized Area"),
                  HH_RO=c("Renter", "Owner: Mortgage", "Owner: Clear", "Owner"))

Cols_Specs <- list(HH_RO_2000=c(1,4),
                  HH_RO_2010=1:3,
                  HH_RO_2020=1:3,
                  Pop_UR_2000=c(1,3:4),
                  Pop_UR_2010=c(1,3:4),
                  Pop_UR_2020=1:2)

CensusYrs <- c(2000, 2010, 2020)
Census_UseCD <- c(FALSE, TRUE, TRUE)
names(Census_UseCD) <- CensusYrs

## Note: if you change these orders, you will need to change the orders 
### in the import files so that the creation of Totals and Proportions matches.
Numerators <- c("EC", "Senate", "House")
Denominators <- c("50 States and DC", "50 States", "50 States, DC, PR")

## Special named vectors for Shiny input
Denom_Titled <- Denominators
names(Denom_Titled) <- c("With DC, Not PR", "Without DC and PR", "With DC and PR")

Num_Titled <- rev(Numerators)
names(Num_Titled) <- c("House", "Senate", "Electoral College")

Names_Titled <- Names
names(Names_Titled) <- Titles

## Trend Plot Scales:
Trend_Scales <- list(House=c(0.970, 1.025),
                     Senate=c(0.55, 2.20),
                     EC=c(0.9, 1.2))
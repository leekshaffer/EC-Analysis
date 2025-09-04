Names <- c("Pop_RE", "Pop_AgeCat", "Pop_Sex", 
           "Pop_UR", "HH_RO")
Titles <- c("Race/Ethnicity", "Age Category", "Sex", 
            "Urban/Rural", "Housing Status")
names(Titles) <- Names
Names_Titled <- Names
names(Names_Titled) <- Titles
Referents <- c("White", "0-17", "Female", "Urban", "Renter")
names(Referents) <- Names

ColOrders <- list(Pop_RE=c("White", "Hispanic", "Black", "Asian", "Multiple", "AIAN", "NHOPI", "Other"),
                  Pop_AgeCat=c("0-17", "18-39", "40-64", "65+"),
                  Pop_Sex=c("Female", "Male"),
                  Pop_UR=c("Rural", "Urban", "Urban Cluster", "Urbanized Area"),
                  HH_RO=c("Renter", "Owner: Mortgage", "Owner: Clear", "Owner"))
                  # HH_RO_Prop_2000=c("Renter", "Owner"),
                  # HH_RO_Prop_2010=c("Renter", "Owner: Mortgage", "Owner: Clear"),
                  # HH_RO_Prop_2020=c("Renter", "Owner: Mortgage", "Owner: Clear"),
                  # Pop_UR_Prop_2000=c("Rural", "Urban Cluster", "Urbanized Area"),
                  # Pop_UR_Prop_2010=c("Rural", "Urban Cluster", "Urbanized Area"),
                  # Pop_UR_Prop_2020=c("Rural", "Urban"))

CensusYrs <- c(2000, 2010, 2020)

## Note: if you change these orders, you will need to change the orders 
### in the Totals and Proportions creating as well
Numerators <- c("EC", "Senate", "House")
Denominators <- c("Population", "Pop: Without DC", "Pop: With PR")

Denom_Titled <- Denominators
names(Denom_Titled) <- c("With DC, Not PR", "Without DC and PR", "With DC and PR")

Num_Titled <- rev(Numerators)
names(Num_Titled) <- c("House", "Senate", "Electoral College")
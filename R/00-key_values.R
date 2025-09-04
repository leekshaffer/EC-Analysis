Names <- c("Pop_RE", "Pop_AgeCat", "Pop_Sex", 
           "Pop_UR", "HH_RO")
Titles <- c("Race/Ethnicity", "Age Category", "Sex", 
            "Urban/Rural", "Housing Status")
names(Titles) <- Names
Referents <- c("White", "0-17", "Female", "Urban", "Renter")
names(Referents) <- Names

ColOrders <- list(Pop_RE=c("White", "Hispanic", "Black", "Asian", "Multiple", "AIAN", "NHOPI", "Other"),
                  Pop_AgeCat=c("0-17", "18-39", "40-64", "65+"),
                  Pop_Sex=c("Female", "Male"),
                  Pop_UR=c("Urban", "Rural","Urbanized Area", "Urban Cluster"),
                  HH_RO=c("Renter", "Owner: Mortgage", "Owner: Clear", "Owner"))
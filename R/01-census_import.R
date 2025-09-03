library(tidycensus)
library(tidyverse)

# v19 <- load_variables(2019, "acs1")
# v23 <- load_variables(2023, "acs1")
# v00sf1 <- load_variables(2000, "sf1")
# v10sf1 <- load_variables(2010, "sf1")
# v20dhc <- load_variables(2020, "dhc")
# 
# save(list=c("v00sf1","v10sf1","v20dhc"),
#      file="int/DecennialCensusVars.Rda")
# 
# save(list=c("v19","v23"),
#      file="int/ACSVars.Rda")


# 2020 Census DHC

## Race/Ethnicity Category 2020:
RE_cols <- c(Total="P11_001N",
             Hispanic="P11_002N",
             White="P11_005N",
             Black="P11_006N",
             AIAN="P11_007N",
             Asian="P11_008N",
             NHOPI="P11_009N",
             Other="P11_010N",
             Multiple="P11_011N")
Adult_RE_2020_State <- get_decennial(geography="state", 
                             variables=RE_cols,
                             year=2020, sumfile="dhc",
                             output="wide")
Adult_RE_2020_CD <- get_decennial(geography="congressional district", 
                               variables=RE_cols,
                               year=2020, sumfile="dhc",
                               output="wide") %>% 
  mutate(State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ")
Adult_RE_2020 <- list(State=Adult_RE_2020_State,
                      CD=Adult_RE_2020_CD,
                    year=2020,
                    VarNames=names(RE_cols),
                    type="get_decennial",
                    sumfile="dhc",
                    variables=RE_cols)
save(Adult_RE_2020,
     file="int/Adult_RE_2020.Rda")

## Race/Ethnicity Full Pop 2020:
RE_pop_cols <- c(Hispanic="P12H_001N",
             White="P12I_001N",
             Black="P12J_001N",
             AIAN="P12K_001N",
             Asian="P12L_001N",
             NHOPI="P12M_001N",
             Other="P12N_001N",
             Multiple="P12O_001N")
Pop_RE_2020_State <- get_decennial(geography="state", 
                                     variables=RE_pop_cols,
                                     year=2020, sumfile="dhc",
                                     output="wide") %>%
  mutate(Total=Hispanic+White+Black+AIAN+Asian+NHOPI+Other+Multiple)
Pop_RE_2020_CD <- get_decennial(geography="congressional district", 
                                  variables=RE_pop_cols,
                                  year=2020, sumfile="dhc",
                                  output="wide") %>% 
  mutate(Total=Hispanic+White+Black+AIAN+Asian+NHOPI+Other+Multiple,
         State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ")
Pop_RE_2020 <- list(State=Pop_RE_2020_State,
                      CD=Pop_RE_2020_CD,
                      year=2020,
                      VarNames=c("Total",names(RE_pop_cols)),
                      type="get_decennial",
                      sumfile="dhc",
                      variables=RE_cols)
save(Pop_RE_2020,
     file="int/Pop_RE_2020.Rda")

## Urban/Rural 2020:
UR_cols <- c(Total="P2_001N",
             Urban="P2_002N",
             Rural="P2_003N")
Pop_UR_2020_State <- get_decennial(geography="state", 
                                   variables=UR_cols,
                                   year=2020, sumfile="dhc",
                                   output="wide")
Pop_UR_2020_CD <- get_decennial(geography="congressional district", 
                                variables=UR_cols,
                                year=2020, sumfile="dhc",
                                output="wide") %>% 
  mutate(State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ")
Pop_UR_2020 <- list(State=Pop_UR_2020_State,
                    CD=Pop_UR_2020_CD,
                    year=2020,
                    VarNames=names(UR_cols),
                    type="get_decennial",
                    sumfile="dhc",
                    variables=UR_cols)
save(Pop_UR_2020,
     file="int/Pop_UR_2020.Rda")


## Sex by Age 2020:
SA_cols <- c(Total="P12_001N",
             Male_0_4="P12_003N",
             Male_5_9="P12_004N",
             Male_10_14="P12_005N",
             Male_15_17="P12_006N",
             Male_18_19="P12_007N",
             Male_20_20="P12_008N",
             Male_21_21="P12_009N",
             Male_22_24="P12_010N",
             Male_25_29="P12_011N",
             Male_30_34="P12_012N",
             Male_35_39="P12_013N",
             Male_40_44="P12_014N",
             Male_45_49="P12_015N",
             Male_50_54="P12_016N",
             Male_55_59="P12_017N",
             Male_60_61="P12_018N",
             Male_62_64="P12_019N",
             Male_65_66="P12_020N",
             Male_67_69="P12_021N",
             Male_70_74="P12_022N",
             Male_75_79="P12_023N",
             Male_80_84="P12_024N",
             Male_85_120="P12_025N",
             Female_0_4="P12_027N",
             Female_5_9="P12_028N",
             Female_10_14="P12_029N",
             Female_15_17="P12_030N",
             Female_18_19="P12_031N",
             Female_20_20="P12_032N",
             Female_21_21="P12_033N",
             Female_22_24="P12_034N",
             Female_25_29="P12_035N",
             Female_30_34="P12_036N",
             Female_35_39="P12_037N",
             Female_40_44="P12_038N",
             Female_45_49="P12_039N",
             Female_50_54="P12_040N",
             Female_55_59="P12_041N",
             Female_60_61="P12_042N",
             Female_62_64="P12_043N",
             Female_65_66="P12_044N",
             Female_67_69="P12_045N",
             Female_70_74="P12_046N",
             Female_75_79="P12_047N",
             Female_80_84="P12_048N",
             Female_85_120="P12_049N")
Pop_SA_2020_State <- get_decennial(geography="state", 
                             variables=SA_cols,
                             year=2020, sumfile="dhc",
                             output="wide") %>% 
  pivot_longer(cols=c(starts_with("Male"),starts_with("Female")),
               names_sep="_",
               names_to=c("Sex","Min","Max"),
               values_to="Pop") %>%
  dplyr::mutate(Min=as.numeric(Min), Max=as.numeric(Max))
Pop_Sex_2020_State <- Pop_SA_2020_State %>% group_by(GEOID,NAME,Total,Sex) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total"),
              names_from="Sex", values_from="Pop")
Pop_Age_2020_State <- Pop_SA_2020_State %>%
  mutate(AgeCat=if_else(Min >= 10 & Max < 18, "Age_10_17",
                        if_else(Min >= 18 & Max < 25, "Age_18_24",
                                if_else(Min >= 85, "Age_85+",
                                        paste("Age",floor(Min/5)*5,floor(Min/5)*5+4, sep="_")))),
         AgeMin=if_else(Min >= 18 & Max < 25, 18,
                        if_else(Min >= 10 & Max < 18, 10,
                                floor(Min/5)*5))) %>%
  group_by(GEOID,NAME,Total,AgeMin,AgeCat) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total"),
              names_from="AgeCat", values_from="Pop")
Pop_AgeCat_2020_State <- Pop_SA_2020_State %>%
  mutate(AgeCat=if_else(Max < 18, "0-17",
                        if_else(Max < 40, "18-39",
                                if_else(Max < 65, "40-64", "65+")))) %>%
  group_by(GEOID,NAME,Total,AgeCat) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total"),
              names_from="AgeCat", values_from="Pop")
  
Adult_Age_2020_State <- Pop_Age_2020_State %>% 
  dplyr::select(-c("Age_0_4","Age_5_9","Age_10_17")) %>%
  mutate(Total=rowSums(across(starts_with("Age"))))

Adult_AgeCat_2020_State <- Pop_AgeCat_2020_State %>%
  dplyr::select(-c("0-17")) %>%
  mutate(Total=`18-39`+`40-64`+`65+`)

Pop_SA_2020_CD <- get_decennial(geography="congressional district", 
                             variables=SA_cols,
                             year=2020, sumfile="dhc",
                             output="wide") %>% 
  mutate(State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ") %>% 
  pivot_longer(cols=c(starts_with("Male"),starts_with("Female")),
               names_sep="_",
               names_to=c("Sex","Min","Max"),
               values_to="Pop") %>%
  dplyr::mutate(Min=as.numeric(Min), Max=as.numeric(Max))
Pop_Sex_2020_CD <- Pop_SA_2020_CD %>% 
  group_by(GEOID,NAME,Total,State,CD,Sex) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total","State","CD"),
              names_from="Sex", values_from="Pop")
Pop_Age_2020_CD <- Pop_SA_2020_CD %>%
  mutate(AgeCat=if_else(Min >= 10 & Max < 18, "Age_10_17",
                        if_else(Min >= 18 & Max < 25, "Age_18_24",
                                if_else(Min >= 85, "Age_85+",
                                        paste("Age",floor(Min/5)*5,floor(Min/5)*5+4, sep="_")))),
         AgeMin=if_else(Min >= 18 & Max < 25, 18,
                        if_else(Min >= 10 & Max < 18, 10,
                                floor(Min/5)*5))) %>%
  group_by(GEOID,NAME,Total,State,CD,AgeMin,AgeCat) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total","State","CD"),
              names_from="AgeCat", values_from="Pop")

Pop_AgeCat_2020_CD <- Pop_SA_2020_CD %>%
  mutate(AgeCat=if_else(Max < 18, "0-17",
                        if_else(Max < 40, "18-39",
                                if_else(Max < 65, "40-64", "65+")))) %>%
  group_by(GEOID,NAME,Total,State,CD,AgeCat) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total","State","CD"),
              names_from="AgeCat", values_from="Pop")

Adult_Age_2020_CD <- Pop_Age_2020_CD %>% 
  dplyr::select(-c("Age_0_4","Age_5_9","Age_10_17")) %>%
  mutate(Total=rowSums(across(starts_with("Age"))))

Adult_AgeCat_2020_CD <- Pop_AgeCat_2020_CD %>%
  dplyr::select(-c("0-17")) %>%
  mutate(Total=`18-39`+`40-64`+`65+`)

Pop_Sex_2020 <- list(State=Pop_Sex_2020_State,
                     CD=Pop_Sex_2020_CD,
                     year=2020,
                     VarNames=c("Total","Female","Male"),
                     type="get_decennial",
                     sumfile="dhc",
                     variables=SA_cols)
save(Pop_Sex_2020,
     file="int/Pop_Sex_2020.Rda")

Pop_Age_2020 <- list(State=Pop_Age_2020_State,
                     CD=Pop_Age_2020_CD,
                     year=2020,
                     VarNames=colnames(Pop_Age_2020_State)[3:20],
                     type="get_decennial",
                     sumfile="dhc",
                     variables=SA_cols)
save(Pop_Age_2020,
     file="int/Pop_Age_2020.Rda")
Adult_Age_2020 <- list(State=Adult_Age_2020_State,
                       CD=Adult_Age_2020_CD,
                       year=2020,
                       VarNames=colnames(Adult_Age_2020_State)[3:17],
                       type="get_decennial",
                       sumfile="dhc",
                       variables=SA_cols)
save(Adult_Age_2020,
     file="int/Adult_Age_2020.Rda")

Pop_AgeCat_2020 <- list(State=Pop_AgeCat_2020_State,
                     CD=Pop_AgeCat_2020_CD,
                     year=2020,
                     VarNames=colnames(Pop_AgeCat_2020_State)[3:7],
                     type="get_decennial",
                     sumfile="dhc",
                     variables=SA_cols)
save(Pop_AgeCat_2020,
     file="int/Pop_AgeCat_2020.Rda")
Adult_AgeCat_2020 <- list(State=Adult_AgeCat_2020_State,
                       CD=Adult_AgeCat_2020_CD,
                       year=2020,
                       VarNames=colnames(Adult_AgeCat_2020_State)[3:6],
                       type="get_decennial",
                       sumfile="dhc",
                       variables=SA_cols)
save(Adult_AgeCat_2020,
     file="int/Adult_AgeCat_2020.Rda")


## Renter/Owner 2020:
RO_cols <- c(Total="H4_001N",
             `Owner: Mortgage`="H4_002N",
             `Owner: Clear`="H4_003N",
             Renter="H4_004N")
HH_RO_2020_State <- get_decennial(geography="state", 
                             variables=RO_cols,
                             year=2020, sumfile="dhc",
                             output="wide") %>%
  mutate(Owner=`Owner: Mortgage`+`Owner: Clear`)
HH_RO_2020_CD <- get_decennial(geography="congressional district", 
                            variables=RO_cols,
                            year=2020, sumfile="dhc",
                            output="wide") %>% 
  mutate(Owner=`Owner: Mortgage`+`Owner: Clear`,
         State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ")
HH_RO_2020 <- list(State=HH_RO_2020_State,
                   CD=HH_RO_2020_CD,
                    year=2020,
                    VarNames=c(names(RO_cols),"Owner"),
                    type="get_decennial",
                    sumfile="dhc",
                    variables=RO_cols)
save(HH_RO_2020,
     file="int/HH_RO_2020.Rda")

# 2010 and 2000 Census SF1

## Owner vs. Renter:

RO_2010 <- c(Total="H004001",
             `Owner: Mortgage`="H004002",
             `Owner: Clear`="H004003",
             Renter="H004004")
RO_2000 <- c(Total="H004001",
             Owner="H004002",
             Renter="H004003")
# Note: 2000 doesn't have mortgage vs. owned free & clear distinction

HH_RO_2010_CD <- get_decennial(geography="congressional district", 
                                variables=RO_2010,
                                year=2010, sumfile="sf1",
                                output="wide") %>% 
  mutate(Owner=`Owner: Mortgage` + `Owner: Clear`,
         State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ")
HH_RO_2010 <- list(CD=HH_RO_2010_CD,
                   year=2010,
                   VarNames=c(names(RO_2010),"Owner"),
                   type="get_decennial",
                   sumfile="sf1",
                   variables=RO_2010)
save(HH_RO_2010,
     file="int/HH_RO_2010.Rda")

HH_RO_2000_State <- get_decennial(geography="state", 
                               variables=RO_2000,
                               year=2000, sumfile="sf1",
                               output="wide")
HH_RO_2000 <- list(State=HH_RO_2000_State,
                   year=2000,
                   VarNames=names(RO_2000),
                   type="get_decennial",
                   sumfile="sf1",
                   variables=RO_2000)
save(HH_RO_2000,
     file="int/HH_RO_2000.Rda")

## Urban vs. Rural:
UR_2010 <- c(Total="P002001",
             Urban="P002002",
             Rural="P002005",
             `Urbanized Area`="P002003",
             `Urban Cluster`="P002004")

Pop_UR_2010_CD <- get_decennial(geography="congressional district", 
                               variables=UR_2010,
                               year=2010, sumfile="sf1",
                               output="wide") %>% 
  mutate(State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ")
Pop_UR_2010 <- list(CD=Pop_UR_2010_CD,
                   year=2010,
                   VarNames=names(UR_2010),
                   type="get_decennial",
                   sumfile="sf1",
                   variables=UR_2010)
save(Pop_UR_2010,
     file="int/Pop_UR_2010.Rda")

Pop_UR_2000_State <- get_decennial(geography="state", 
                                variables=UR_2010,
                                year=2000, sumfile="sf1",
                                output="wide")
Pop_UR_2000 <- list(State=Pop_UR_2000_State,
                    year=2000,
                    VarNames=names(UR_2010),
                    type="get_decennial",
                    sumfile="sf1",
                    variables=UR_2010)
save(Pop_UR_2000,
     file="int/Pop_UR_2000.Rda")

## Race and Ethnicity:
RE_2010 <- c(Total="P005001",
             Hispanic="P005010",
             White="P005003",
             Black="P005004",
             AIAN="P005005",
             Asian="P005006",
             NHOPI="P005007",
             Other="P005008",
             Multiple="P005009")

RE_2000 <- c(Total="P004001",
             Hispanic="P004002",
             White="P004005",
             Black="P004006",
             AIAN="P004007",
             Asian="P004008",
             NHOPI="P004009",
             Other="P004010",
             Multiple="P004011")

Pop_RE_2010_CD <- get_decennial(geography="congressional district", 
                                variables=RE_2010,
                                year=2010, sumfile="sf1",
                                output="wide") %>% 
  mutate(State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ")
Pop_RE_2010 <- list(CD=Pop_RE_2010_CD,
                    year=2010,
                    VarNames=names(RE_2010),
                    type="get_decennial",
                    sumfile="sf1",
                    variables=RE_2010)
save(Pop_RE_2010,
     file="int/Pop_RE_2010.Rda")

Pop_RE_2000_State <- get_decennial(geography="state", 
                                variables=RE_2000,
                                year=2000, sumfile="sf1",
                                output="wide")
Pop_RE_2000 <- list(State=Pop_RE_2000_State,
                    year=2000,
                    VarNames=names(RE_2000),
                    type="get_decennial",
                    sumfile="sf1",
                    variables=RE_2000)
save(Pop_RE_2000,
     file="int/Pop_RE_2000.Rda")

## Sex and Age:
Sex_2010 <- c(Total="P012001",
              Male="P012002",
              Female="P012026")
Pop_Sex_2010_CD <- get_decennial(geography="congressional district",
                                 variables=Sex_2010,
                                 year=2010, sumfile="sf1",
                                 output="wide") %>% 
  mutate(State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ")
Pop_Sex_2010 <- list(CD=Pop_Sex_2010_CD,
                    year=2010,
                    VarNames=names(Sex_2010),
                    type="get_decennial",
                    sumfile="sf1",
                    variables=Sex_2010)
save(Pop_Sex_2010,
     file="int/Pop_Sex_2010.Rda")

Pop_Sex_2000_State <- get_decennial(geography="state",
                                 variables=Sex_2010,
                                 year=2000, sumfile="sf1",
                                 output="wide")
Pop_Sex_2000 <- list(State=Pop_Sex_2000_State,
                     year=2000,
                     VarNames=names(Sex_2010),
                     type="get_decennial",
                     sumfile="sf1",
                     variables=Sex_2010)
save(Pop_Sex_2000,
     file="int/Pop_Sex_2000.Rda")

SA_2010 <- c(Total="P012001",
                        Male_0_4="P012003",
                        Male_5_9="P012004",
                        Male_10_14="P012005",
                        Male_15_17="P012006",
                        Male_18_19="P012007",
                        Male_20_20="P012008",
                        Male_21_21="P012009",
                        Male_22_24="P012010",
                        Male_25_29="P012011",
                        Male_30_34="P012012",
                        Male_35_39="P012013",
                        Male_40_44="P012014",
                        Male_45_49="P012015",
                        Male_50_54="P012016",
                        Male_55_59="P012017",
                        Male_60_61="P012018",
                        Male_62_64="P012019",
                        Male_65_66="P012020",
                        Male_67_69="P012021",
                        Male_70_74="P012022",
                        Male_75_79="P012023",
                        Male_80_84="P012024",
                        Male_85_120="P012025",
                        Female_0_4="P012027",
                        Female_5_9="P012028",
                        Female_10_14="P012029",
                        Female_15_17="P012030",
                        Female_18_19="P012031",
                        Female_20_20="P012032",
                        Female_21_21="P012033",
                        Female_22_24="P012034",
                        Female_25_29="P012035",
                        Female_30_34="P012036",
                        Female_35_39="P012037",
                        Female_40_44="P012038",
                        Female_45_49="P012039",
                        Female_50_54="P012040",
                        Female_55_59="P012041",
                        Female_60_61="P012042",
                        Female_62_64="P012043",
                        Female_65_66="P012044",
                        Female_67_69="P012045",
                        Female_70_74="P012046",
                        Female_75_79="P012047",
                        Female_80_84="P012048",
                        Female_85_120="P012049")

Pop_SA_2010_CD <- get_decennial(geography="congressional district", 
                                variables=SA_2010,
                                year=2010, sumfile="sf1",
                                output="wide") %>% 
  mutate(State=str_split_i(NAME, ", ", 2),
         CD=substr(GEOID, 3, 4)) %>%
  dplyr::filter(CD != "ZZ") %>% 
  pivot_longer(cols=c(starts_with("Male"),starts_with("Female")),
               names_sep="_",
               names_to=c("Sex","Min","Max"),
               values_to="Pop") %>%
  dplyr::mutate(Min=as.numeric(Min), Max=as.numeric(Max))
Pop_Age_2010_CD <- Pop_SA_2010_CD %>%
  mutate(AgeCat=if_else(Min >= 10 & Max < 18, "Age_10_17",
                        if_else(Min >= 18 & Max < 25, "Age_18_24",
                                if_else(Min >= 85, "Age_85+",
                                        paste("Age",floor(Min/5)*5,floor(Min/5)*5+4, sep="_")))),
         AgeMin=if_else(Min >= 18 & Max < 25, 18,
                        if_else(Min >= 10 & Max < 18, 10,
                                floor(Min/5)*5))) %>%
  group_by(GEOID,NAME,Total,State,CD,AgeMin,AgeCat) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total","State","CD"),
              names_from="AgeCat", values_from="Pop")
Pop_AgeCat_2010_CD <- Pop_SA_2010_CD %>%
  mutate(AgeCat=if_else(Max < 18, "0-17",
                        if_else(Max < 40, "18-39",
                                if_else(Max < 65, "40-64", "65+")))) %>%
  group_by(GEOID,NAME,Total,State,CD,AgeCat) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total","State","CD"),
              names_from="AgeCat", values_from="Pop")
Pop_AgeCat_2010 <- list(CD=Pop_AgeCat_2010_CD,
                        year=2010,
                        VarNames=colnames(Pop_AgeCat_2010_CD)[c(3,6:9)],
                        type="get_decennial",
                        sumfile="sf1",
                        variables=SA_2010)
save(Pop_AgeCat_2010,
     file="int/Pop_AgeCat_2010.Rda")
Pop_Age_2010 <- list(CD=Pop_Age_2010_CD,
                        year=2010,
                        VarNames=colnames(Pop_Age_2010_CD)[c(3,6:22)],
                        type="get_decennial",
                        sumfile="sf1",
                        variables=SA_2010)
save(Pop_Age_2010,
     file="int/Pop_Age_2010.Rda")

Pop_SA_2000_State <- get_decennial(geography="state", 
                                variables=SA_2010,
                                year=2000, sumfile="sf1",
                                output="wide") %>% 
  pivot_longer(cols=c(starts_with("Male"),starts_with("Female")),
               names_sep="_",
               names_to=c("Sex","Min","Max"),
               values_to="Pop") %>%
  dplyr::mutate(Min=as.numeric(Min), Max=as.numeric(Max))
Pop_Age_2000_State <- Pop_SA_2000_State %>%
  mutate(AgeCat=if_else(Min >= 10 & Max < 18, "Age_10_17",
                        if_else(Min >= 18 & Max < 25, "Age_18_24",
                                if_else(Min >= 85, "Age_85+",
                                        paste("Age",floor(Min/5)*5,floor(Min/5)*5+4, sep="_")))),
         AgeMin=if_else(Min >= 18 & Max < 25, 18,
                        if_else(Min >= 10 & Max < 18, 10,
                                floor(Min/5)*5))) %>%
  group_by(GEOID,NAME,Total,AgeMin,AgeCat) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total"),
              names_from="AgeCat", values_from="Pop")
Pop_AgeCat_2000_State <- Pop_SA_2000_State %>%
  mutate(AgeCat=if_else(Max < 18, "0-17",
                        if_else(Max < 40, "18-39",
                                if_else(Max < 65, "40-64", "65+")))) %>%
  group_by(GEOID,NAME,Total,AgeCat) %>%
  dplyr::summarize(Pop=sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(id_cols=c("GEOID","NAME","Total"),
              names_from="AgeCat", values_from="Pop")
Pop_AgeCat_2000 <- list(State=Pop_AgeCat_2000_State,
                        year=2000,
                        VarNames=colnames(Pop_AgeCat_2000_State)[3:7],
                        type="get_decennial",
                        sumfile="sf1",
                        variables=SA_2010)
save(Pop_AgeCat_2000,
     file="int/Pop_AgeCat_2000.Rda")
Pop_Age_2000 <- list(State=Pop_Age_2000_State,
                     year=2000,
                     VarNames=colnames(Pop_Age_2000_State)[3:20],
                     type="get_decennial",
                     sumfile="sf1",
                     variables=SA_2010)
save(Pop_Age_2000,
     file="int/Pop_Age_2000.Rda")


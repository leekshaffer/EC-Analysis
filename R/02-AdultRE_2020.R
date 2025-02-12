require(tidyverse)
load("int/Apportion.Rda")

load("int/Adult18_byRE_2020.Rda")
Group_Cols <- c("Hisp","White","Black","AIAN","Asian","NHOPI","Other","Multi")
A_RE_20 <- Adult18_byRE_2020 %>%
  mutate(Year=2020) %>%
  mutate(across(all_of(Group_Cols), ~.x/Total)) %>%
  dplyr::select(-c("NHisp","OneRace")) %>%
  left_join(EC, by=join_by(NAME, Year)) %>%
  left_join(House, by=join_by(NAME, Year, State, Abbrev))

Results <- bind_cols(tibble(Result=c("Population","PopEx0",
                                         "House","EC")),
                     bind_rows(apply(A_RE_20 %>% dplyr::select(all_of(Group_Cols)), 2, 
                                     FUN=function(x) sum(x*A_RE_20$Total)/sum(A_RE_20$Total)),
                           apply(A_RE_20 %>% dplyr::filter(House > 0) %>% 
                                   dplyr::select(all_of(Group_Cols)), 2, 
                                 FUN=function(x) sum(x*A_RE_20 %>% dplyr::filter(House > 0) %>% pull(Total))/
                                   sum(A_RE_20 %>% dplyr::filter(House > 0) %>% pull(Total))),
                           apply(A_RE_20 %>% dplyr::select(all_of(Group_Cols)), 2, 
                                 FUN=function(x) sum(x*A_RE_20$House)/sum(A_RE_20$House)),
                           apply(A_RE_20 %>% dplyr::select(all_of(Group_Cols)), 2, 
                                 FUN=function(x) sum(x*A_RE_20$EC)/sum(A_RE_20$EC)))) %>% 
  pivot_longer(cols=!c("Result"), names_to="Group") %>% 
  pivot_wider(id_cols="Group", names_from="Result", values_from="value") %>%
  dplyr::mutate(HP=House/Population, HPex0=House/PopEx0,
                ECP=EC/Population, ECPex0=EC/PopEx0)

Analyze_List <- function(Name) {
  load(file=paste0("int/",Name,".Rda"))
  Obj <- get(Name)
  Group_Cols <- Obj$VarNames
  
  dat <- Obj$data  %>%
    mutate(Year=Obj$year) %>%
    mutate(across(all_of(Group_Cols), ~.x/Total)) %>%
    left_join(EC, by=join_by(NAME, Year)) %>%
    left_join(House, by=join_by(NAME, Year, State, Abbrev))
  
  Results <- bind_cols(tibble(Result=c("Population","PopEx0",
                                       "House","EC")),
                       bind_rows(apply(dat %>% dplyr::select(all_of(Group_Cols)), 2, 
                                       FUN=function(x) sum(x*dat$Total)/sum(dat$Total)),
                                 apply(dat %>% dplyr::filter(House > 0) %>% 
                                         dplyr::select(all_of(Group_Cols)), 2, 
                                       FUN=function(x) sum(x*dat %>% dplyr::filter(House > 0) %>% pull(Total))/
                                         sum(dat %>% dplyr::filter(House > 0) %>% pull(Total))),
                                 apply(dat %>% dplyr::select(all_of(Group_Cols)), 2, 
                                       FUN=function(x) sum(x*dat$House)/sum(dat$House)),
                                 apply(dat %>% dplyr::select(all_of(Group_Cols)), 2, 
                                       FUN=function(x) sum(x*dat$EC)/sum(dat$EC)))) %>% 
    pivot_longer(cols=!c("Result"), names_to="Group") %>% 
    pivot_wider(id_cols="Group", names_from="Result", values_from="value") %>%
    dplyr::mutate(HP=House/Population, HPex0=House/PopEx0,
                  ECP=EC/Population, ECPex0=EC/PopEx0)
  return(list(Results=Results,
              Data=dat))
}

Adult_RE_2020_Res <- Analyze_List("Adult_RE_2020")
Pop_UR_2020_Res <- Analyze_List("Pop_UR_2020")


load(file="int/Pop_UR_2020.Rda")
Obj <- Pop_UR_2020
Group_Cols <- Obj$VarNames

dat <- Obj$data  %>%
  mutate(Year=Obj$year) %>%
  mutate(across(all_of(Group_Cols), ~.x/Total)) %>%
  left_join(EC, by=join_by(NAME, Year)) %>%
  left_join(House, by=join_by(NAME, Year, State, Abbrev))

Results <- bind_cols(tibble(Result=c("Population","PopEx0",
                                     "House","EC")),
                     bind_rows(apply(dat %>% dplyr::select(all_of(Group_Cols)), 2, 
                                     FUN=function(x) sum(x*dat$Total)/sum(dat$Total)),
                               apply(dat %>% dplyr::filter(House > 0) %>% 
                                       dplyr::select(all_of(Group_Cols)), 2, 
                                     FUN=function(x) sum(x*dat %>% dplyr::filter(House > 0) %>% pull(Total))/
                                       sum(dat %>% dplyr::filter(House > 0) %>% pull(Total))),
                               apply(dat %>% dplyr::select(all_of(Group_Cols)), 2, 
                                     FUN=function(x) sum(x*dat$House)/sum(dat$House)),
                               apply(dat %>% dplyr::select(all_of(Group_Cols)), 2, 
                                     FUN=function(x) sum(x*dat$EC)/sum(dat$EC)))) %>% 
  pivot_longer(cols=!c("Result"), names_to="Group") %>% 
  pivot_wider(id_cols="Group", names_from="Result", values_from="value") %>%
  dplyr::mutate(HP=House/Population, HPex0=House/PopEx0,
                ECP=EC/Population, ECPex0=EC/PopEx0)

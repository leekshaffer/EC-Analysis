require(tidyverse)
load("int/Apportion.Rda")

Analyze_List <- function(Name) {
  load(file=paste0("int/",Name,".Rda"))
  Obj <- get(Name)
  Group_Cols <- Obj$VarNames
  
  dat <- Obj$data  %>%
    mutate(Year=Obj$year) %>%
    mutate(across(all_of(Group_Cols[Group_Cols != "Total"]), ~.x/Total)) %>%
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
  assign(x=Name,
         value=list(Results=Results, Data=dat))
  save(list=Name,
       file=paste0("res/",Name,".Rda"))
  return(list(Results=Results,
              Data=dat))
}

Adult_RE_2020_Res <- Analyze_List("Adult_RE_2020")
Pop_UR_2020_Res <- Analyze_List("Pop_UR_2020")
Pop_Sex_2020_Res <- Analyze_List("Pop_Sex_2020")
Pop_Age_2020_Res <- Analyze_List("Pop_Age_2020")
Adult_Age_2020_Res <- Analyze_List("Adult_Age_2020")
HH_RO_2020 <- Analyze_List("HH_RO_2020")

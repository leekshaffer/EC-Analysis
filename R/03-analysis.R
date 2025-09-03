library(tidyverse)
load("int/Apportion.Rda")

Analyze_List <- function(Name, Use_CD=TRUE, Referent=NULL) {
  load(file=paste0("int/",Name,".Rda"))
  Obj <- get(Name)
  Group_Cols <- Obj$VarNames
  Group_Cols_Prop <- paste0(Group_Cols, "_Prop")
  
  Rows <- c("Population", "Pop: Without DC", "Pop: With PR", 
            "House", "House: With DC",
            "Senate", "EC")
  
  if (Use_CD) {
    CD <- Obj$CD %>%
      mutate(Year=Obj$year) %>%
      left_join(Abb %>% rename(Postal=State, State=NAME), by="State") %>%
      mutate(across(all_of(Group_Cols), 
                    ~.x/Total,
                    .names="{paste(col,'Prop',sep='_')}"))
    
    State_Summ <- CD %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>% 
      group_by(State) %>% 
      dplyr::summarize(across(all_of(Group_Cols), 
                              ~sum(.x)/sum(Total), 
                              .names="{.col}_Prop")) %>%
      ungroup()
    
    EC_Summ <- CD %>% dplyr::filter(Postal != "PR") %>% 
      group_by(State) %>% 
      dplyr::summarize(across(all_of(Group_Cols), 
                              ~sum(.x)/sum(Total), 
                              .names="{.col}_Prop"),
                       N=n()) %>%
      ungroup() %>%
      mutate(EC=2+if_else(State %in% c("Maine","Nebraska"), 0, N)) %>%
      dplyr::select(-N) %>%
      bind_rows(CD %>% dplyr::filter(State %in% c("Maine","Nebraska")) %>%
                  dplyr::mutate(State=paste(State,CD,sep="-"),
                                EC=1) %>%
                  dplyr::select(c("State",all_of(Group_Cols_Prop),"EC")))
    
    Totals <- tibble(Analysis=Rows[1:3]) %>%
      bind_cols(bind_rows(CD %>% dplyr::filter(Postal != "PR") %>%
                            dplyr::summarize(across(all_of(Group_Cols),
                                                    ~sum(.x),
                                                    .names="{.col}")),
                          CD %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                            dplyr::summarize(across(all_of(Group_Cols),
                                                    ~sum(.x),
                                                    .names="{.col}")),
                          CD %>% dplyr::summarize(across(all_of(Group_Cols),
                                                         ~sum(.x),
                                                         .names="{.col}"))))
    
    Proportions <- tibble(Analysis=Rows) %>% 
      bind_cols(bind_rows(CD %>% dplyr::filter(Postal != "PR") %>%
                                        dplyr::summarize(across(all_of(Group_Cols),
                                                                ~sum(.x)/sum(Total),
                                                                .names="{.col}_Prop")),
                             CD %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                                        dplyr::summarize(across(all_of(Group_Cols),
                                                                ~sum(.x)/sum(Total),
                                                                .names="{.col}_Prop")),
                             CD %>% dplyr::summarize(across(all_of(Group_Cols),
                                                            ~sum(.x)/sum(Total),
                                                            .names="{.col}_Prop")),
                             CD %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                                        dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                                ~mean(.x))),
                             CD %>% dplyr::filter(Postal != "PR") %>%
                               dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                       ~mean(.x))),
                             State_Summ %>%
                               dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                       ~mean(.x))),
                             EC_Summ %>%
                               dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                       ~sum(.x*EC)/sum(EC)))))
  } else {
    State <- Obj$State  %>%
      mutate(Year=Obj$year) %>%
      mutate(across(all_of(Group_Cols),
                    ~.x/Total,
                    .names="{paste(col,'Prop',sep='_')}")) %>%
      left_join(EC_State, by=join_by(NAME, Year)) %>%
      left_join(House, by=join_by(NAME, Year, State, Abbrev)) %>%
      mutate(Senate=if_else(NAME %in% c("District of Columbia", "Puerto Rico"), 0, 2)) %>%
      mutate(Postal=State)
    
    Totals <- tibble(Analysis=Rows[1:3]) %>%
      bind_cols(bind_rows(State %>% dplyr::filter(Postal != "PR") %>%
                            dplyr::summarize(across(all_of(Group_Cols),
                                                    ~sum(.x),
                                                    .names="{.col}")),
                          State %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                            dplyr::summarize(across(all_of(Group_Cols),
                                                    ~sum(.x),
                                                    .names="{.col}")),
                          State %>% dplyr::summarize(across(all_of(Group_Cols),
                                                         ~sum(.x),
                                                         .names="{.col}"))))
    
    Proportions <- tibble(Analysis=Rows) %>% 
      bind_cols(bind_rows(State %>% dplyr::filter(Postal != "PR") %>%
                            dplyr::summarize(across(all_of(Group_Cols),
                                                    ~sum(.x)/sum(Total),
                                                    .names="{.col}_Prop")),
                          State %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                            dplyr::summarize(across(all_of(Group_Cols),
                                                    ~sum(.x)/sum(Total),
                                                    .names="{.col}_Prop")),
                          State %>% dplyr::summarize(across(all_of(Group_Cols),
                                                         ~sum(.x)/sum(Total),
                                                         .names="{.col}_Prop")),
                          State %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~sum(.x*House)/sum(House))),
                          State %>% dplyr::filter(Postal != "PR") %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~sum(.x*House)/sum(House))),
                          State %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~sum(.x*Senate)/sum(Senate))),
                          State %>% dplyr::filter(Postal != "PR") %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~sum(.x*EC)/sum(EC)))))
  }
  
  Combos <- as_tibble(expand.grid(Rows[4:7],
                                   Rows[1:3]),
                      .name_repair="minimal") %>%
    mutate(Numerator=as.character(Var1), Denominator=as.character(Var2)) %>%
    dplyr::select(Numerator, Denominator)
  
  Weights <- bind_cols(Combos,
                       as_tibble(t(apply(Combos, 1, 
                   FUN=function(x) unlist((Proportions %>% dplyr::filter(Analysis==x["Numerator"]) %>% dplyr::select(-Analysis))/
                     (Proportions %>% dplyr::filter(Analysis==x["Denominator"]) %>% dplyr::select(-Analysis))))),
                   .name_repair="minimal")) %>%
    dplyr::rename_with(~str_remove(., "_Prop$"))
  
  if (is.null(Referent)) {
    Referent <- (Group_Cols[Group_Cols != "Total"])[which.max(as.numeric(
      Proportions[Proportions$Analysis=="Population", 
                  Group_Cols_Prop[Group_Cols_Prop != "Total_Prop"]]))]
    ## Gets the column with the max proportion as the referent
  }
  
  Rel_Weights <- Weights %>% 
    dplyr::mutate(Reference=get(Referent)) %>%
    dplyr::mutate(across(all_of(Group_Cols),
                         ~.x/Reference)) %>%
    dplyr::select(-c("Total","Reference"))
  
  Excess_Pops <- bind_cols(Combos,
                                 as_tibble(t(apply(Weights, 1, 
                                         function(x) (as.numeric(x[Group_Cols])-1)*
                                           as.numeric(Totals[Totals$Analysis==unlist(x["Denominator"]),Group_Cols]))),
                                         .name_repair="minimal"))
  colnames(Excess_Pops) <- colnames(Weights)
  
  Res <- list(Proportions=Proportions, 
              Weights=Weights,
              Referent=Referent,
              Relative=Rel_Weights,
              Excess=Excess_Pops)
  assign(x=paste(Name, "Res", sep="_"),
         value=Res)
  save(list=paste(Name, "Res", sep="_"),
       file=paste0("res/",Name,".Rda"))
    
  return(Res)
  
  # Results <- bind_cols(tibble(Result=c("Population","PopEx0",
  #                                      "House","EC","Total","TotalEx0")),
  #                      bind_rows(apply(dat %>% dplyr::select(all_of(paste(Group_Cols, "Prop", sep="_"))), 2, 
  #                                      FUN=function(x) sum(x*dat$Total)/sum(dat$Total)),
  #                                apply(dat %>% dplyr::filter(House > 0) %>% 
  #                                        dplyr::select(all_of(paste(Group_Cols, "Prop", sep="_"))), 2, 
  #                                      FUN=function(x) sum(x*dat %>% dplyr::filter(House > 0) %>% pull(Total))/
  #                                        sum(dat %>% dplyr::filter(House > 0) %>% pull(Total))),
  #                                apply(dat %>% dplyr::select(all_of(paste(Group_Cols, "Prop", sep="_"))), 2, 
  #                                      FUN=function(x) sum(x*dat$House)/sum(dat$House)),
  #                                apply(dat %>% dplyr::select(all_of(paste(Group_Cols, "Prop", sep="_"))), 2, 
  #                                      FUN=function(x) sum(x*dat$EC)/sum(dat$EC)),
  #                                apply(dat %>% dplyr::select(all_of(Group_Cols)), 2,
  #                                      FUN=function(x) sum(x)),
  #                                apply(dat %>% dplyr::filter(House > 0) %>% dplyr::select(all_of(Group_Cols)), 2,
  #                                      FUN=function(x) sum(x)))) %>%
  #   pivot_longer(cols=!c("Result"), names_to="Group") %>% 
  #   dplyr::filter(!is.na(value)) %>%
  #   dplyr::mutate(Group=sub("_Prop","",Group)) %>%
  #   pivot_wider(id_cols="Group", names_from="Result", values_from="value") %>%
  #   dplyr::mutate(HP=House/Population, HPEx0=House/PopEx0,
  #                 ECP=EC/Population, ECPEx0=EC/PopEx0,
  #                 ECP_ExtraPop=(ECP-1)*Total,
  #                 ECPEx0_ExtraPop=(ECPEx0-1)*TotalEx0)
  # assign(x=Name,
  #        value=list(Results=Results, Data=dat))
  # return(list(Results=Results,
  #             Data=dat))
}

HH_RO_2020_Res <- Analyze_List("HH_RO_2020", Use_CD=TRUE,
                               Referent="Renter")
Pop_RE_2020_Res <- Analyze_List("Pop_RE_2020", Use_CD=TRUE,
                                Referent="White")
Pop_UR_2020_Res <- Analyze_List("Pop_UR_2020", Use_CD=TRUE,
                                Referent="Urban")
Pop_Sex_2020_Res <- Analyze_List("Pop_Sex_2020", Use_CD=TRUE,
                                 Referent="Female")
Pop_AgeCat_2020_Res <- Analyze_List("Pop_AgeCat_2020", Use_CD=TRUE,
                                    Referent="0-17")
# Adult_RE_2020_Res <- Analyze_List("Adult_RE_2020", Use_CD=TRUE,
#                                   Referent="White")
# Pop_Age_2020_Res <- Analyze_List("Pop_Age_2020")
# Adult_Age_2020_Res <- Analyze_List("Adult_Age_2020")
# Adult_AgeCat_2020_Res <- Analyze_List("Adult_AgeCat_2020")

HH_RO_2010_Res <- Analyze_List("HH_RO_2010", Use_CD=TRUE,
                               Referent="Renter")
Pop_RE_2010_Res <- Analyze_List("Pop_RE_2010", Use_CD=TRUE,
                                Referent="White")
Pop_UR_2010_Res <- Analyze_List("Pop_UR_2010", Use_CD=TRUE,
                                Referent="Urban")
Pop_Sex_2010_Res <- Analyze_List("Pop_Sex_2010", Use_CD=TRUE,
                                 Referent="Female")
Pop_AgeCat_2010_Res <- Analyze_List("Pop_AgeCat_2010", Use_CD=TRUE,
                                    Referent="0-17")
# Pop_Age_2010_Res <- Analyze_List("Pop_Age_2010")

HH_RO_2000_Res <- Analyze_List("HH_RO_2000", Use_CD=FALSE,
                               Referent="Renter")
Pop_RE_2000_Res <- Analyze_List("Pop_RE_2000", Use_CD=FALSE,
                                Referent="White")
Pop_UR_2000_Res <- Analyze_List("Pop_UR_2000", Use_CD=FALSE,
                                Referent="Urban")
Pop_Sex_2000_Res <- Analyze_List("Pop_Sex_2000", Use_CD=FALSE,
                                 Referent="Female")
Pop_AgeCat_2000_Res <- Analyze_List("Pop_AgeCat_2000", Use_CD=FALSE,
                                    Referent="0-17")
# Pop_Age_2000_Res <- Analyze_List("Pop_Age_2000", Use_CD=FALSE)


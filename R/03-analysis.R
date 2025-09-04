library(tidyverse)
load("int/Apportion.Rda")
source("R/00-key_values.R")

## Note: if you change these orders, you will need to change the orders 
### in the Totals and Proportions creating as well
Numerators <- c("EC", "Senate", "House")
Denominators <- c("Population", "Pop: Without DC", "Pop: With PR")

Analyze_List <- function(Name, Year=2020, Use_CD=TRUE,
                         Type="Census") {
  load(file=paste0("int/",Type,"/",Name,"_",Year,".Rda"))
  Obj <- get(paste(Name, Year, sep="_"))
  
  ColNames <- ColOrders[[Name]]
  ColNames_Use <- ColNames[ColNames %in% Obj$VarNames]
  Referent <- ColNames_Use[1]
  Group_Cols <- c("Total", ColNames_Use)
  Group_Cols_Prop <- paste0(Group_Cols, "_Prop")
  
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
    
    Totals <- tibble(Analysis=Denominators) %>%
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
    
    Proportions <- tibble(Analysis=c(Denominators,Numerators)) %>% 
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
                          EC_Summ %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~sum(.x*EC)/sum(EC))),
                          State_Summ %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~mean(.x))),
                          CD %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                                        dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                                ~mean(.x)))))
                          ## Creates a group that is the House including DC as 1:
                             # CD %>% dplyr::filter(Postal != "PR") %>%
                             #   dplyr::summarize(across(all_of(Group_Cols_Prop),
                             #                           ~mean(.x)))
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
    
    Totals <- tibble(Analysis=Denominators) %>%
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
    
    Proportions <- tibble(Analysis=c(Denominators,Numerators)) %>% 
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
                          State %>% dplyr::filter(Postal != "PR") %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~sum(.x*EC)/sum(EC))),
                          State %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~sum(.x*Senate)/sum(Senate))),
                          State %>% dplyr::filter(!(Postal %in% c("DC","PR"))) %>%
                            dplyr::summarize(across(all_of(Group_Cols_Prop),
                                                    ~sum(.x*House)/sum(House)))))
                          ## House with DC:
                          # State %>% dplyr::filter(Postal != "PR") %>%
                          #   dplyr::summarize(across(all_of(Group_Cols_Prop),
                          #                           ~sum(.x*House)/sum(House)))
  }
  

  
  Prop_Long <- Proportions %>%
    pivot_longer(cols=all_of(Group_Cols_Prop),
                 names_to=c("Category","Hold"),
                 names_sep="_",
                 values_to="Proportion") %>%
    dplyr::select(-c("Hold"))
  PL_Nums <- Prop_Long %>%
    dplyr::filter(Analysis %in% Numerators,
                  Category != "Total")
  
  Tbl_List <- list(Cols=ColNames_Use,
                   Referent=Referent)
  for (denom in Denominators) {
    Total_Pop <- Totals %>% dplyr::filter(Analysis==denom) %>%
      pull(Total)
    
    tbl <- Prop_Long %>% dplyr::filter(Analysis==denom,
                                       Category != "Total") %>%
      dplyr::rename(`Population Proportion`=Proportion) %>%
      dplyr::select(-c("Analysis")) %>%
      right_join(PL_Nums, by=join_by(Category)) %>%
      dplyr::mutate(`Abs. Weight`=Proportion/`Population Proportion`)
    
    Refs <- tbl %>%
      left_join(tbl %>% dplyr::filter(Category==Referent) %>% 
                  dplyr::rename(RefW=`Abs. Weight`) %>%
                  dplyr::select(Analysis, RefW),
                by=join_by(Analysis)) %>%
      dplyr::mutate(`Rel. Weight`=`Abs. Weight`/RefW) %>%
      dplyr::select(Category, Analysis, `Rel. Weight`)
    
    tbl <- tbl %>% 
      left_join(Refs, by=join_by(Category, Analysis)) %>%
      dplyr::mutate(`Excess Pop.`=Total_Pop*(Proportion-`Population Proportion`),
                    Analysis=factor(Analysis, levels=Numerators),
                    Category=factor(Category, levels=ColNames_Use)) %>%
      dplyr::arrange(desc(Analysis), Category) %>%
      dplyr::select(Analysis, Category, everything())
    
    Tbl_List[[denom]] <- tbl
  }
  
  assign(x=paste(Name, Year, "Res", sep="_"),
         value=Tbl_List)
  save(list=paste(Name, Year, "Res", sep="_"),
       file=paste0("res/",Type,"/",Name,"_",Year,".Rda"))
}

for (Name in Names) {
  Analyze_List(Name, Year=2020, Use_CD=TRUE, Type="Census")
  Analyze_List(Name, Year=2010, Use_CD=TRUE, Type="Census")
  Analyze_List(Name, Year=2000, Use_CD=FALSE, Type="Census")
}
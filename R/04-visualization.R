library(tidyverse)

Create_Trend <- function(Name, Years, Cols=NULL) {
  Weights <- NULL
  for (year in Years) {
    load(paste0("res/",Name,"_",year,".Rda"))
    Weights <- bind_rows(Weights,
                         get(paste(Name,year,"Res", sep="_"))$Weights %>%
                           mutate(Year=year))
  }
  Trend_Dat <- Weights %>% dplyr::filter(Numerator %in% c("House", "Senate", "EC"),
                                   Denominator %in% c("Population"))
  
  if (is.null(Cols)) {
    Cols <- colnames(Trend_Dat)[!(colnames(Trend_Dat) %in% c("Numerator","Denominator","Total_Prop","Year"))]
  }
  Dat <- Trend_Dat %>% dplyr::select(all_of(c("Numerator", "Year", Cols))) %>% 
    pivot_longer(cols=Cols, names_to=c("Category","Hold"), names_sep="_",
                 values_to="Weight")
  plot <- ggplot(Dat, mapping=aes(x=Year, y=Weight, 
                                  group=interaction(Category,Numerator), 
                                  shape=Numerator, linetype=Numerator, 
                                  color=Category)) +
    geom_point() +
    geom_line() +
    theme_bw()
  return(plot)
}

UR_Trend_Viz <- Create_Trend(Name="Pop_UR",
                             Years=c(2000, 2010, 2020))
UR_Trend_Viz
UR_Trend_Viz_UR <- Create_Trend(Name="Pop_UR",
                                Years=c(2000, 2010, 2020),
                                Cols=c("Urban_Prop","Rural_Prop"))
UR_Trend_Viz_UR

RE_Trend_Viz <- Create_Trend(Name="Pop_RE",
                             Years=c(2000, 2010, 2020))
RE_Trend_Viz



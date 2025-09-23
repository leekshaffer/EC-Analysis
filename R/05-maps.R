library(tidyverse)
library(usmap)
source("R/00-key_values.R")
load(file="int/Apportion.Rda")
# library(maps)
# maps::map(database="state")

## Creating Mapping data frame:
Yr <- 2020
Type <- "Census"
load(paste0("int/", Type, "/Pop_Sex_", Yr, ".Rda"))
M_Dat <- get(paste("Pop", "Sex", Yr, sep="_"))$CD %>%
  dplyr::filter(State != "Puerto Rico") %>%
  dplyr::mutate(fips=substr(GEOID, 1, 2)) %>%
  group_by(fips,State) %>% 
  dplyr::summarize(Total=sum(Total), House=n()) %>% 
  dplyr::mutate(House=if_else(State=="District of Columbia", 0, House),
                Senate=if_else(State=="District of Columbia", 0, 2),
                EC=if_else(State=="District of Columbia", 3, House+Senate)) %>%
  ungroup() %>%
  dplyr::mutate(`Population Proportion`=Total/sum(Total),
                across(all_of(Numerators), 
                       ~.x/sum(.x),
                       .names="{paste(col, 'Prop', sep='_')}"),
                across(all_of(Numerators),
                       ~(.x/sum(.x))/`Population Proportion`,
                       .names="{paste(col, 'Weight', sep='_')}"),
                Fixed=1)
file_usm <- us_map(regions="states", exclude="PR") %>%
  left_join(M_Dat, by=join_by(full==State, fips)) %>%
  left_join(get(x=paste("DR", "Res", Yr, sep="_")) %>% dplyr::select(State, EC_D, Senate_D, House_D),
            by=join_by(abbr==State)) %>%
  dplyr::mutate(House_D_Prop=House_D/House,
                Senate_D_Prop=Senate_D/Senate,
                EC_D_Prop=EC_D/EC)
save(file_usm, file=paste0("res/",Type,"/Map_States_", Yr, ".Rda"))

## Plotting:
Breaks <- list(EC=c(0.7, 1.0, 1.4, 2.0, 2.8),
               House=c(0.64, 0.8, 1.0, 1.25, 1.56),
               Senate=c(0.16, 0.4, 1.0, 2.5, 6.25),
               Prop=c(0.005, 0.02, 0.08))

plot_usmap(regions="states", exclude="PR",
           data=file_usm,
           color="grey70",
           values="Fixed") +
  scale_fill_gradient(name="",
                       trans="log",
                       low="white",
                       high="white",
                       na.value="white",
                       breaks=Breaks[["Prop"]])
ggsave(filename="figs/Maps/Map_Blank.png",
       device=png, width=6, height=4, units="in", dpi=300)

plot_usmap(regions="states", exclude="PR",
           data=file_usm,
           color="grey70",
           values="Population Proportion") +
  scale_fill_gradient(name="Population Proportion",
                      trans="log",
                      low="#feedde",
                      high="#e6550d",
                      na.value="#feedde",
                      breaks=Breaks[["Prop"]]) +
  theme(legend.position="bottom")
ggsave(filename="figs/Maps/Map_Population.png",
       device=png, width=6, height=4, units="in", dpi=300)

for (num in Numerators) {
  plot_usmap(regions="states", exclude="PR",
             data=file_usm,
             color="grey70",
             values=paste(num, "Weight", sep="_")) +
    scale_fill_gradient2(name=paste(names(Num_Titled)[Num_Titled==num], "Weight", sep=" "),
                         trans="log",
                         low="#762a83",
                         high="#1b7837",
                         midpoint=0,
                         na.value="#762a83",
                         breaks=Breaks[[num]]) +
    theme(legend.position="bottom")
  ggsave(filename=paste0("figs/Maps/Map_",num,"_Weights.png"),
         device=png, width=6, height=4, units="in", dpi=300)
  
  plot_usmap(regions="states", exclude="PR",
             data=file_usm,
             color="grey70",
             values=paste(num, "D", "Prop", sep="_")) +
    scale_fill_gradient(name=paste("Democratic Share", sep=" "),
                         low="#D00000",
                         high="#0000FF",
                         na.value="white",
                         breaks=c(0, 0.5, 1.0)) +
    theme(legend.position="bottom")
  ggsave(filename=paste0("figs/Maps/Map_",num,"_Votes.png"),
         device=png, width=6, height=4, units="in", dpi=300)
}


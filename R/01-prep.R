require(tidyverse)
require(readxl)

## Importing Apportionment Data:
Abb <- read_excel(path="data/StateAbbrevs.xlsx")
ECVotes <- read_excel(path="data/ECvotes.xlsx")
EC <- ECVotes %>%
  pivot_longer(cols=!State,
               names_to="Year",
               values_to="EC") %>%
  mutate(Year=as.numeric(Year)) %>%
  left_join(Abb, by="State")
EC_State <- ECVotes %>%
  mutate(State=substr(State,1,2)) %>%
  group_by(State) %>% 
  dplyr::summarize(across(everything(), sum)) %>%
  ungroup()  %>%
  pivot_longer(cols=!State,
               names_to="Year",
               values_to="EC") %>%
  mutate(Year=as.numeric(Year)) %>%
  left_join(Abb, by="State")

House <- read_excel(path="data/HouseApportion.xlsx",
                    na="––- ",
                    col_types=c("text",rep("numeric",24))) %>%
  pivot_longer(cols=!State,
               names_to="Year",
               values_to="House") %>%
  mutate(Year=as.numeric(Year))
House <- unique(House %>% dplyr::select(State)) %>% 
  cross_join(tibble(Year=seq(1788, 2024, by=2))) %>%
  left_join(House, by=c("State","Year")) %>% 
  group_by(State) %>%
  arrange(State,Year) %>%
  fill(House, .direction="down")  %>%
  left_join(Abb, by="State") %>%
  dplyr::mutate(House=replace_na(House, 0))

save(list=c("Abb", "EC","EC_State","House"), file="int/Apportion.Rda")

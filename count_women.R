## add number of women running and winning in each election to city data set 

library(tidyverse)
library(here)

city_data <- read_csv(here("Data Current", "city_qis.csv"))
candidate_data <- read_csv(here("Data Current", "candidate-district_data.csv"))
candidate_data <- candidate_data %>% 
  mutate(gender = na_if(gender, 9), 
         win = na_if(win, 9))

candidate_data <- candidate_data %>% 
  group_by(State, city, year) %>% 
  mutate(Women_run_e = sum(gender, na.rm = T)) 

candidate_data <- candidate_data %>% 
  group_by(State, city, year, d) %>% 
  mutate(Women_run_d = sum(gender, na.rm = T)) 

candidate_data <- candidate_data %>% 
  filter(win == 1) %>% 
  group_by(State, city, year) %>% 
  mutate(Women_win_e = sum(gender, na.rm = T))

candidate_data <- candidate_data %>% 
  filter(win == 1) %>% 
  group_by(State, city, year, d) %>% 
  mutate(Women_win_d = sum(gender, na.rm = T)) 

city_data <- candidate_data %>% 
  select(city, year, Women_run_e, Women_run_d, Women_win_e, Women_win_d) %>% 
  left_join(y = ., x = city_data, by = c("city", "year")) %>% 
  distinct() 

write_csv(city_data, here("Data Current", "city_qis.csv"))
write_csv(candidate_data, here("Data Current", "candidate-district_data.csv"))
#dont run again 
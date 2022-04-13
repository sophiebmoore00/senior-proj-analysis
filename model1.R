library(tidyverse)
library(here)

###### data prep ######

city_data <- read_csv(here("Data Current", "city_qis.csv"))
candidate_data <- read_csv(here("Data Current", "candidate-district_data.csv"))
city_data <- city_data %>% 
  mutate(MS = medianM*S) 
candidate_data <- candidate_data %>% 
  mutate(MS = M*S)

#cut sample 
sample_cities <- c("Amsterdam", 
                   "Campbelltown City Council",
                   "Canterbury-Bankstown", 
                   "Dublin", "Dublin ", #some entries for Dublin have a trailing space 
                   "Chicago", 
                   "Liverpool",
                   "Oakland", 
                   "Sacramento",
                   "St. Paul",
                   "Sydney")
city_data_sample <- city_data %>% 
  filter(city %in% sample_cities)
candidate_data_sample <- candidate_data %>% 
  filter(city %in% sample_cities)




###### set up the null models ######

#### Model 1 ####
#based on T&S 2020 Fig. 1
candidate_data %>% 
  group_by(city, year, d) %>% 
  ggplot(aes(x = log(M), y = log(Women_win_d))) +
  geom_jitter(alpha = 0.5, width = .2, height = .2) + 
  geom_smooth(method = lm, se = F, linetype = "dashed") + 
  geom_abline(intercept = 0, slope = 1) +
  ggtitle("Women winners vs. District Magnitude", 
          subtitle = "One observation = a single district in a single election") + 
  theme_classic() + 
  labs(x = "Log of District Magnitude", 
       y = "Log of Women Winners Per District")

#linear model for the number of women winning in each district per d 
candidate_data %>% 
  group_by(city, year, d) %>% 
  lm(Women_win_d ~ M, data = .) %>% 
  summary()

#### Model 2 ####

#based on T&S 2020 Fig. 2
candidate_data %>% 
  group_by(city, year) %>% 
  ggplot(aes(x = log(MS), y = log(Women_win_e))) +
  #geom_jitter(alpha = 0.5, width = .2, height = .2) +
  geom_point() +
  geom_smooth(method = lm, se = F, linetype = "dashed") + 
  ggtitle("Women winners vs. Seat Product", 
          subtitle = "One observation = a single election") +
  theme_classic() + 
  labs(x = "Log of Seat Product", 
       y = "Log of Women Winners Per Election")

#trying to add in the logical model line
candidate_data$m1pred <- candidate_data$MS^(1/8)
candidate_data$m2pred <- candidate_data$MS^(1/4)/2

candidate_data %>% 
  group_by(city, year) %>% 
  ggplot(aes(x = log(MS), y = log(Women_win_e))) +
  #geom_jitter(alpha = 0.5, width = .2, height = .2) +
  geom_point() +
  geom_smooth(method = lm, se = F, linetype = "dashed") + 
  geom_smooth(aes(y = m1pred), method = lm, col = "black") +
  ggtitle(bquote('Women winners vs. MS, with logical model' ~MS^(1/8)), 
          subtitle = "One observation = a single election") +
  theme_classic() +
  labs(x = "Log of Seat Product", 
       y = "Log of Women Winners Per Election")



candidate_data %>% 
  group_by(city, year) %>% 
  ggplot(aes(x = log(MS), y = log(Women_win_e))) +
  #geom_jitter(alpha = 0.5, width = .2, height = .2) +
  geom_point() +
  geom_smooth(method = lm, se = F, linetype = "dashed") + 
  geom_smooth(aes(y = m2pred), method = lm, col = "black") +
  ggtitle("Women winners vs. MS, with logical model MS^(1/4)/2", 
          subtitle = "One observation = a single election") +
  theme_classic()

candidate_data %>%
  mutate(m3pred = ((MS-1)^(1/4))/2) %>%
  group_by(city, year) %>%
  ggplot(aes(x = log(MS), y = log(1+Women_win_e))) +
  geom_jitter(alpha = 0.5, width = .2, height = .2) +
  #geom_point() +
  geom_smooth(method = lm, se = F, linetype = "dashed") +
  geom_smooth(aes(y = (MS^(1/4)-1)/2), method = lm, col = "black") +
  ggtitle("Women winners vs. MS, with logical model 3",
          subtitle = "One observation = a single election") +
  theme_classic()

candidate_data %>% 
  group_by(city, year) %>% 
  lm(Women_win_e ~ -1 + log(MS/2), data = .) %>% 
  summary()

candidate_data %>% 
  group_by(city, year) %>% 
  lm(Women_win_e ~ -1 + log(MS), data = .) %>% 
  summary()

candidate_data %>%
  mutate(m3pred = ((MS-1)^(1/4))/2) %>%
  group_by(city, year) %>%
  ggplot(aes(x = log(MS), y = log(1+Women_win_e))) +
  geom_jitter(alpha = 0.5, width = .2, height = .2) +
  #geom_point() +
  geom_smooth(method = lm, se = F, linetype = "dashed") +
  geom_smooth(aes(y = (MS-1)^(1/4)/2), method = lm, col = "black") +
  ggtitle("Women winners vs. MS, with logical model 3",
          subtitle = "One observation = a single election") +
  theme_classic()

#linear model for the number of women winning in each election
candidate_data %>% 
  group_by(city, year) %>% 
  lm(Women_win_e ~ -1 + log(MS), data = .) %>% 
  summary()


#### ####

#predict number of women who run from the seat product 
fit_run <- city_data %>% 
  lm(Women_win ~ MS, data = .)
summary(fit_run)

#predict number of women who win from the seat product 
fit_win <- city %>% 
  group_by(city) %>% 
  lm(Women_win ~ MS, data = .)
summary(fit_win)

############ fooling around

city_data %>% 
  group_by(city, state) %>% 
  count() %>% 
  View()

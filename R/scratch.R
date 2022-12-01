pacman::p_load(pacman, tidyverse, ggthemes, GGally, broom)

crash <- read_csv("data/TIM 2018 and 2022 Data.csv") %>% 
  mutate(`Crash Type` = case_when(
    `Crash Type` == "PD crash" ~ "PD Crash",
    `Crash Type` == "PI crash" ~ "PI Crash",
    TRUE ~ `Crash Type`),
    Year = as.character(Year),
    `RCT for IMT` = `RCT for IMT` %>% as.numeric(),
    `Response Time IMT` = `Response Time IMT` %>% 
      as.difftime() %>% 
      as.numeric(),
    Time = Time %>% as.numeric()
  ) %>% 
  select(Year, Date, Time, `Crash Type`, `Number of IMT Teams`, `Response Time IMT`, `RCT for IMT`, `Lanes at Bottleneck`, `Number of Lanes Closed`, `Affected Volume`, `Total Excess Travel Time`)

crash %>% 
  select(-Date) %>% 
  ggpairs(aes(color = Year), legend = c(1,1))

model <- crash %>% 
  lm(
    `Total Excess Travel Time` ~ Year + `Crash Type` + `Number of IMT Teams`,
    data = .)

crash %>% 
  ggplot(aes(x = time, y = `Total Excess Travel Time`,
             color = `Crash Type`)) +
  geom_point() +
  facet_wrap(~Year) +
  scale_y_log10() +
  scale_x_log10()

pacman::p_load(pacman, tidyverse, ggthemes, GGally, broom)

crash <- read_csv("data/TIM 2018 and 2022 Data.csv") %>% 
  mutate(`Crash Type` = case_when(
    `Crash Type` == "PD crash" ~ "PD Crash",
    `Crash Type` == "PI crash" ~ "PI Crash",
    TRUE ~ `Crash Type`),
    Year = as.character(Year),
    time = `RCT for IMT` %>% as.numeric()
  )

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

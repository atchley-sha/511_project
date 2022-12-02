pacman::p_load(pacman, tidyverse, ggthemes, GGally, broom) 

crashfile <- read_csv("data/TIM 2018 and 2022 Data mod.csv") 

crash <- crashfile %>% 
  transmute(
    year = as.factor(Year),
    type = as.factor(`Crash Type`),
    teams = as.integer(`Number of IMT Teams`),
    response_time = as.numeric(RT),
    tot_lanes = as.integer(`Lanes at Bottleneck`),
    closed_lanes = as.integer(`Number of Lanes Closed`),
    lane_ratio = closed_lanes / tot_lanes,
    duration = as.numeric(`T7-T0 mod`),
    peak = ifelse(str_detect(`Time Range`, "Off Peak"), "Off-peak", "Peak"),
    tett = as.numeric(`Total Excess Travel Time`)) %>% 
  drop_na(tett)
  

crash %>% 
  ggpairs(aes(color = year), legend = c(1,1))

model <- crash %>% 
  lm(tett ~ .^2, data = .)


step(model, direction = "both", k=log(278), trace = 0)

step(crash, scope = list(upper = tett ~ .^2, lower = tett ~ 1))

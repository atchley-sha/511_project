library(tidyverse)
library(magrittr)
library(patchwork)
library(ggthemes)

crashfile <- read_csv("data/TIM 2018 and 2022 Data.csv")

crash <- crashfile %>% 
  select(Year, `Crash Type #`, `Response Time IMT`, `RCT for IMT`, `Lanes at Bottleneck`, `Number of Lanes Closed`, `Total Excess Travel Time`, `Number of IMT Teams`, `T7-T0`, `Time Range`) %>% 
  transmute(
    Year,
    Type = case_when(
      `Crash Type #` == 1 ~ "Fatal",
      `Crash Type #` == 2 ~ "PI",
      `Crash Type #` == 3 ~ "PD"
    ),
    RT = `Response Time IMT` %>% 
      as.difftime() %>% 
      as.numeric() %>% 
      divide_by(60),
    RCT = `RCT for IMT` %>% as.numeric() %>% divide_by(60),
    Teams = `Number of IMT Teams`,
    Duration = `T7-T0` %>% as.numeric() %>% divide_by(60),
    LR = (`Lanes at Bottleneck` - `Number of Lanes Closed`) /
      `Lanes at Bottleneck`,
    Time = `Time Range`,
    Peak = if_else(
      Time %in% c("AM Peak", "PM Peak"), "Peak", "Off-Peak"),
    
    Delay = `Total Excess Travel Time` * 60
  ) %>% 
  filter(
    RT > 0,
    RCT > 0,
    Duration > 0,
    Delay > 0) %>% 
  drop_na()

compare_log_plot <- function(term){
  p <- crash %>% 
    ggplot(aes(x = {{term}})) +
    geom_density() +
    theme_pander()
  
  p_log <- crash %>% 
    ggplot(aes(x = log({{term}}))) +
    geom_density() +
    theme_pander()
  
  p / p_log
}

dur <- crash %>%
  ggplot(aes(x = Duration, y = log(Delay))) +
  geom_point() +
  theme_pander()

dur_log <- crash %>%
  ggplot(aes(x = log(Duration), y = log(Delay))) +
  geom_point() +
  theme_pander()

compare_log_plot(Delay) | (dur / dur_log)


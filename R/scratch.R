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
  ) 

crash
%>% 
  select(Year, Time, `Crash Type`, `Number of IMT Teams`, `Response Time IMT`, `RCT for IMT`, `Lanes at Bottleneck`, `Number of Lanes Closed`, `Affected Volume`, `Total Excess Travel Time`) %>%
  filter(`Total Excess Travel Time` > 0)

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


crash2 <- crash %>% 
  transmute(Year, Time, `Number of IMT Teams`, `Crash Type`, `Response Time IMT`, `Affected Volume`, logtime = log(`Total Excess Travel Time`), roottime = sqrt(`Total Excess Travel Time`), open_lanes = `Lanes at Bottleneck` - `Number of Lanes Closed`, `Total Excess Travel Time`)

crash2 %>% 
  ggpairs(aes(color = Year), legend = c(1,1))

crash %>% 
  ggplot(aes(x = (`RCT for IMT`), y = (`Total Excess Travel Time`), color = Year, size = `Lanes at Bottleneck` - `Number of Lanes Closed`)) +
  geom_point() +
  geom_smooth()

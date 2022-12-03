library(broom)

raw_mod <- crash %>% 
  lm(log(Delay) ~ LR + log(Duration) + Peak + Type*RT, data = .)

final_mod <- crash %>% 
  lm(log(Delay) ~ LR + Peak + Type, data = .)

duration_mod <- crash %>% 
  lm(log(Delay) ~ LR + Peak + Type + log(Duration), data = .)

library(targets)
library(tarchetypes)
library(magrittr)

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "ggthemes",
    "GGally",
    "broom",
    "magrittr",
    "patchwork",
    "olsrr",
    "kableExtra"), # packages that your targets need to run
  format = "rds" # default storage format
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

data_targets <- tar_plan(
  tar_target(crashfile, "data/TIM 2018 and 2022 Data.csv", format = "file"),
  crash = clean_data(crashfile) %>% 
    select(-Time),
  crash_pred = clean_data(crashfile) %>% 
    select(-c(Time, Peak, RCT, Teams, Duration)),
  crash_filter = crash_pred %>% 
    filter(Type != "Fatal"),
  crash_final = clean_data(crashfile) %>% 
    transmute(log_Delay = log(Delay), Year, LR, RT)
)

model_targets <- tar_plan(
  full_model = lm(log(Delay) ~ (log(Duration) + . - Duration)^2, data = crash),
  step_full = step(full_model, k = log(nrow(crash))),
  pred_model = lm(log(Delay) ~ .^2, data = crash_pred),
  step_pred = step(pred_model, k = log(nrow(crash_pred))),
  
  final_model = lm(log_Delay ~ ., data = crash_final),
  final_rsq = final_model %>% summary() %>% {.$r.squared},
  
  pred_rsq = step_pred %>% summary() %>% {.$r.squared},
  full_rsq = step_full %>% summary() %>% {.$r.squared}
)

viz_targets <- tar_plan(
  influence = case_influence_plots(crash, final_model),
  log_comparison = compare_logs(crash),
  final_pairs = plot_pairs(crash_final),
  
  full_table = make_model_table(step_full, digits = c(NA,2,3,2,4)),
  pred_table = make_model_table(step_pred, digits = c(NA,2,3,2,4)),
  final_table = make_model_table(final_model, digits = c(NA,2,3,2,4)),
  
  rt_plot = one_pair_plot(crash, aes(x = RT, y = log(Delay)), "image/rt.png"),
  teams_plot = one_pair_plot(crash, aes(x = Teams, y = log(Delay)), "image/teams.png"),
  duration_plot = one_pair_plot(crash, aes(x = log(Duration), y = log(Delay)), "image/duration.png"),
  
  crash_type_plot = plot_types(crash),
  
  fit_plot = plot_fit(crash, final_model),
  resid_plot = plot_residuals(crash, final_model)
)

#### Run all targets ####
tar_plan(
  data_targets,
  model_targets,
  viz_targets
)

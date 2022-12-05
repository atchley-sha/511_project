case_influence_plots <- function(data, model){
  
  fit <- data %>% 
    mutate(
      case = 1:nrow(.),
      cooks = cooks.distance(model),
      leverage = hatvalues(model),
      students = rstudent(model)
    )
  
  
  cooks <- fit %>% 
    ggplot(aes(x = case, y = cooks)) +
    geom_point() +
    geom_hline(yintercept = 1, lty = "dashed") +
    expand_limits(y = c(0,1)) +
    theme_bw() +
    theme(axis.title.x = element_blank())
  
  lev <- fit %>% 
    ggplot(aes(x = case, y = leverage)) +
    geom_point() +
    expand_limits(y = c(0,1)) +
    theme_bw() +
    theme(axis.title.x = element_blank())
  
  stud <- fit %>% 
    ggplot(aes(x = case, y = students)) +
    geom_point() +
    geom_hline(yintercept = c(2, -2), lty = "dashed") +
    expand_limits(y = c(-2,2)) +
    theme_bw()
  
  plot <- wrap_plots(cooks, lev, stud, ncol = 1)
  
  ggsave("image/case_influence.png",
         plot,
         scale = 2,
         width = 6,
         height = 4,
         units = "in")
  
  plot
}


compare_logs <- function(crash){
  
  delay <- crash %>% 
    ggplot(aes(x = Delay)) +
    geom_density() +
    theme_bw()
  
  logdelay <- crash %>% 
    ggplot(aes(x = log(Delay))) +
    geom_density() +
    theme_bw()
  
  duration <- crash %>% 
    ggplot(aes(x = Duration, y = log(Delay))) +
    geom_point() +
    theme_bw()
  
  logduration <- crash %>% 
    ggplot(aes(x = log(Duration), y = log(Delay))) +
    geom_point() +
    theme_bw()
  
  plot <- wrap_plots(delay, logdelay, duration, logduration,
                     ncol = 2, byrow = FALSE)
  
  ggsave("image/compare_logs.png",
         plot,
         scale = 2,
         width = 6,
         height = 4,
         units = "in"
         )
  
  plot
}


plot_pairs <- function(crash){
  
  plot <- crash %>%
    ggpairs(upper = "blank", switch = "both") +
    theme_bw()
  
  ggsave("image/pairs.png",
         plot,
         scale = 2,
         width = 6,
         height = 4,
         units = "in")
  
  plot
}


one_pair_plot <- function(data, aes, file, ...){
  
  plot <- data %>% 
    ggplot(mapping = aes) +
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    theme_bw()
  
  ggsave(file, plot, ...)
  
  plot
}


plot_fit <- function(data, model){
  
  plot <- data %>% 
    mutate(pred = fitted(model)) %>% 
    ggplot(aes(x = pred, y = log(Delay))) + 
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    theme_bw() +
    labs(x = "Fitted log(Delay)", y = "Actual log(Delay)")
  
  ggsave("image/fit.png", plot)
  
  plot
  
}

plot_residuals <- function(data, model){
  
  plot <- data %>% 
    mutate(resid = residuals(model),
           pred = fitted(model)) %>% 
    ggplot(aes(x = pred, y = resid)) + 
    geom_point() +
    geom_smooth(method = 'lm', se = FALSE) +
    theme_bw() +
    labs(x = "Fitted log(Delay)", y = "Residuals")
  
  ggsave("image/residuals.png", plot)
  
  plot
  
}


plot_types <- function(crash){
  
  plot <- crash %>% 
    ggplot(aes(x = Type)) +
    geom_bar() +
    geom_text(stat='count',aes(label=after_stat(count)), vjust = -1) +
    theme_bw() +
    labs(x = "Crash Type") +
    expand_limits(y = c(NA,155)) +
    theme(axis.title.y = element_blank())
  
  ggsave("image/types.png", plot,
         width = 6, height = 4, units = "in")
  
  plot
}
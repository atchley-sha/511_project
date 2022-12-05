clean_data <- function(crashfile){
  
  crash <- crashfile %>% 
    read_csv() %>% 
    select(Year, `Crash Type #`, `Response Time IMT`, `RCT for IMT`, `Lanes at Bottleneck`, `Number of Lanes Closed`, `Total Excess Travel Time`, `Number of IMT Teams`, `T7-T0`, `Time Range`) %>% 
    transmute(
      Year = as.factor(Year),
      Type = case_when(
        `Crash Type #` == 1 ~ "Fatal",
        `Crash Type #` == 2 ~ "PI",
        `Crash Type #` == 3 ~ "PDO"
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
  
  crash
}
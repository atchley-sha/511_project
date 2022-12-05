make_model_table <- function(
    model, digits = 4, colnames = c("", "Coeff", "SE", "t-stat", "p-value")){
  
  model %>% 
    tidy() %>% 
    `colnames<-`(colnames) %>% 
    kbl(digits = digits) %>% 
    kable_styling()
}
make_model_table <- function(
    model, digits = 4, colnames = c("", "Coefficient", "SE", "p-value")){
  
  model %>% 
    tidy() %>% 
    select(1,2,3,5) %>% 
    `colnames<-`(colnames) %>% 
    kbl(digits = digits) %>% 
    kable_styling()
}
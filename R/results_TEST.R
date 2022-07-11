### Results

library(kableExtra)
library(broom)

lead_zero = function(num) sub("^0+", "", num %>% as.character())  

sig_format = function(num, sig=.001, prefix='<', round=3) {
  ifelse(
    abs(num) <= sig, 
      sig %>% 
       lead_zero() %>%
       paste0(prefix,.), 
      num %>%
       round(digits=round) %>%
       lead_zero()
  )}

sig_format(3.0001)
sig_format(.001)

mod_linear %>%
  tidy() %>%
  mutate_at(
    c("std.error","statistic","p.value"),
    sig_format
  ) %>%
  kable(
    label = NA,
    #caption = glue('Table {tab_num}: Training Model Summary'),
    #align = 'lrrrr'
  ) %>%
  kable_styling()


workflow() %>%
  add_recipe(prediction_recipe) %>%
  add_model(lasso_spec) %>%
  fit(data = train) %>%
  pull_workflow_fit() %>%
  tidy() %>%
  kable(
    label = NA,
    #caption = glue('Table {tab_num}: Training Model Summary'),
    #align = 'lrrrr'
  ) %>%
  kable_styling()
  
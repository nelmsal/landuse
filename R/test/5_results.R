### Results

library(kableExtra)
library(broom)

hist(log(zoning$maxhd_macro))
hist(log(zoning$maxhd_micro))


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

trail_zero = function(x) sub("0+$", "", x)
lead_zero = function(x) ifelse(as.numeric(x)==0,"0", sub("^0+", "", x))
no_zero <- function(x) {
  ifelse(as.numeric(x)==0,"0",
         lead_zero(x) %>%
           trail_zero(.))
}
cnum = function(x, digits=3) x %>% round(digits) %>% no_zero(.)

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

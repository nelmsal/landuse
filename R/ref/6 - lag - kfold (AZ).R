###############################
## modelling
###training set: The training set is used to fit the different models
### validation set: The performance on the validation set is then used for the model dplyr::selection
### test set: The advantage of having a test set that the model hasn't seen before during the training and model dplyr::selection steps
###           is that we can obtain a less biased estimate of its ability to generalize to new data.
###############################
setwd("//econha01/client/CLIENT/1 Consulting/Freddie Mac/Prediction Modeling/AZ")

library(tidyverse)
library(tidymodels)
library(tigris)
library(sf)
library(sp)
rm(list=ls())
# load("R/regression.rda") #2028
# load('R/infill.rda') #4178
# load('R/lag_data.Rda') #4178
load("R/regression_nodense.rda") #3977
load('R/infill_nodense.rda') #9153
load('R/lag_data_nodense.Rda') #9154
regression_lags <- left_join(dplyr::select(regression, -maxhd_micro), dplyr::select(infill, -geometry,-area_total_lag3)) #353
full_dataset <- left_join(lag_data%>%st_drop_geometry()%>%dplyr::select(-area_total,-X,-Y),dplyr::select(infill,-geometry,-area_total_lag3))

####Prepare Training, Validation (test), and Test dataset(holdout)####
set.seed(43)

split <-
  regression_lags %>%
  drop_na() %>%
  dplyr::select(-GEOID) %>%
  # log transform the dependent variable
  mutate(maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro)) %>%
  # go with a 70/30 split since we have a limited sample, try  50/50 as well to avoid overfitting
  initial_split(prop = 0.7,strata=maxhd_macro)

train <- training(split)
test <- testing(split)

# set.seed(43)
# validation <- validation_split(train,strata=maxhd_macro,prop=0.7)

blocks <-
  block_groups(state = "AZ", cb = FALSE, class = 'sf',year=2019) %>%  #default year is 2019
  st_transform(3857) %>%
  transmute(GEOID,
            area_total = units::set_units(st_area(geometry), acres),
            centroid = st_centroid(geometry)) %>%
  mutate(X = st_coordinates(centroid)[, 1],
         Y = st_coordinates(centroid)[, 2]) %>%
  dplyr::select(-centroid)


bind_rows(mutate(train, split = "train"),
          mutate(test,split="test")) %>%
  ggplot(aes(exp(maxhd_macro), fill = split)) +
  geom_histogram() +
  scale_x_log10() +
  facet_wrap(~ split) +
  theme_minimal()

####Prepare variables and Use Lasso to pick variables####
continuous <- c("isoperi_mean", "building_size", "intersection_density", "prop_4way", "orientation_order", "ndvi_mean", "built_intensity", "dist_towns", "dist_city", "pct_nonwhite", "density", "household_size", "median_income")
lags <- str_c(continuous, "_lag3")
dummy <- c("built_change")

ls_recipe <-
  recipe(maxhd_macro ~ .,
         data = train) %>%
  step_center(all_of(c(continuous, lags)))%>%
  step_scale(all_of(c(continuous, lags))) %>% #step_scale creates a specification of a recipe step that will normalize numeric data to have a standard deviation of one.
  step_dummy(all_of(dummy))%>% #A recipe is a description of the steps to be applied to a data set in order to prepare it for data analysis.
  step_zv(all_predictors())

lasso_spec <-
linear_reg(penalty = tune(), mixture = 1) %>% #linear_reg() defines a model that can predict numeric values from predictors using a linear function. This function can fit regression models.
  set_engine("glmnet")

ls_workflow <-
  workflow()%>%
  add_model(lasso_spec)%>%
  add_recipe(ls_recipe)

ls_res <-
  ls_workflow %>%
  tune_grid(resamples = vfold_cv(train),
            grid = grid_regular(penalty(range = c(0.01, 0.06), trans = NULL),
                                levels = 20),
            metrics = metric_set(mae), #rmse,rsq
            control = control_grid(save_pred = TRUE))


ls_plot<-
  ls_res %>%
  collect_metrics()%>%
  ggplot(aes(x = penalty, y = mean,color = .metric)) +
  geom_point(size = 2) +
  facet_wrap(~.metric, scales = 'free') +
  theme_minimal()

ls_plot

top_models <-
  ls_res %>%
  show_best("mae",n=20)%>%
  arrange(mean)

ls_best <-
  ls_res %>%
  collect_metrics()%>%
  arrange(penalty)%>%
  slice(13) #mae:0.7085161; penalty: 0.03020408

ls_result <-
  ls_res %>%
  collect_predictions(parameters=ls_best)%>%
  transmute(predicted = exp(.pred),
            observed = exp(maxhd_macro)) %>%
  mutate(error = abs(observed - predicted),
         group = factor(ntile(observed, 10))) %>%
  drop_na() %>%
  pivot_longer(cols = c(observed, predicted)) %>%
  group_by(group, name) %>%
  summarise(value = mean(value)) %>%
  mutate(error = abs(value - lag(value))) %>%
  ungroup() %>%
  fill(error, .direction = "up") %>%
  ggplot(aes(x = factor(group), y = value, colour = name, style = name)) +
  geom_point() +
  scale_colour_brewer(palette = 'Set1', name = "Housing Density...") +
  scale_y_continuous(breaks = c(0, 5,10,15,20)) +
  labs(title = "Penalized Linear Regression",
       subtitle = "Deciles of block groups arranged by housing density",
       x = "< less dense | more dense >", y = "Maximum allowable housing units per acre") +
  annotate(geom="text",x=4,y=20,label="MAE: 0.7549559",color="red")+
  theme_minimal()
ls_result
ggsave(plot = last_plot(), filename = "viz/ls_kfold_prediction_vs_observation_az_cutoffdense.jpeg", height = 6, width = 8, dpi = 300)


lasso_spec <-
  linear_reg(penalty = 0.04157895, mixture = 1) %>%
  set_engine("glmnet")

useful_terms <-
  workflow() %>%
  add_recipe(ls_recipe) %>%
  add_model(lasso_spec) %>%
  fit(data = train) %>%
  extract_fit_parsnip()%>%
  # pull_workflow_fit() %>%
  tidy() %>%
  filter(estimate != 0) %>%
  slice(2:n()) %>%
  pull(term)

useful_terms
####Use train dataset to train the model####
# regression_lags <- dplyr::select(regression_lags, GEOID, maxhd_macro, any_of(useful_terms),built_change)
regression_lags <- dplyr::select(regression_lags, GEOID, maxhd_macro, any_of(useful_terms))
set.seed(43)
split <-
  regression_lags %>%
  drop_na() %>%
  dplyr::select(-GEOID) %>%
  # log transform the dependent variable
  mutate(maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro)) %>%
  # go with a 70/30 split since we have a limited sample, try  50/50 as well to avoid overfitting
  initial_split(prop = 0.7,strata=maxhd_macro)

train <- training(split)
test <- testing(split)

# set.seed(43)
# validation <- validation_split(train,strata=maxhd_macro,prop=0.7)

prediction_recipe <-
  recipe(maxhd_macro ~ .,
         data = train) %>%
  step_center(all_numeric(), -all_outcomes())%>%
  step_scale(all_numeric(), -all_outcomes()) %>% #step_scale creates a specification of a recipe step that will normalize numeric data to have a standard deviation of one.
  # step_dummy(built_change)%>% #A recipe is a description of the steps to be applied to a data set in order to prepare it for data analysis.
  step_zv(all_predictors())
# prediction_prep <- prep(prediction_recipe)
# prediction_juice <- juice(prediction_prep)

## model it
set.seed(42)

# random forest
mod_rf <-
  rand_forest(trees = 500,
              mtry = tune(),
              min_n = tune()) %>%
  set_mode("regression") %>%
  set_engine("ranger") #gbm

wflow_rf <-
  workflow() %>%
  add_recipe(prediction_recipe) %>%
  add_model(mod_rf)
# this will take a while without parallel processing
doParallel::registerDoParallel(cores = 6)

tictoc::tic()
tuned_rf <-
  tune_grid(
    wflow_rf,
    resamples = vfold_cv(train),
    grid = 20,
    metrics = metric_set(mae)
  )
tictoc::toc()

tuned_rf %>%
  collect_metrics() %>%
  filter(.metric == "mae") %>%
  dplyr::select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "MAE")

# create a regular grid to search through
grid <-
  grid_regular(
    mtry(range = c(2, 14)),
    min_n(range = c(2, 10)),
    levels = 20
  )
## tune on 10 folds
set.seed(42)

# default is 10 folds
# folds <- vfold_cv(train)
# tune the next (54.95 seconds)
tictoc::tic()
tuned_rf <-
  tune_grid(
    wflow_rf,
    resamples = vfold_cv(train),
    grid = grid,
    metrics = metric_set(mae),
    control=control_grid(save_pred=TRUE)
  )
tictoc::toc()

best_rf<-select_best(tuned_rf, "mae")
autoplot(tuned_rf)
rf_metrics <-
  tuned_rf %>%
  collect_metrics()%>%
  filter(mtry==2 & min_n==3)

rmse_rf <-
  tuned_rf%>%
  collect_predictions(parameters=best_rf)%>%
  transmute(predicted = exp(.pred),
            observed = exp(maxhd_macro)) %>%
  mutate(error = abs(observed - predicted),
         group = factor(ntile(observed, 10))) %>%
  drop_na() %>%
  pivot_longer(cols = c(observed, predicted)) %>%
  group_by(group, name) %>%
  summarise(value = mean(value)) %>%
  mutate(error = abs(value - lag(value))) %>%
  ungroup() %>%
  fill(error, .direction = "up") %>%
  ggplot(aes(x = factor(group), y = value, colour = name, style = name)) +
  geom_point() +
  scale_colour_brewer(palette = 'Set1', name = "Housing Density...") +
  scale_y_continuous(breaks = c(0, 5,10,15,20)) +
  labs(title = "Random Forest",
       subtitle = "Deciles of block groups arranged by housing density",
       x = "< less dense | more dense >", y = "Maximum allowable housing units per acre") +
  annotate(geom="text",x=4,y=20,label="MAE:0.6274219",color="red")+
  theme_minimal()
rmse_rf
ggsave(plot = last_plot(), filename = "viz/rf_kfold_prediction_vs_observation_az_cutoffdense.jpeg", height = 6, width = 8, dpi = 300)



last_rf_mod<-
  rand_forest(mtry=2,min_n=3,trees=500)%>%
  set_engine("ranger", importance = "permutation")%>%
  set_mode("regression")

last_rf_workflow <-
  wflow_rf %>%
  update_model(last_rf_mod)

set.seed(45)
last_rf_fit <-
  last_rf_workflow %>%
  last_fit(split,metrics=metric_set(mae))

last_rf_fit %>%
  collect_metrics() #For test dataset

last_rf_fit %>%
  pluck(".workflow", 1) %>%
  pull_workflow_fit() %>%
  vip::vip(num_features = 14)
ggsave(plot = last_plot(), filename = "viz/rf_kfold_variable_importance_cutoffdense.jpeg", height = 6, width = 8, dpi = 300)

####Predict the rest of the block groups and Visualize####
rest <-
  full_dataset %>%
  filter(!GEOID %in% regression$GEOID )%>%
  dplyr::select(GEOID, any_of(useful_terms),built_change)%>%
  drop_na()
for (col in useful_terms[useful_terms!="built_change_X1"]) {
  rest[col] <-scale(rest[col],scale=TRUE,center=TRUE)
}
rest <- rest %>% rename(built_change_X1 = built_change)
rest$built_change_X1 <- as.numeric(as.character(rest$built_change_X1))

prediction_prep <- prep(prediction_recipe)
prediction_juice <-juice(prediction_prep)
rest_prediction <- predict(last_rf_mod%>%set_engine("ranger")%>%fit(maxhd_macro~.,data=prediction_juice),rest%>%dplyr::select(-GEOID)) #use training dataset
rest_prediction <-
  rest_prediction %>%
  transmute(maxhd_macro = exp(.pred))
rest <- cbind(rest,rest_prediction)

az_dens <-
  regression %>%
  mutate(category = "observation")%>%
  dplyr::select(GEOID,maxhd_macro,category)%>%
  rbind(rest %>%mutate(category = "prediction")%>%dplyr::select(GEOID,maxhd_macro,category))
blocks_dens <- left_join(blocks,az_dens)
sum(blocks_dens$maxhd_macro==0,na.rm=TRUE) #15 maximum allowable density are 0
sum(is.na(blocks_dens$maxhd_macro)) #62 block groups don't have the maximum allowable density data
st_write(blocks_dens,"data/model/observation_prediction_bg_az.shp")
####ADD Protected Area to Visualization####
blocks_dens <- st_read("data/model/observation_prediction_bg_az.shp")
blocks_dens <- rename(blocks_dens,maxhd_macro = mxhd_mc)
protected <- read.csv('data/Protected Areas/area.csv')
protected$GEOID <- as.character(protected$GEOID)
protected$GEOID<-paste0("0",protected$GEOID)
protected_bg <- protected[protected$coverage>=80,]$GEOID #405
blocks_dens$categry <- ifelse(blocks_dens$GEOID %in% protected_bg & (blocks_dens$categry=="prediction"|is.na(blocks_dens$categry)),"protected",blocks_dens$categry)
blocks_dens <- left_join(blocks_dens ,protected,by="GEOID")
blocks_dens$maxhd_macro <- ifelse(blocks_dens$categry=="protected",blocks_dens$housing_density,blocks_dens$maxhd_macro)
table(blocks_dens$categry)
st_write(blocks_dens,"data/model/observation_prediction_protection_bg_az.shp")
# observation  prediction   protected
# 2028        1757         357
sum(is.na(blocks_dens$maxhd_macro)) #36, cannot predict and also not protected




library(classInt)
library(leaflet)
regression_blocks <-
  blocks %>%
  left_join(regression %>% dplyr::select(GEOID,maxhd_macro))

st_write(regression_blocks,"data/model/observation_bg_az.shp")
regression_blocks <- st_read("data/model/observation_bg_az.shp")
regression_blocks <- rename(regression_blocks,maxhd_macro = mxhd_mc)
protected_blocks <-
  blocks_dens %>%
  filter(categry=="protected")%>%
  select(GEOID,maxhd_macro)%>%
  st_drop_geometry()%>%
  right_join(blocks)%>%
  st_as_sf()
prediction_blocks <-
  blocks_dens %>%
  filter(categry=="prediction"|is.na(categry))%>%
  select(GEOID,maxhd_macro)%>%
  st_drop_geometry()%>%
  right_join(blocks)%>%
  st_as_sf()

blocks2 <- blocks_dens %>% filter(maxhd_macro!=0 & !is.na(maxhd_macro))
breaks_qt <- classIntervals(c(min(blocks2$maxhd_macro) - .00001, blocks2$maxhd_macro), n = 8, style = "quantile")
blocks_dens <-as(blocks_dens,Class = "Spatial")
regression_blocks <- as(regression_blocks,Class = "Spatial")
protected_blocks <- as(protected_blocks,Class = "Spatial")
prediction_blocks <-as(prediction_blocks,Class = "Spatial")
geo_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
blocks_dens = spTransform(blocks_dens,geo_proj)
regression_blocks = spTransform(regression_blocks,geo_proj)
protected_blocks = spTransform(protected_blocks,geo_proj)
prediction_blocks = spTransform(prediction_blocks,geo_proj)
palette9 <- c("#ffffff","#0000ff","#3874ff","#29dbff","#8affbe","#daff61","#ffe100","#ff8400","#ff0000")
# pal <-colorBin(palette9,blocks_dens@data$maxhd_macro,bins=c(0,breaks_qt[["brks"]]),pretty=TRUE,right=FALSE,na.color = "grey")
# pal_reg <- colorBin(palette9,regression_blocks@data$maxhd_macro,bins=c(0,breaks_qt[["brks"]]),pretty=TRUE,right=FALSE,na.color = "grey")
# pal_nona <-colorBin(palette9,blocks_dens@data$maxhd_macro,bins=c(0,breaks_qt[["brks"]]),pretty=TRUE,right=FALSE,na.color = NA)
pal <-colorBin(palette9,blocks_dens@data$maxhd_macro,bins=c(0,6.846475e-05,0.5,1.5,3,4,5,10,50,86),pretty=TRUE,right=FALSE,na.color = "grey")
pal_reg <- colorBin(palette9,regression_blocks@data$maxhd_macro,bins=c(0,6.846475e-05,0.5,1.5,3,4,5,10,50,86),pretty=TRUE,right=FALSE,na.color = "grey")
pal_pred <- colorBin(palette9,prediction_blocks@data$maxhd_macro,bins=c(0,6.846475e-05,0.5,1.5,3,4,5,10,50,86),pretty=TRUE,right=FALSE,na.color = "grey")
pal_pro <- colorBin(palette9,protected_blocks@data$maxhd_macro,bins=c(0,6.846475e-05,0.5,1.5,3,4,5,10,50,86),pretty=TRUE,right=FALSE,na.color = "grey")
pal_nona <-colorBin(palette9,blocks_dens@data$maxhd_macro,bins=c(0,6.846475e-05,0.5,1.5,3,4,5,10,50,86),pretty=TRUE,right=FALSE,na.color = NA)
all_popup <-paste('<strong>GEOID: </strong>',blocks_dens@data$GEOID,
                  '<hr><strong>Maximum Allowed Density: </strong>',round(blocks_dens@data$maxhd_macro,4))
o_popup <-paste('<strong>GEOID: </strong>',regression_blocks@data$GEOID,
                '<hr><strong>Maximum Allowed Density: </strong>',round(regression_blocks@data$maxhd_macro,4))
p_popup <-paste('<strong>GEOID: </strong>',prediction_blocks@data$GEOID,
                '<hr><strong>Maximum Allowed Density: </strong>',round(prediction_blocks@data$maxhd_macro,4))
pro_popup <-paste('<strong>GEOID: </strong>',protected_blocks@data$GEOID,
                  '<hr><strong>Maximum Allowed Density: </strong>',round(protected_blocks@data$maxhd_macro,4))
library(rgdal)
protected_area <- readOGR('data/Protected Areas',layer = "pad_Arizona")
protected_area = spTransform(protected_area,geo_proj)
area <- read.csv('data/Protected Areas/area.csv')
area$GEOID<-as.character(area$GEOID)
area$GEOID<-paste0("0",area$GEOID)
census_block <- left_join(blocks,area%>%select(GEOID,housing_density),by="GEOID")
census_block <- as(census_block,Class = "Spatial")
census_block = spTransform(census_block,geo_proj)
pal_census <- colorBin(palette9,census_block@data$housing_density,bins=c(0,6.846475e-05,0.5,1.5,3,4,5,10,50,86),pretty=TRUE,right=FALSE,na.color = "grey")
c_popup <- paste('<strong>GEOID: </strong>',census_block@data$GEOID,
                 '<hr><strong>Existing Housing Units Per Acre: </strong>',round(census_block@data$housing_density,4))
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -112, lat = 34, zoom = 7)%>%
  addLayersControl(
    baseGroups =c("Observation","Prediction","Protected","All","Existing"),
    overlayGroups = c("Protected Area"),
    options = layersControlOptions(collapsed=FALSE)
  )%>%
  addPolygons(
    data = protected_area,
    stroke = FALSE,
    smoothFactor = 0.1,
    fillOpacity = 1,
    fillColor = "dark green",
    group = "Protected Area"
  )%>%
  # hideGroup(c("Observation"))%>%
  addPolygons(
    data=blocks_dens,
    weight=0.5,
    color="grey",
    smoothFactor = 0.1,
    fillOpacity = 0.8,
    fillColor = ~pal(maxhd_macro),
    popup = all_popup,
    group="All"
  )%>%
  addPolygons(
    data=regression_blocks,
    weight=0.5,
    color="grey",
    smoothFactor=0.1,
    fillOpacity=0.8,
    fillColor=~pal_reg(maxhd_macro),
    popup = o_popup,
    group="Observation"
  )%>%
  addPolygons(
    data=prediction_blocks,
    weight=0.5,
    color="grey",
    smoothFactor=0.1,
    fillOpacity=0.8,
    fillColor=~pal_pred(maxhd_macro),
    popup = p_popup,
    group="Prediction"
  )%>%
  addPolygons(
    data=protected_blocks,
    weight=0.5,
    color="grey",
    smoothFactor=0.1,
    fillOpacity=0.8,
    fillColor=~pal_pro(maxhd_macro),
    popup = pro_popup,
    group="Protected"
  )%>%
  addPolygons(
    data=census_block,
    weight=0.5,
    color="grey",
    smoothFactor=0.1,
    fillOpacity=0.8,
    fillColor=~pal_census(housing_density),
    popup = c_popup,
    group="Existing"
  )%>%
  addLegend("bottomright",
            # pal = pal,
            colors=c("grey",palette9),
            # colors = c("#0000ff","#3874ff","#29dbff","#8affbe","#daff61","#ffe100","#ff8400","#ff0000"),
            # values = blocks_dens@data$maxhd_macro,
            labels = c('NA','0','0 - 0.5','0.5 - 1.5','1.5 - 3.0','3.0 - 4.0','4.0 - 5.0','5.0 - 10.0','10.0 - 50.0','50.0 - 86.0'),
            title = "Maximum Allowable Housing Units per Acre",
            opacity = 1)

####Overall Performance and Error Visualization####
train_pred <-
  tuned_rf%>%
  collect_predictions(parameters=best_rf) %>%
  arrange(.row)
test_pred <-
  last_rf_fit%>%
  collect_predictions()%>%
  arrange(.row)

test <- cbind(test,test_pred %>%select(.pred))
train <- cbind(train,train_pred %>% select(.pred))
all  <- rbind(test,train)

library(Metrics)
mae(test$maxhd_macro,test$.pred) #0.8714629 for test
mae(train$maxhd_macro,train$.pred) #  0.8975119 for train
mae(all$maxhd_macro,all$.pred) #0.8896848 for all

label <-
  regression_lags %>%
  drop_na() %>%
  # log transform the dependent variable
  mutate(maxhd_macro = log(maxhd_macro)) %>%
  filter(!is.infinite(maxhd_macro)) %>%
  select(GEOID)
train_id <- split[["in_id"]]
trainGEOID <-
  label %>%
  filter(row(label) %in% train_id)
train <- cbind(trainGEOID,train)
testGEOID <-
  label %>%
  filter(!GEOID %in% trainGEOID$GEOID)
test <- cbind(testGEOID,test)
train$cat <- "train"
test$cat <- "test"
all  <- rbind(test,train)
write.csv(all,"data/model/regression_pred.csv")
all$error <- all$.pred-all$maxhd_macro
all$error_actual <- exp(all$.pred)-exp(all$maxhd_macro)


error <-
  all %>%
  select(GEOID,maxhd_macro,.pred,cat,error,error_actual) %>%
  rename(observation = maxhd_macro,
         prediction = .pred)%>%
  mutate(exp_obs = exp(observation),
         exp_pred = exp(prediction))

mae(error$exp_obs,error$exp_pred) #4.369397 for all

blocks_error <-
  blocks %>%
  select(GEOID) %>%
  left_join(error)

breaks_qt <- classIntervals(c(min(blocks_error$error_actual) - .00001, blocks_error$error_actual), n = 5, style = "quantile")
blocks_error <-as(blocks_error,Class = "Spatial")
geo_proj = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
blocks_error = spTransform(blocks_error,geo_proj)
colfunc <- colorRampPalette(c("blue", "white","red"))
colfunc(5)
palette5 <- c( "#0000FF","#7F7FFF","#FFFFFF","#FF7F7F","#FF0000")
pal5 <-colorBin(palette5,blocks_error@data$error_actual,bins=breaks_qt[["brks"]],pretty=TRUE,right=FALSE,na.color = "grey")
popup <-paste('<strong>GEOID: </strong>',blocks_error@data$GEOID,
              '<hr><strong>Observation: </strong>',round(blocks_error@data$observation,2),
              '<hr><strong>Prediction: </strong>',round(blocks_error@data$prediction,2),
              '<hr><strong>Error: </strong>',round(blocks_error@data$error,2),
              '<hr><strong>Actual Error (No Log Transformed): </strong>',round(blocks_error@data$error_actual,2),
              '<hr><strong>Category: </strong>',blocks_error@data$cat
)


leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = -111.8, lat = 34.3, zoom = 6)%>%
  addPolygons(
    data=blocks_error,
    weight=0.5,
    color="grey",
    smoothFactor = 0.1,
    fillOpacity = 0.8,
    fillColor = ~pal5(error_actual),
    popup = popup
  )%>%
  addLegend("bottomright",
            pal = pal5,
            values = blocks_error@data$error_actual,
            title = "Error",
            opacity = 1)

library(plyr)

mu <- all %>%
  filter(cat=="train")%>%
  transmute(predicted = exp(.pred),
            observed = exp(maxhd_macro)) %>%
  mutate(error = predicted-observed,
         group = factor(ntile(observed, 10))) %>%
  mutate(group = as.numeric(as.character(group)))%>%
  ddply(.,"group", summarise, grp.mean=mean(error))%>%
  filter(group>=8)

Dens_10 <-
  all %>%
  filter(cat=="train")%>%
  transmute(predicted = exp(.pred),
            observed = exp(maxhd_macro)) %>%
  mutate(error = predicted-observed,
         group = factor(ntile(observed, 10))) %>%
  mutate(group = as.numeric(as.character(group)))%>%
  filter(group>=8)%>%
  ggplot(aes(x=error))+
  geom_histogram(aes(y=..density..),color="black", fill="white",bins=40)+
  geom_density(alpha=.2,fill="#FF6666")+
  geom_vline(data=mu,aes(xintercept=grp.mean),
             color="blue", linetype="dashed", size=1)+
  geom_vline(xintercept = 0,color="red",linetype="dashed",size=1)+
  annotate(geom="text",x=-60,y=0.15,label="Group 8 Mean Error: -1.153792",color="red")+
  annotate(geom="text",x=-60,y=0.1,label="Group 9 Mean Error: -3.841493",color="red")+
  annotate(geom="text",x=-60,y=0.05,label="Group 10 Mean Error: -12.362586",color="red")+
  facet_grid(group ~.)

Dens_10
ggsave(plot = last_plot(), filename = "viz/Error Distribution for top 3 deciles.jpeg", height = 6, width = 8, dpi = 300)

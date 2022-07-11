##################################
## Regression Data
##################################

## packages
library(tidyverse)
library(tidymodels)
library(sf)
library(tigris)
library(tidycensus)
library(tmap)
library(tmaptools)
library(spdep)

blocks <- 
  block_groups(state = "34", cb = TRUE, class = 'sf') %>% 
  st_transform(3857) %>% 
  transmute(GEOID, 
            area_total = units::set_units(st_area(geometry), acres),
            centroid = st_centroid(geometry)) %>%
  mutate(X = st_coordinates(centroid)[, 1],
         Y = st_coordinates(centroid)[, 2]) %>%
  select(-centroid)

### TEMP PRE EXTRACTED CSV
DATA_DIR = "C:\\Users\\nelms\\OneDrive - PennO365\\Penn\\Wharton\\NLURI\\data"
pre_extract_csv = 
  "regression_dataframe_nj_updated_220701.csv" %>%
  paste(DATA_DIR, ., sep='\\')

## data for the left side of the regression
regression <- 
  read_csv(pre_extract_csv, col_type = cols("GEOID" = col_character()))

## data to put through the spatial lag function
lag_data <- 
  regression %>%
    select(-maxhd_micro, -maxhd_macro) %>% 
    mutate(built_change = factor(built_change)) %>%
  left_join(blocks) %>%
    mutate(area_total = units::drop_units(area_total)) %>%
    st_as_sf()

## smoothing
infill <- nn_interpolate(lag_data, 3)

     
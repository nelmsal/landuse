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

PROJECT_CRS = 3857
STATE_FIPS = "34"
YEAR = 2020

### TEMP PRE EXTRACTED CSV
DATA_DIR = "C:\\Users\\nelms\\OneDrive - PennO365\\Penn\\Wharton\\NLURI\\data"
pre_extract_csv =
  "regression_dataframe_nj_updated_220701.csv" %>%
  paste(DATA_DIR, ., sep='\\')

## block group geometry
blocks.data <-
  block_groups(
    state = STATE_FIPS,
    year = YEAR,
    cb = TRUE, class = 'sf') %>%
  st_transform() %>%
  transmute(
    GEOID,
    area_total = units::set_units(st_area(geometry), acres)) %>%
  # from helper script
  getXYcols(.)

## data for the left side of the regression
base.data <-
  read_csv(
    pre_extract_csv,
    col_type = cols("GEOID" = col_character())
    )

## data to put through the spatial lag function
lag.data <-
  base.data %>%
    select(-maxhd_micro, -maxhd_macro) %>%
    mutate(built_change = factor(built_change)) %>%
  left_join(blocks.data) %>%
    mutate(area_total = units::drop_units(area_total)) %>%
    st_as_sf()

## smoothing
infill.data <-
  nn_interpolate(lag.data, 3)

## combine data
reg.data <-
  left_join(
    base.data,
    infill.data %>%
      select(., -geometry))
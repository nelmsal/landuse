##################################
## Regression Data
##################################

## packages
library(tidyverse)
library(tidymodels)
library(sf)
library(tigris)
library(tidycensus)
census_api_key("f16397dacea3353a0e221135c845724e872968be",install=TRUE,overwrite=TRUE)
library(tmap)
library(tmaptools)
library(spdep)
library(reshape2)

setwd("\\\\econha01/client/CLIENT/1 Consulting/Freddie Mac/Prediction Modeling/ID")
# setwd("/Volumes/client/CLIENT/1 Consulting/Freddie Mac/Prediction Modeling/Florida")

blocks <-
  block_groups(state = geography, cb = FALSE, class = 'sf',year=2019) %>%
  st_transform(geog_proj) %>%
  transmute(GEOID,
            area_total = units::set_units(st_area(geometry), acres),
            centroid = st_centroid(geometry))%>%
  mutate(X = st_coordinates(centroid)[, 1],
         Y = st_coordinates(centroid)[, 2]) %>%
  dplyr::select(-centroid)
  
plot(st_geometry(blocks))

zoning <-
  # read_csv("~/Dropbox/ESI-Wharton Freddie Project/Deliverables/MD_2021-10-25/MD_BlockGroup_Dataset_2021-10-25.csv",
  #          col_type = cols("BG2010" = col_character())) %>%
  read_csv("data/density/ID_summary.bg-05262022.csv",
           col_type = cols("GEOID" = col_character())) %>% #MAXHD.hmacro
  glimpse() %>%
  transmute(GEOID,
            maxhd_micro = MAXHD.hmicro,
            maxhd_macro = GISMAXHD.hmacro)

sum(is.na(zoning$maxhd_macro)) #81 NA
# sum(is.na(zoning$maxhd_macro) & is.na(zoning$maxhd_micro)) #3753 NA

zoning <-
  zoning %>%
  drop_na() #387


hist(log(zoning$maxhd_macro))
hist(log(zoning$maxhd_micro))

# income <-
#   vroom::vroom("~/Desktop/R/git/philamonitor/data/census/data/cbg_b19.csv") %>%
#   filter(census_block_group %in% blocks$GEOID) %>%
#   select(census_block_group, B19301e1, B19301m1) %>%
#   transmute(GEOID = census_block_group,
#             median_income = B19301e1)
income <- get_acs(
  geography="block group",
  table = "B19013",
  year=2019,
  state="Idaho",
  geometry=FALSE
) %>%
  dplyr::select(GEOID,estimate)%>%
  rename(median_income=estimate)

# size <-
#   vroom::vroom("~/Desktop/R/git/philamonitor/data/census/data/cbg_b25.csv") %>%
#   filter(census_block_group %in% blocks$GEOID) %>%
#   select(census_block_group, B25010e1) %>%
#   transmute(GEOID = census_block_group,
#             household_size = B25010e1)

size <- get_acs(
  geography='block group',
  table='B25010',
  year=2019,
  state="Idaho",
  geometry=FALSE
)%>%
  filter(variable=="B25010_001")%>%
  dplyr::select(GEOID,estimate)%>%
  rename(household_size=estimate)

# population <-
#   vroom::vroom("~/Desktop/R/git/philamonitor/data/census/data/cbg_b02.csv") %>%
#   filter(census_block_group %in% blocks$GEOID) %>%
#   select(census_block_group, B02001e1, B02001e2) %>%
#   transmute(GEOID = census_block_group,
#             population = B02001e1,
#             pct_nonwhite = 1 - (B02001e2 / B02001e1)) %>%
#   left_join(blocks) %>%
#   transmute(GEOID,
#             density = population / units::drop_units(area_total),
#             pct_nonwhite)

population <- get_acs(
  geography='block group',
  table='B02001',
  year=2019,
  state="Idaho",
  geometry=FALSE
)%>%
  filter(variable %in% c("B02001_001","B02001_002"))%>%
  select(GEOID, variable,estimate)%>%
  spread(variable,estimate) %>%
  transmute(
    GEOID,
    population = B02001_001,
    pct_nonwhite = 1 - (B02001_002 / B02001_001)
  )%>%
  left_join(blocks,by="GEOID") %>%
  transmute(GEOID,
            density = population / units::drop_units(area_total),
            pct_nonwhite)


area_topology <-
  read_csv("python/area_characteristics_id.csv", col_type = cols("GEOID" = col_character())) %>%
  left_join(blocks) %>%
  transmute(GEOID,
            isoperi_mean,
            building_size = area_normed,
            intersection_density = node_count / units::drop_units(area_total),
            prop_4way,
            orientation_order)

area_morphology <-
  # blocks_complete %>%
  # st_drop_geometry() %>%
  read_csv("data/sensing/remote_sensing_id.csv")%>%
  transmute(GEOID,
            ndvi_mean,
            built_intensity = total_development_norm,
            built_change = case_when(development_change > 0 ~ 1,
                                     TRUE ~ 0))

## cities and towns file (centroid of the shape)
cities <-
  read_csv("data/cities/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE) %>%
  filter(population > 100000) %>%
  st_transform(3857) %>%
  mutate(X = st_coordinates(geometry)[, 1],
         Y = st_coordinates(geometry)[, 2]) %>%
  glimpse()

towns <-
  read_csv("data/cities/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326, remove = FALSE) %>%
  filter(population > 10000) %>%
  st_transform(3857) %>%
  mutate(X = st_coordinates(geometry)[, 1],
         Y = st_coordinates(geometry)[, 2]) %>%
  glimpse()

## distance to cities
d_city <-
  FNN::get.knnx(select(st_drop_geometry(cities), X, Y), select(st_drop_geometry(blocks), X, Y), k = 1) %>% #k-nearest neighbor searching algorithms
  magrittr::use_series("nn.dist") %>% # an n*k matrix for the nearest neighbor Euclidean distances
  as_tibble() %>%
  rownames_to_column(var = "id") %>%
  left_join(rownames_to_column(blocks, var = "id")) %>%
  transmute(GEOID, dist_city = V1)

## distance to smaller towns
d_town <-
  FNN::get.knnx(select(st_drop_geometry(towns), X, Y), select(st_drop_geometry(blocks), X, Y), k = 10) %>%
  magrittr::use_series("nn.dist") %>%
  as_tibble() %>%
  rownames_to_column(var = "id")%>%
  pivot_longer(!id) %>% #pivot_longer() "lengthens" data, increasing the number of rows and decreasing the number of columns.
  group_by(id) %>%
  summarise(dist_towns = mean(value)) %>%
  left_join(rownames_to_column(blocks, var = "id")) %>%
  select(GEOID, dist_towns)

## data for the left side of the regression
area_morphology$GEOID <- as.character(area_morphology$GEOID)
regression <-
  zoning %>%
  left_join(income) %>%
  left_join(size) %>%
  left_join(population) %>%
  left_join(area_topology) %>%
  left_join(area_morphology) %>%
  left_join(d_town) %>%
  left_join(d_city) %>%
  mutate(built_change = factor(built_change)) %>%
  write_csv("data/model/regression_dataframe_id_updated.csv") %>%
  glimpse()

## data to put through the spatial lag function: https://rspatial.org/raster/analysis/7-spregression.html
lag_data <-
  income %>%
  left_join(size) %>%
  left_join(population) %>%
  left_join(area_topology) %>%
  left_join(area_morphology) %>%
  left_join(d_town) %>%
  left_join(d_city) %>%
  mutate(built_change = factor(built_change)) %>%
  left_join(blocks) %>%
  mutate(area_total = units::drop_units(area_total)) %>%
  st_as_sf()

## smoothing
infill <- nn_interpolate(lag_data, 3) #average the values of 3 depth neighborhoods

# sum(zoning$maxhd_micro==0) #13
sum(zoning$maxhd_macro==0) #29

save(regression,file="R/regression.Rda")
save(infill,file='R/infill.Rda')
save(lag_data,file="R/lag_data.Rda")


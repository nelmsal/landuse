###############################
## Protected areas
###############################
setwd("//econha01/client/CLIENT/1 Consulting/Freddie Mac/Prediction Modeling/AZ")
## packages
library(tidyverse)
library(sf)
library(tigris)
library(tidycensus)
census_api_key("f16397dacea3353a0e221135c845724e872968be",install=TRUE,overwrite=TRUE)
## set the state
geography <- "04"

# get name
geog_name <- 
  tigris::fips_codes %>%
  filter(state_code == geography) %>%
  distinct(state_name) %>%
  pull()

# get local crs
geog_proj <-
  crsuggest::crs_sf %>%
  filter(str_detect(crs_name, "NAD83\\(HARN\\)"),
         crs_units == "m") %>%
  filter(str_detect(crs_name, geog_name)) %>%
  slice(1) %>%
  pull(crs_proj4)

# another way...
# crsuggest::suggest_crs(block_groups(state = geography, cb = TRUE, class = 'sf'))

## block groups
blocks <- 
  block_groups(state = geography, cb = FALSE, class = 'sf',year=2019) %>% 
  st_transform(geog_proj) %>% 
  transmute(GEOID, 
            area_total = units::set_units(st_area(geometry), m^2),
            difference = (units::drop_units(area_total) - ALAND - AWATER) / units::drop_units(area_total))

plot(select(blocks, difference))

## state boundary
border <- 
  blocks %>%
  st_union() %>%
  st_combine()

## which layers do we want?
layers <- st_layers("//econha01/client/CLIENT/1 Consulting/Freddie Mac/Prediction Modeling/National Data/protected areas/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg")
layers <- layers$name[c(6:8, 10)]

## load all layers 
pad <- 
  map(layers,
      ~st_read("//econha01/client/CLIENT/1 Consulting/Freddie Mac/Prediction Modeling/National Data/protected areas/PADUS2_1_Geopackage/PADUS2_1_Geopackage.gpkg",
               layer = .x) %>%
        st_transform(st_crs(border)) %>%
        st_intersection(border))

## difference out the protected areas 
protections <- 
  reduce(imap(pad, 
              function(x, y){
                designation <- 
                  x %>%
                  st_union() %>% 
                  st_combine()
                
                layer_label <- layers[y]
                
                tictoc::tic()
                designation_area <- 
                  blocks %>%
                  st_difference(designation) %>%
                  mutate(area_protected = area_total - units::set_units(st_area(geometry), m^2),
                         protection = layer_label) %>%
                  st_drop_geometry() %>%
                  select(-area_total)
                tictoc::toc()
                
                return(designation_area)
                
              }),
         rbind)

## aggregate all protections
protections %>%
  left_join(blocks) %>%
  mutate(area_total = units::drop_units(area_total), 
         area_protected = units::drop_units(area_protected),
         area_protected = round(area_protected, 4)) %>%
  pivot_wider(id_cols = GEOID:area_total,
              names_from = protection,
              values_from = area_protected) %>%
  mutate_at(vars(starts_with("PAD")), funs(case_when(is.na(.) ~ area_total, TRUE ~ .))) %>%
  write_csv("data/Protected Areas/protections_az.csv")

protections %>%
  left_join(blocks) %>%
  mutate(area_total = units::drop_units(area_total), 
         area_protected = units::drop_units(area_protected),
         area_protected = round(area_protected, 4)) %>%
  pivot_wider(id_cols = GEOID:area_total,
              names_from = protection,
              values_from = area_protected) %>%
  mutate_at(vars(starts_with("PAD")), funs(case_when(is.na(.) ~ area_total, TRUE ~ .))) %>%
  select(-area_total) %>%
  select(GEOID, starts_with("PAD")) %>%
  pivot_longer(!GEOID) %>%
  mutate(name = str_remove_all(name, "PADUS2_1")) %>%
  left_join(blocks) %>%
  st_as_sf() %>%
  ggplot(aes(fill = value / (1000 * 1000))) +
  geom_sf(colour = '#ffffff', size = 0) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(n = 9, name = 'Greens'),
                       name = expression(bold(area~(km^2)))) +
  facet_wrap(~name) + 
  theme_void() +
  theme(strip.text = element_text(face = 'bold', size = 15))

ggsave(plot = last_plot(), filename = "data/Protected Areas/arizona_protected_areas.jpeg", height = 10, width = 12, dpi = 300)

####Merge Four Layers####
all_pad <- dplyr::bind_rows(pad)
all_pad <-
  all_pad %>%
  st_union() %>%
  st_combine()%>%
  st_as_sf()%>%
  st_transform(geog_proj)%>%
  mutate(pad_area = units::set_units(st_area(.), m^2))
all_pad$pad_area #247642422362 [m^2]
sum(blocks$area_total) #295329354037 [m^2]
all_pad$pad_area/sum(blocks$area_total) #84%
plot(all_pad)
st_write(all_pad,"data/Protected Areas/pad_Arizona.shp")

area <-
  st_interpolate_aw(all_pad["pad_area"],blocks,extensive=TRUE)%>%
  as.data.frame(.)%>%
  left_join(blocks,.,by="geometry")%>%
  st_drop_geometry()%>%
  units::drop_units()%>%
  mutate(pad_area = replace_na(pad_area,0),
         coverage = pad_area*100/area_total)

ggplot(area, aes(x=coverage)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=10)+
  geom_density(alpha=.2, fill="#FF6666") 
area$area_total = area$area_total *0.000247105
area$pad_area = area$pad_area*0.000247105
area <- get_acs(
  geography='block group',
  table='B02001',
  year=2019,
  state="Arizona",
  geometry=FALSE
)%>%
  filter(variable %in% c("B02001_001"))%>%
  select(GEOID, variable,estimate)%>%
  spread(variable,estimate) %>%
  transmute(
    GEOID,
    population = B02001_001
  )%>%
  left_join(area,by="GEOID") %>%
  transmute(GEOID,
            density = population / area_total)%>%
  right_join(area,by="GEOID")

v19 <- load_variables(2019,"acs5",cache=TRUE)
#B25001_001 for housing units

area <-get_acs(
  geography='block group',
  table='B25001',
  year=2019,
  state="Arizona",
  geometry=FALSE
)%>%
  filter(variable %in% c("B25001_001"))%>%
  select(GEOID, variable,estimate)%>%
  spread(variable,estimate) %>%
  transmute(
    GEOID,
    units = B25001_001
  )%>%
  left_join(area,by="GEOID") %>%
  transmute(GEOID,
            housing_density = units / area_total)%>%
  right_join(area,by="GEOID")


write.csv(area,"data/Protected Areas/area.csv")  
####Use Andrew Data####
protected <- read.csv('data/Protected Areas/protections_04.csv')
protected$GEOID <- as.character(protected$GEOID)
protected$GEOID<-paste0("0",protected$GEOID)
protected$coverage <- protected$area_protected*100/protected$area_total
blocks <- 
  block_groups(state = geography, cb = FALSE, class = 'sf',year=2019) %>% 
  st_transform(geog_proj) %>% 
  transmute(GEOID, 
            area_total = units::set_units(st_area(geometry), m^2),
            difference = (units::drop_units(area_total) - ALAND - AWATER) / units::drop_units(area_total))
area <-
  blocks%>%
  st_drop_geometry()%>%
  left_join(protected %>% select(GEOID,coverage),by="GEOID")
sum(is.na(area$coverage)) #4178 - 3999 = 179
area$coverage <- ifelse(is.na(area$coverage),100,area$coverage)
area <- units::drop_units(area)
area$area_total = area$area_total *0.000247105
area <- get_acs(
  geography='block group',
  table='B02001',
  year=2019,
  state="Arizona",
  geometry=FALSE
)%>%
  filter(variable %in% c("B02001_001"))%>%
  select(GEOID, variable,estimate)%>%
  spread(variable,estimate) %>%
  transmute(
    GEOID,
    population = B02001_001
  )%>%
  left_join(area,by="GEOID") %>%
  transmute(GEOID,
            density = population / area_total)%>%
  right_join(area,by="GEOID")

v19 <- load_variables(2019,"acs5",cache=TRUE)
#B25001_001 for housing units

area <-get_acs(
  geography='block group',
  table='B25001',
  year=2019,
  state="Arizona",
  geometry=FALSE
)%>%
  filter(variable %in% c("B25001_001"))%>%
  select(GEOID, variable,estimate)%>%
  spread(variable,estimate) %>%
  transmute(
    GEOID,
    units = B25001_001
  )%>%
  left_join(area,by="GEOID") %>%
  transmute(GEOID,
            housing_density = units / area_total)%>%
  right_join(area,by="GEOID")


write.csv(area,"data/Protected Areas/area.csv")  

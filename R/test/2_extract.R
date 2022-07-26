###############################
## Area Characteristics
###############################
setwd(SENSING_DIR)

# packages
library(tidyverse)
library(sf)
library(stars)
library(terra)
library(tmap)
library(tmaptools)
library(exactextractr)
library(tigris)

## set the state
STATE_CRS <- get_crs(STATE_FIPS)

## get the images from the (combined) drive folder
images <- fs::dir_ls(glue::glue("./tiles"))

## aggregate the images
tictoc::tic()
# load them all with terra::rast, iterating over the paths
parts <- map(images, ~terra::rast(.x))
# create raster collection with terra::sprc and terra::merge them to a single raster
whole <- terra::merge(terra::sprc(parts))
# take the time for good measure: Nevada was 70 minutes
tictoc::toc()

#Nevada: 14 mins
tictoc::tic()
terra::writeRaster(whole, filename = comp_csv)
tictoc::toc()

## plot to see all the bands
plot(whole)

## block groups
blocks <-
  block_groups(
    state = STATE_FIPS, cb = TRUE,
    class = 'sf', year = 2019) %>%
  # use local crs for most accurate area measurement
  st_transform(STATE_CRS) %>%
  transmute(GEOID,
            STATEFP,
            COUNTYFP,
            area_total = units::drop_units(units::set_units(st_area(geometry), mi^2))) %>%
  # get back to the GEE CRS
  st_transform(gee_crs)

# plot to check
plot(st_geometry(blocks))

counties <- group_split(blocks, COUNTYFP)
zonal_stats <- map_dfr(counties, process_county)


# COUNTY
process_county <-
  function(county){

    plot(county)

    tictoc::tic()
    clipped <- terra::crop(whole, st_combine(st_union(county)))
    tictoc::toc()

    ## zonal statistics, a lot of them
    tictoc::tic()
    # start with a total pixel count to use  as the dividend in normalizations
    pixels <- terra::classify(clipped$landcover_2016, matrix(c(0, 100, 1), ncol = 3, byrow =TRUE))

    extract_cnty = function(raster_cells, agg_fun, county_poly=county){
      exact_extract(pixels, county_poly, agg_fun)
    }
    extract_cnty_s = function(raster_cells, county_poly=county) {
      extract_cnty(raster_cells, "sum", county_poly)
    }
    extract_cnty_m = function(raster_cells, county_poly=county) {
      extract_cnty(raster_cells, "mean", county_poly)
    }

    county$pixels <- extract_cnty(pixels, 'sum')

    # various spectral indicies
    county$ndvi_mean <- extract_cnty_m(clipped$NDVI)
    county$ndwi_mean <- extract_cnty_m(clipped$NDWI)
    county$ndbi_mean <- extract_cnty_m(clipped$NDBI)

    # impervious surface
    county$impervious_mean <- extract_cnty_m(clipped$impervious_2016)

    # slope
    county$slope_mean <- extract_cnty_m(clipped$slope)
    county$slope_sum <- extract_cnty_s(clipped$slope)

    # night lights
    county$nightlights_2016_mean <- extract_cnty_m(clipped$avg_rad_1)
    county$nightlights_2016_sum <- extract_cnty_s(clipped$avg_rad_1)
    county$nightlights_2021_mean <- extract_cnty_m(clipped$avg_rad)
    county$nightlights_2021_sum <- extract_cnty_s(clipped$avg_rad)

    county$stablelights_y <- extract_cnty_m(clipped$stablelights_y)
    county$stablelights_b <- extract_cnty_m(clipped$stablelights_b)

    # population
    county$daytime_pop_mean <- extract_cnty_m(clipped$avg_rad)
    county$daytime_pop_sum <- extract_cnty_s(clipped$avg_rad)
    county$nighttime_pop_mean <- extract_cnty_m(clipped$avg_rad_1)
    county$nighttime_pop_sum <- extract_cnty_s(clipped$avg_rad_1)

    # more ways to get at impervious, as summed "high impervious" pixels
    rewhole <- terra::classify(
      clipped$impervious_2016,
      matrix(c(0, 50, 0,  50, 100, 1),
      ncol = 3, byrow =TRUE))
    county$impervious_count <- extract_cnty_s(rewhole)

    # another way to get at slope in the same way
    rewhole <- terra::classify(
      clipped$slope,
      matrix(c(0, 20, 0,  20, 100, 1),
      ncol = 3, byrow =TRUE))
    county$slope_count <- extract_cnty_s(rewhole)

    # reclassify USGS landcover to simple ordinal framework for easier math
    total_dev <- terra::classify(
      clipped$landcover_2016,
      matrix(c(
        10, 20, 0, # not built : 0
        20, 21, 1, # low built : 1
        21, 22, 2, # built more: 2
        22, 23, 3, # built more: 3
        23, 24, 4, # built more: 4
        30, 95, 0), # not built: 0
      ncol = 3, byrow =TRUE))
    # sum it
    county$total_development <- extract_cnty_s(total_dev)

    # just get a high intensity
    rewhole <- terra::classify(
      clipped$landcover_2016,
      matrix(c(0, 23, 0, 23, 24, 1, 30, 100, 0),
      ncol = 3, byrow = TRUE))
    county$high_development <- extract_cnty_s(rewhole)

    # just get at medium intensity
    rewhole <- terra::classify(
      clipped$landcover_2016,
      matrix(c(0, 22, 0, 22, 23, 1, 23, 100, 0),
      ncol = 3, byrow =TRUE))
    county$medium_development <- extract_cnty_s(rewhole)

    # just get at low intensity
    rewhole <- terra::classify(
      clipped$landcover_2016,
      matrix(c(0, 21, 0, 21, 22, 1, 22, 100, 0),
      ncol = 3, byrow =TRUE))
    county$low_development <- extract_cnty_s(rewhole)

    ## historic
    total_dev <- terra::classify(
      clipped$landcover_2006,
      matrix(c(
        10, 20, 0,
        20, 21, 1,
        21, 22, 2,
        22, 23, 3,
        23, 24, 4,
        30, 95, 0),
      ncol = 3, byrow =TRUE))

    rewhole <- terra::classify(
      clipped$impervious_2006,
      matrix(c(0, 50, 0,  50, 100, 1),
      ncol = 3, byrow =TRUE))

    county$historic_development <- extract_cnty_s(total_dev)
    county$historic_impervious <- extract_cnty_s(rewhole)

    tictoc::toc()

    ## aggregating
    county_complete <-
      county %>%
      st_as_sf() %>%
      mutate(
        total_development_norm = total_development / pixels,
        total_development_norm_historic = historic_development / pixels,
        high_development_norm = high_development / pixels,
        medium_development_norm = medium_development / pixels,
        low_development_norm = low_development / pixels,
        daytime_pop_norm = daytime_pop_sum / area_total,
        nighttime_pop_norm = nighttime_pop_sum / area_tota,
        slope_sum_norm = slope_sum / pixels,
        slope_count_norm = slope_count / pixels,
        impervious_count_norm = impervious_count / pixels,
        impervious_count_norm_historic = historic_impervious / pixels) %>%
      mutate(
        development_change =
          (total_development_norm - total_development_norm_historic) / total_development_norm_historic,
        impervious_change =
          (impervious_count_norm - impervious_count_norm_historic) / impervious_count_norm_historic,
        nightlights_mean_change =
          nightlights_2021_mean - nightlights_2016_mean,
        nightlights_sum_change =
          (nightlights_2021_sum / area_total) - (nightlights_2016_sum / area_total),
        pop_difference_norm = (daytime_pop_sum - nighttime_pop_sum) / area_total
        )

    return(county_complete)

  }

write_csv(
  st_drop_geometry(zonal_stats),
  remote_sensing_csv)

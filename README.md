
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ServicePlanningFunctions

<!-- badges: start -->
<!-- badges: end -->

The goal of ServicePlanningFunctions is to standardize functions
repeatedly used in service planning analyses at King County Metro. These
are the functions that have been developed over multiple analyses and
projects, so they are not all necessarily related to each other. Read
through the function descriptions below to learn about available
functions

## Installation

You can install the development version of ServicePlanningFunctions from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KC-Metro-Transit/ServicePlanningFunctions")
```

## save_objects() Example

save_objects() is designed to work with the purrr package to iterate
over all objects in your environment and save a copy of them to disk.
This is useful if you are trying to save your work or if you are getting
data ready to import into a Shiny app.

``` r
library(ServicePlanningFunctions)
## basic example code
object_1 <- cars
object_2 <- iris
object_3 <- gapminder

object_list <- ls() #list all object names you want to export


object_list <- object_list[! object_list %in% c("object_list")] #remove the list of objects from the list of names of objects to export

purrr::map(object_list, file_location ="your/file/location",  save_objects) #choose where the files should write to disk
```

## remove_water() Example

``` r
 kc_tracts <- sf::read_sf(fs::path_package( "extdata", "2020_Census_Tracts_for_King_County___tracts20_area.shp", package = "ServicePlanningFunctions"))

kc_tracts_no_water <- remove_water(polygon = kc_tracts, state_code = "WA", county_code = "King", crs = 2926)

mapview::mapview(kc_tracts_no_water)
```

## combine_gtfs() Example

``` r

gtfs_folder <- fs::path_package( "extdata", "combine_gtfs", package = "ServicePlanningFunctions")

madison_st_area_gtfs <- combine_gtfs(gtfs_filepath = gtfs_folder, designated_start_date = 20240901, designated_end_date = 20250330, save_csv = FALSE, save_RDS = FALSE, output_folder = NULL)
```

## clean_service_rte_num() Example

``` r


spring_24_gtfs <- tidytransit::read_gtfs(path =  fs::path_package( "extdata", "gtfs", "241_gtfs.zip", package = "ServicePlanningFunctions"))

spring_24_routes <- clean_service_rte_num(spring_24_gtfs$routes, netplan_gtfs = FALSE)

netplan_sept24_gtfs <- tidytransit::read_gtfs(path =  fs::path_package( "extdata", "gtfs", "SEPT24_TRIP_GTFS.zip", package = "ServicePlanningFunctions"))

netplan_routes <- clean_service_rte_num(netplan_sept24_gtfss$routes, netplan_gtfs = TRUE)
```

## gtfs_calendar_full_dates() Example

``` r

spring_24_gtfs <- tidytransit::read_gtfs(path =  fs::path_package( "extdata", "gtfs", "241_gtfs.zip", package = "ServicePlanningFunctions"))

full_cal <- gtfs_calendar_full_dates(calendar = spring_24_gtfs$calendar, calendar_dates = spring_24_gtfs$calendar_dates, netplan_gtfs = FALSE)

netplan_sept24_gtfs <- tidytransit::read_gtfs(path =  fs::path_package( "extdata", "gtfs", "SEPT24_TRIP_GTFS.zip", package = "ServicePlanningFunctions"))

netplan_full_cal <- gtfs_calendar_full_dates(
                            calendar = netplan_sept24_gtfs$calendar,                                           calendar_dates = netplan_sept24_gtfs$calendar_dates, 
                            netplan_gtfs = TRUE, 
                            designated_start_date = 20240901,                                                  designated_end_date = 20250330)
```

## st_erase() Example

A helper function for remove_water() that clips a polygon out from
another polygon. Results in a “donut” polygon.

``` r
 big_box <- sf::st_polygon(x= list(polygon))
 plot(sf::st_geometry(big_box))
 shape_to_remove <- matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#create smaller polygon to subset
 polygon_to_remove <- sf::st_polygon(x = list(shape_to_remove))
 plot(sf::st_geometry(polygon_to_remove))
# Polygon with smaller area removed
 clipped_polygon <- st_erase(big_box, polygon_to_remove)
 plot(st_geometry(clipped_polygon))
```

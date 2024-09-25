#' Count Transit Trips by Geography
#'
#' @param gtfs_object A GTFS list-object available in your working environment
#' @param begin_time Character. HH:MM:SS. When does the analysis period start?
#' @param end_time Character. HH:MM:SS. When does the analysis period end?
#' @param analysis_period Character. Name of analysis period. Example: AM, PM
#' @param day_type Character. Day of Week. Options are "weekday", "saturday", "sunday", "week", "weekend"
#' @param project_name Character. Name of project. Used for naming exports
#' @param netplan_gtfs T/F. Is this GTFS from NetPlan?
#' @param geography Character. What geography do you want to use to summarise results? Options are "block_group", "tract", "quarter_mile_hex", "eigth_mile_hex"
#' @param gtfs_type Character. Does the GTFS reflect the baseline or proposed network? Options are "baseline" or "proposed".
#' @param output_folder Character. Where do you want the outputs saved?
#' @param save_csv T/F. Do you want to export csvs?
#' @param save_RDS T/F. Do you want to export RDS?
#' @param run_id Numeric. Useful for using this function with pmap.
#'
#' @return List of trips by route/geography and by geography (summed)
#' @export
#'
#' @examples
count_trips_by_geography <- function(gtfs_object, begin_time, end_time, analysis_period,
                                     day_type, project_name, netplan_gtfs=F, geography, save_csv=F,save_RDS=F,
                                     gtfs_type, output_folder, run_id=1){


  start_time_vec <- stringr::str_split_fixed(begin_time, pattern= ":", 3)

  start_time_sec <- (as.numeric(start_time_vec[,1])*3600) + as.numeric(start_time_vec[,2])*60 + as.numeric(start_time_vec[,3])

  end_time_vec <- stringr::str_split_fixed(end_time, pattern= ":", 3)

  end_time_sec <- (as.numeric(end_time_vec[,1])*3600) + as.numeric(end_time_vec[,2])*60 + as.numeric(end_time_vec[,3])




  #get geography #####

  if( geography == "block_group") {

    acs <- sf::read_sf(fs::path_package( "extdata", "blkgrp20_shore.shp",
                                         package = "ServicePlanningFunctions")) %>%
      dplyr::rename(GEOID = GEO_ID_GRP) %>%
      sf::st_transform(2926) %>%
      sf::st_buffer(dist=50)
  } else if(geography == "tract") {
    acs <- sf::read_sf(fs::path_package( "extdata", "2020_Census_Tracts_for_King_County___tracts20_area.shp",
                                         package = "ServicePlanningFunctions")) %>%
      dplyr::rename(GEOID = GEO_ID_TRT) %>%
      sf::st_transform(2926)
    acs <- remove_water(acs) %>%
      sf::st_buffer(dist=50)
  } else if(geography == "eigth_mile_hex"){
    acs <- sf::read_sf(fs::path_package( "extdata", "eigth_mile_hex_grid.shp",
                                         package = "ServicePlanningFunctions")) %>%
      dplyr::rename(GEOID = rowid)
  } else if (geography == "quarter_mile_hex"){
    acs <- sf::read_sf(fs::path_package( "extdata", "quarter_mile_hex_grid.shp",
                                         package = "ServicePlanningFunctions")) %>%
      dplyr::rename(GEOID = rowid)
  }else{

    cli::cli_abort(c(
      "Geography Input Error:",
      "x" = "Geography parameter should be set to block_group, tract, eigth_mile_hex, or quarter_mile_hex."
    ))

  }
  #GTFS #####
  #set gtfs object to reference gtfs in global environment.

  if(!(("tidygtfs" %in% class(gtfs_object)) | ("gtfs"  %in% class(gtfs_object)) | ("list" %in% class(gtfs_object)))){
    cli::cli_abort(c(
      "Object Type Error",
      "x" = "The gtfs_object parameter requires a list or tidygtfs object. It is currently referencing a {class(gtfs_object)} object.",
      "i" = "Hint: Have you imported the gtfs using tidytransit or the combine_gtfs() function?"
    ))


  } else {
    gtfs <- gtfs_object

  }


    stops <- gtfs$stops %>%
      dplyr::distinct(stop_id, .keep_all = TRUE)
    # Routes #####


      routes <- clean_service_rte_num(gtfs$routes, netplan_gtfs = netplan_gtfs) %>%
        dplyr::mutate(route_num = as.numeric(service_rte_num)) %>%
      dplyr::mutate(vehicle_capacity = dplyr::case_when((service_rte_num < 400 & !(service_rte_num %in% c(200, 204, 224, 96, 98))) ~ 61,
                                            service_rte_num %in% c(671, 672, 673, 674, 675, 676, 677, 678) ~ 76,
                                            ( service_rte_num >= 900 & route_num <= 980) ~ 51,
                                            service_rte_num %in% c(200, 204, 224, 630) ~ 51,
                                            service_rte_num %in% c(599, 600) ~ 600,
                                            TRUE ~ 61))



    stop_times <- stringr::str_split_fixed(gtfs$stop_times$arrival_time, pattern= ":", 3)

    stop_time_sec <- (as.numeric(stop_times[,1])*3600) + as.numeric(stop_times[,2])*60 + as.numeric(stop_times[,3])

    gtfs$stop_times$seconds_after_midnight <- stop_time_sec

    # Calendar #####
    calendar_full_dates <- gtfs_calendar_full_dates(calendar = gtfs$calendar, calendar_dates = gtfs$calendar_dates,
                                                    netplan_gtfs = netplan_gtfs) %>%
      dplyr::group_by(service_id, day_of_week) %>%
      dplyr::summarize(weekly_trip_ct = sum(ct) / max(num_of_weeks), .groups = "keep")
    if( day_type == "weekday") { #filter to only weekday service
      calendar <- calendar_full_dates %>%
        dplyr::filter(!(day_of_week %in% c("saturday", "sunday") )) %>%
        dplyr::group_by(service_id) %>%
        dplyr::summarize(calendar_sum = sum(weekly_trip_ct))
      #Reduce gtfs_trips by keeping only those with service_ids matching the reduced calendar dataset (no weekend-only trips)

    } else if (day_type == "weekend") { #filter saturday and sunday
      calendar <- calendar_full_dates %>%
        dplyr::filter((day_of_week %in% c("saturday", "sunday") )) %>%
        dplyr::group_by(service_id) %>%
        dplyr::summarize(calendar_sum = sum(weekly_trip_ct))


    } else if (day_type == "saturday") { #filter saturday only
      calendar <- calendar_full_dates %>%
        dplyr::filter(!(day_of_week %in% c("saturday") )) %>%
        dplyr::group_by(service_id) %>%
        dplyr::summarize(calendar_sum = sum(weekly_trip_ct))
    } else if (day_type == "sunday") { #filter sunday only
      calendar <- calendar_full_dates %>%
        dplyr::filter(!(day_of_week %in% c( "sunday") )) %>%
        dplyr::group_by(service_id) %>%
        dplyr::summarize(calendar_sum = sum(weekly_trip_ct))
    } else if (day_type == "week") { #no filter
      calendar <- calendar_full_dates %>%
      dplyr::group_by(service_id) %>%
        dplyr::summarize(calendar_sum = sum(weekly_trip_ct))

    }


      # check datum of block groups to make sure stops are projected correctly. pass to st_transform for easy matching
      #this was altered to accommodate the fact that GIRO did not do the transformation from HARN #to world mercator


      stops_sf <- stops %>%
        tidyr::drop_na(stop_id) %>%
        dplyr::filter(!is.na(stop_lon)) %>%
        sf::st_as_sf(. , coords = c("stop_lon", "stop_lat"),
                 crs = 4326, agr= "constant") %>%
        sf::st_transform(2926)

      #join stops to block groups or tracts. This is how you know which stops are in each block group or tract.

      stops_geo <- sf::st_join( stops_sf,acs, join = sf::st_intersects) %>%
        sf::st_drop_geometry()

      #stops are getting read in as numeric but there are character types at the bottom of the document. Need to force type.
      gtfs$stop_times$stop_id <- as.character(gtfs$stop_times$stop_id )
      stops_geo$stop_id <- as.character(stops_geo$stop_id)
      stops$stop_id <- as.character(stops$stop_id)
      stops_sf$stop_id <- as.character(stops_sf$stop_id)
      gtfs$trips$trip_id <- as.character( gtfs$trips$trip_id )

      #this is the important function. It finds unique trips serving stops associated with the geography you provided.


      trips_by_geo_rte <- stops_geo %>%
        dplyr::select(stop_id, GEOID) %>%
        dplyr::left_join( gtfs$stop_times, multiple = "all") %>%
        dplyr::select(stop_id, GEOID, trip_id, arrival_time, seconds_after_midnight) %>%
        dplyr::left_join(gtfs$trips, multiple = "all") %>%
        dplyr::left_join(routes) %>%
        dplyr::left_join(calendar) %>%
        #filter out community ride and trailhead direct service but leave in the community shuttles
        dplyr::filter(!(service_rte_num %in% c(97, 90, 560:595, 629, 632:634, 636:662 , 680:772, 776:900, 932:999))) %>%
        dplyr::filter(service_id %in% calendar$service_id) %>%
        dplyr::filter(seconds_after_midnight >= start_time_sec & seconds_after_midnight <= end_time_sec ) %>%
        dplyr::group_by(GEOID, trip_id, route_id, service_rte_num, vehicle_capacity) %>%
        dplyr::slice(which.min(seconds_after_midnight)) %>%
        dplyr::mutate(trip_count = sum(calendar_sum)) %>% #summarise trips based on full calendar
        #unlike the week level analysis, we don't need to multiply the # of trips by the weekly trips.
        dplyr::ungroup() %>%
        dplyr::group_by(GEOID, route_id, service_rte_num) %>%
        dplyr::summarize(trips_per_rte = sum(trip_count, na.rm = TRUE),
                  route_capacity = sum(vehicle_capacity, na.rm=T))  %>%
        dplyr::mutate(output_type = "trips_by_rte_geo",
               run_id = run_id,
               GEOID = as.character(GEOID))

      routes_in_geo <- trips_by_geo_rte %>%
        dplyr::ungroup() %>%
        dplyr::group_by(GEOID) %>%
        dplyr::summarize(routes_in_geo = toString(service_rte_num) )

      trips_by_geo <- trips_by_geo_rte %>%
        dplyr::group_by(GEOID) %>%
        dplyr::summarize(trips_per_geo = sum(trips_per_rte, na.rm = TRUE),
                  total_capacity = sum(route_capacity, na.rm=TRUE) ) %>%
        dplyr::left_join(routes_in_geo) %>%
        dplyr::mutate(output_type = "trips_by_geo",
               run_id = run_id,
               GEOID = as.character(GEOID))


 if(save_csv == TRUE){

        readr::write_csv(trips_by_geo_rte, na= "", file = paste0(output_folder, "/",
                                                                 project_name,"_", lubridate::today() , "_", analysis_period, "_",
                                                                 geography ,"_" ,gtfs_type, "_by_route_trips.csv"))

        readr::write_csv(trips_by_geo, na= "",file = paste0(output_folder, "/", project_name,"_", lubridate::today() ,
                                                            "_", analysis_period, "_", geography ,"_" ,gtfs_type, "_trips.csv"))

        cli::cli_inform("CSV exports at {output_folder}")
      } else {
        cli::cli_inform("No CSVs saved")
      }

      if(save_RDS == TRUE){

        saveRDS(trips_by_geo_rte,  file = paste0(output_folder, "/",
                                                 project_name,"_", lubridate::today() , "_", analysis_period, "_",
                                                 geography ,"_" ,gtfs_type, "_by_route_trips.RDS"))

        saveRDS(trips_by_geo, file = paste0(output_folder, "/", project_name,"_", lubridate::today() ,
                                            "_", analysis_period, "_", geography ,"_" ,gtfs_type, "_trips.RDS"))

        cli::cli_inform("RDS export at {output_folder}")
      }else {
        cli::cli_inform("No RDS saved")
      }




  output <- vector("list", length=2)
  output[[1]]<-trips_by_geo_rte
  output[[2]]<- trips_by_geo
  names(output)<- c("routes", "geo")


  cli::cli_inform("Finishing {run_id}")
  return(output)



}

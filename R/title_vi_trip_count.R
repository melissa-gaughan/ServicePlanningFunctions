#' Title VI Transit Trips
#'
#' @param gtfs_object A GTFS list-object available in your working environment
#' @param project_name Character string. Used as unique name for file exports
#' @param netplan_gtfs T/F Is the GTFS from NetPlan?
#' @param geography A sf polygon object to use as analysis geography. If null, defaults to 2020 Census Tracts
#' @param gtfs_type Character string. Used in output naming for exports. Typically "baseline" or "proposed".
#' @param include_st T/F Should ST routes be included in the trip change analysis?
#' @param save_csv T/F Do you want to save csv files of the combined GTFS tables?
#' @param save_RDS T/F Do you want to save an RDS object of the combined GTFS list object?
#' @param output_folder File path of the place to export file exports
#'
#'
#' @return csv files of results
#' @export
#'
#' @examples
count_trips_by_geography_title_vi <- function(gtfs_object,  project_name, geography = NULL, netplan_gtfs,
                                              gtfs_type, include_st = F, save_csv = T, save_RDS = T, output_folder){
  #get geography from global environment#####

  if(is.null(geography)){
    cli::cli_inform(c(
      "Default Geography",
      "i" = "Using ACS 2020 Tracts with 50 ft edge buffer and water removed for analysis geography."))

      kc_tracts <- sf::read_sf(fs::path_package( "extdata", "2020_Census_Tracts_for_King_County___tracts20_area.shp",
                                             package = "ServicePlanningFunctions"))
      kc_tracts_no_water <- remove_water(polygon = kc_tracts, state_code = "WA",  county_code = "King", crs = 2926) %>%
        sf::st_buffer(50)

 acs <- kc_tracts_no_water

  } else if(!(("MULTIPOLYGON" %in% class(geography))| "POLYGON" %in% class(geography))){
    cli::cli_abort(c(
      "Object Type Error",
      "x" = "The geography parameter requires a polygon or multipolygon object. It is currently referencing a {class(geography)} object."
    ))
  } else {
  acs <- geography
  }

if(!(("tidygtfs" %in% class(gtfs_object)) | ("gtfs"  %in% class(gtfs_object)) | ("list" %in% class(gtfs_object)))){
  cli::cli_abort(c(
    "Object Type Error",
    "x" = "The gtfs_object parameter requires a list or tidygtfs object. It is currently referencing a {class(gtfs_object)} object.",
    "i" = "Hint: Have you imported the gtfs using tidytransit or the combine_gtfs() function?"
  ))


} else {
  gtfs <- gtfs_object

  }
  #GTFS #####


  stops <- gtfs$stops %>%
   dplyr::distinct(stop_id, .keep_all = TRUE)


    # Calculate calendar_sum using gtfs_calendar_full_dates function
    calendar <- gtfs_calendar_full_dates( calendar =  gtfs$calendar, calendar_dates = gtfs$calendar_dates) %>%
      dplyr::group_by(service_id, day_of_week) %>%
      dplyr::summarize(weekly_trip_ct = sum(ct) / max(num_of_weeks), .groups = "keep") %>%
      dplyr::group_by(service_id) %>%
      dplyr::summarize(calendar_sum = sum(weekly_trip_ct))

    stops_sf <- stops %>%
      tidyr::drop_na(stop_id) %>%
      dplyr::filter(!is.na(stop_lat)) %>%
      sf::st_as_sf(. , coords = c("stop_lon", "stop_lat"),
               crs = 4326, agr= "constant") %>%
      sf::st_transform(2926)
    # data from main gtfs in web mercator, reproject to HARN

    # Routes
routes <- clean_service_rte_num(gtfs$routes, netplan_gtfs = netplan_gtfs)
 #join stops to block groups or tracts. This is how you know which stops are in each block group or tract.

  stops_geo <- sf::st_join( stops_sf,acs, join = st_intersects) %>%
    sf::st_drop_geometry()
  #
  # stops_without_bg <-  st_join( stops_sf,acs, join = st_intersects) %>% #stops_geo %>%
  #   filter(is.na(GEOID))

  #stops are getting read in as numeric but there are character types at the bottom of the document. Need to force type.
  gtfs$stop_times$stop_id <- as.character(gtfs$stop_times$stop_id )
  stops_geo$stop_id <- as.character(stops_geo$stop_id)
  stops$stop_id <- as.character(stops$stop_id)
  stops_sf$stop_id <- as.character(stops_sf$stop_id)
  gtfs$trips$trip_id <- as.character( gtfs$trips$trip_id )
  routes$service_rte_num <- as.numeric((routes$service_rte_num))
  #this is the important function. It finds unique trips serving stops associated with the geography you provided.

  if(netplan_gtfs == TRUE){
    trips_by_geo_rte <- stops_geo %>%
      dplyr::select(stop_id, GEOID) %>%
      dplyr::left_join( gtfs$stop_times) %>%
      dplyr::select(stop_id, GEOID, trip_id, arrival_time) %>%
      dplyr::left_join(gtfs$trips) %>%
      tidyr::separate(route_id, into = c("service_rte_num", "schedule"), sep = "-",  extra = "merge") %>%
      dplyr::mutate (route = stringr::str_remove(service_rte_num, "S|C|B|E"))    %>%
      dplyr::mutate(route = as.numeric(service_rte_num)) %>%
      dplyr::left_join(routes, by = "service_rte_num")

    if(include_st == TRUE){
      trips_by_geo_rte <- trips_by_geo_rte %>%
        dplyr::filter(!(service_rte_num %in% c(97, 90,400:601, 629, 632:634, 636:662 , 700:772, 776:900, 932:999))) %>% #for title VI filter out all non metro services
        #this step is fixing the times after midnight issue. I cheated
        # and assigned all trips after midnight to 4 am becuase it doesn't
        #really matter when the trips arrive in the early am block for
        #this analysis 2021.10.05
        dplyr::mutate(arrival_time = stringr::str_replace(arrival_time, "^24:|25:|26:|27:|28:|29:|30:/d" , "04:00:00")) %>%
        dplyr::mutate(arrival_time = hms::as_hms(arrival_time)) %>%
        #filter(arrival_time >= begin_time & arrival_time <= end_time ) %>%
        dplyr::left_join(netplan_calendar) %>%
        dplyr::group_by(GEOID, trip_id, service_rte_num, service_id) %>%
        dplyr::slice(which.min(arrival_time)) %>%
        dplyr::summarize(weekly_trips = sum(calendar_sum, na.rm = TRUE))%>%
        dplyr::ungroup () %>%
        dplyr::group_by(GEOID, service_rte_num) %>%
        dplyr:: summarise(trips_per_rte =  sum(weekly_trips, na.rm = TRUE))
    } else { #allowing for situations with ST routes should be included in analysis. (doesn't filter out 500 series)

      trips_by_geo_rte <- trips_by_geo_rte %>%
        dplyr::filter(!(service_rte_num %in% c(97, 90,400:500, 629, 632:634, 636:662 , 700:772, 776:900, 932:999))) %>% #filter out everything but metro and ST
        #this step is fixing the times after midnight issue. I cheated
        # and assigned all trips after midnight to 4 am becuase it doesn't
        #really matter when the trips arrive in the early am block for
        #this analysis 2021.10.05
        dplyr::mutate(arrival_time = stringr::str_replace(arrival_time, "^24:|25:|26:|27:|28:|29:|30:/d" , "04:00:00")) %>%
        dplyr::mutate(arrival_time = hms::as_hms(arrival_time)) %>%
        #filter(arrival_time >= begin_time & arrival_time <= end_time ) %>%
        dplyr::left_join(netplan_calendar) %>%
        dplyr::group_by(GEOID, trip_id, service_rte_num, service_id) %>%
        dplyr::slice(which.min(arrival_time)) %>%
        dplyr::summarize(weekly_trips = sum(calendar_sum, na.rm = TRUE))%>%
        dplyr::ungroup () %>%
        dplyr::group_by(GEOID, service_rte_num) %>%
        dplyr::summarise(trips_per_rte =  sum(weekly_trips, na.rm = TRUE))
    }

  } else { #handling the difference between netplan GTFS which uses route_id for storage of route num and production GTFS which uses route_short_name
    trips_by_geo_rte <- stops_geo %>%
      dplyr::select(stop_id, GEOID) %>%
      dplyr::left_join( gtfs$stop_times) %>%
      dplyr::select(stop_id, GEOID, trip_id, arrival_time) %>%
      dplyr::left_join(gtfs$trips) %>%
      dplyr::left_join(routes, by = "route_id")
    if(include_st == TRUE){
      trips_by_geo_rte <- trips_by_geo_rte %>%
        dplyr::filter(!(service_rte_num %in% c(97, 90,400:601, 629, 632:634, 636:662 , 700:772, 776:900, 932:999))) %>% #for title VI filter out all non metro services
        #this step is fixing the times after midnight issue. I cheated
        # and assigned all trips after midnight to 4 am becuase it doesn't
        #really matter when the trips arrive in the early am block for
        #this analysis 2021.10.05
        dplyr::mutate(arrival_time = stringr::str_replace(arrival_time, "^24:|25:|26:|27:|28:|29:|30:/d" , "04:00:00")) %>%
        dplyr::mutate(arrival_time = hms::as_hms(arrival_time)) %>%
        #filter(arrival_time >= begin_time & arrival_time <= end_time ) %>%
        dplyr::left_join(calendar) %>%
        dplyr::group_by(GEOID, trip_id, service_rte_num, service_id) %>%
        dplyr::slice(which.min(arrival_time)) %>%
        dplyr::summarize(weekly_trips = sum(calendar_sum, na.rm = TRUE))%>%
        dplyr::ungroup () %>%
        dplyr::group_by(GEOID, service_rte_num) %>%
        dplyr::summarise(trips_per_rte =  sum(weekly_trips, na.rm = TRUE))
    } else { #allowing for situations with ST routes should be included in analysis. (doesn't filter out 500 series)
      trips_by_geo_rte <- trips_by_geo_rte %>%
        dplyr::filter(!(service_rte_num %in% c(97, 90,400:500, 629, 632:634, 636:662 , 700:772, 776:900, 932:999))) %>% #for title VI filter out everything but Metro and ST
        #this step is fixing the times after midnight issue. I cheated
        # and assigned all trips after midnight to 4 am becuase it doesn't
        #really matter when the trips arrive in the early am block for
        #this analysis 2021.10.05
        dplyr::mutate(arrival_time = stringr::str_replace(arrival_time, "^24:|25:|26:|27:|28:|29:|30:/d" , "04:00:00")) %>%
        dplyr::mutate(arrival_time = hms::as_hms(arrival_time)) %>%
        #filter(arrival_time >= begin_time & arrival_time <= end_time ) %>%
        dplyr::left_join(calendar) %>%
        dplyr::group_by(GEOID, trip_id, service_rte_num, service_id) %>%
        dplyr::slice(which.min(arrival_time)) %>%
        dplyr::summarize(weekly_trips = sum(calendar_sum, na.rm = TRUE))%>%
        dplyr::ungroup () %>%
        dplyr::group_by(GEOID, service_rte_num) %>%
        dplyr::summarise(trips_per_rte =  sum(weekly_trips, na.rm = TRUE))
    }


  }

  routes_in_geo <- trips_by_geo_rte %>%
    dplyr::ungroup() %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize(routes_in_geo = toString(service_rte_num) )

  trips_by_geo <- trips_by_geo_rte %>%
    dplyr::group_by(GEOID) %>%
    dplyr::summarize(trips_per_geo = sum(trips_per_rte, na.rm = TRUE)) %>%
    dplyr::left_join(routes_in_geo)



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

    saveRDS(trips_by_geo_rte, na= "", file = paste0(output_folder, "/",
                                                             project_name,"_", lubridate::today() , "_", analysis_period, "_",
                                                             geography ,"_" ,gtfs_type, "_by_route_trips.RDS"))

    saveRDS(trips_by_geo, na= "",file = paste0(output_folder, "/", project_name,"_", lubridate::today() ,
                                                        "_", analysis_period, "_", geography ,"_" ,gtfs_type, "_trips.RDS"))

    cli::cli_inform("RDS export at {output_folder}")
  }else {
    cli::cli_inform("No RDS saved")
  }

 output <- list(trips_by_geo = trips_by_geo,
                trips_by_geo_rte = trips_by_geo_rte)

 output


}

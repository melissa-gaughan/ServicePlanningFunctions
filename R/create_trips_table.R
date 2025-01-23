#' Create summary table of trips and headways
#'
#' @param day_type Character. Day of Week. Options are 'wkd', 'sat', 'sun'
#' @param network Character. GTFS. Options are 'baseline_gtfs', 'proposed_gtfs'
#' @param by_direction T/F. Do you want a breakdown by inbound/outbound direction?
#' @param by_period T/F. Do you want a breakdown by period?
#' @param routes Character. Filter by Routes
#'
#'
#' @return Data frame of summary metrics of GTFS input (Trip Count, Span, Headway)
#' @export
#'
#' @examples
#' baseline_gtfs <- tidytransit::read_gtfs(path =
#' fs::path_package( "extdata", "gtfs", "241_gtfs.zip", package = "ServicePlanningFunctions"))
#'
#' spring_24_trip_table <- create_trips_table(day_type = 'wkd', network = 'baseline_gtfs', by_direction = FALSE, by_period = TRUE)

create_trips_table <- function(day_type, network, by_direction = TRUE, by_period = TRUE, routes = NULL){
  if(network == "baseline_gtfs"){
    kcm <- baseline_gtfs
  } else if(network == "proposed_gtfs"){
    kcm <- proposed_gtfs
  }else{
    print("check network")
  }

  # Update routes argument if no route selection is provided or provided routes are not found
  if(nrow(filter(kcm$routes, route_id %in% routes)) == 0) {
    routes <- kcm$routes$route_id
    cli::cli_alert_info("No valid routes provided. Generating table for all routes.")
  }

  kcm$routes <- kcm$routes %>%
    filter(route_id %in% routes) %>% # filter for select routes
    mutate(route_short_name = ifelse(is.na(route_short_name), route_long_name,
                                     paste0(route_short_name,
                                            ' ',
                                            route_long_name))) %>%
    mutate(route_short_name = trimws(route_short_name))



  # Identify service ID by type of day (weekday, sat or sun)
  service_days <- kcm$calendar %>%
    # Make sure service IDs are actually in use in the trip tables
    filter(service_id %in% unique(kcm$trips$service_id)) %>%
    # Define daytype of service ID to filter in next steps
    mutate(daytype = ifelse(saturday == 1, 'sat',
                            ifelse(sunday == 1, 'sun', 'wkd')))
  if (day_type == "wkd"){
    # Filter weekday only service IDs
    serv_wkd <- service_days %>%
      filter(daytype == 'wkd')
  } else if (day_type == "sat"){
    # Filter weekday only service IDs
    serv_wkd <- service_days %>%
      filter(daytype == 'sat')
  } else if (day_type == "sun"){
    serv_wkd <- service_days %>%
      filter(daytype == 'sun')
  }else{
    print("Day type incorrect. Options are 'wkd', 'sat', sun'. Case sensistive.")
  }

  # If by_direction argument is set to TRUE, output is broken down by direction (include direction_id in group_by)
  # If by_direction argument is set to FALSE, output is not broken down by direction (exclude direction_id in group_by)
  if (by_direction) {
    direction_toggle <- 'direction_id'
  } else {
    direction_toggle <- NULL
  }

  # If by_period argument is set to TRUE, output is broken down by period (include period in group_by)
  # If by_period argument is set to FALSE, output is not broken down by period (exclude period in group_by)
  if (by_period) {
    period_toggle <- 'period'
  } else {
    period_toggle <- NULL
  }

  # Weekdays
  # Filter only trips in the routes in the corridors selected
  trips_wkd <- kcm$trips %>%
    filter(route_id %in% routes) %>% # filter for select routes
    # Filter only weekday trips
    filter(service_id %in% serv_wkd$service_id) %>%
    # Keep only relevant variables
    select(route_id, service_id, trip_id, direction_id)  #trip_headsign,


  # Create a dataframe with the stops and times of desired trips
  if(is.numeric(kcm$stop_times$arrival_time) == TRUE){
    print("numeric arrival times")
    stop_times <- kcm$stop_times %>%
      # Filter stop-trips of the relevant routes and only weekday
      filter(trip_id %in% trips_wkd$trip_id) %>%
      select(trip_id, arrival_time, departure_time, stop_id, stop_sequence)

    #split the character string into hours minutes seconds and append to dataframe
    stop_times[,6:8] <- stringr::str_split_fixed(stop_times$arrival_time, pattern= ":", 3)



    stop_times <- stop_times %>%
      mutate(seconds_after_midnight = (as.numeric(V1)*3600) + as.numeric(V2)*60 + as.numeric(V3)) %>%
      # Sort the dataframe
      arrange(trip_id, seconds_after_midnight, stop_sequence) %>%
      # Group by unique trip (Recall this dataframe is already sorted by time)
      group_by(trip_id) %>%
      # Create two new columns for each trip (group)
      # One with the start time (string type) of every trip
      # The other with the end time (string type) of every trip
      mutate(start_time_str = as.numeric(min(seconds_after_midnight)),
             end_time_str = as.numeric(max(seconds_after_midnight))) %>%
      mutate(start_time_str = hms(seconds = start_time_str),
             end_time_str = hms(seconds = end_time_str))

  }else if (is.character(kcm$stop_times$arrival_time) == TRUE){
    print("character arrival times")
    # split into hours, minutes, seconds, convert into seconds, then find max by trip ID
    stop_times <- kcm$stop_times %>%
      # Filter stop-trips of the relevant routes and only weekday
      filter(trip_id %in% trips_wkd$trip_id) %>%
      select(trip_id, arrival_time, departure_time, stop_id, stop_sequence)

    stop_times[,6:8] <- stringr::str_split_fixed(stop_times$arrival_time, pattern= ":", 3)



    stop_times <- stop_times %>%
      mutate(seconds_after_midnight = (as.numeric(V1)*3600) + as.numeric(V2)*60 + as.numeric(V3)) %>%
      # Sort the dataframe
      arrange(trip_id, seconds_after_midnight, stop_sequence) %>%
      # Group by unique trip (Recall this dataframe is already sorted by time)
      group_by(trip_id) %>%
      # Create two new columns for each trip (group)
      # One with the start time (string type) of every trip
      # The other with the end time (string type) of every trip
      mutate(start_time_str = as.numeric(min(seconds_after_midnight)),
             end_time_str = as.numeric(max(seconds_after_midnight))) %>%
      mutate(start_time_str = hms::hms(seconds = start_time_str),
             end_time_str = hms::hms(seconds = end_time_str))
  }else{
    print("check data structure of stop_times$arrival_times.")
    break
  }

  #headway range #####
  #calculate shortest and longest time between trips in a period.
  headway_range_trips <- left_join(trips_wkd, stop_times) %>%

    filter(!(is.na(start_time_str) | is.na(end_time_str) )) %>%
    # Remove potential duplicates to avoid errors in the calculations
    distinct(., route_id, trip_id, direction_id, start_time_str, .keep_all = TRUE)# %>%

  # Set of Clockface Headways to round headways to. Can add/remove values to designate granularity
  clockface_headway_set <- c(1, 2, 3, 4, 5, 6, 10, 12, 15, 20, 30, 45, 60)

  headway_range <- headway_range_trips %>%

    # Extract only the hour integer from the character time and set it as numeric
    #2023.01.20 Changing this to use the str_split hour. Times over 24 are getting coded as zero.
    mutate(hour =  as.numeric(V1) )%>%    #lubridate::hour(start_time_str)) %>%
    # Calculate service hours for every trip
    # A conditional statement is used for those trips that start before midnight and end after it
    mutate(service_hr =  ifelse(start_time_str <  end_time_str,
                                difftime(end_time_str, start_time_str, unit = 'hours'),
                                difftime(end_time_str, start_time_str, unit = 'hours') + 24) ,
           # Define period of day based on the second after midnight the trip starts
           #adding in an early AM period so that the XNT calcs look less insane
           period = case_when(between(seconds_after_midnight, 0, 17999)  ~ "0.AAM" ,

                              #shifting the AM start period to be 4:30 AM so that trips scheduled right before 5 AM aren't messing
                              #with overnight service calcs.
                              between(seconds_after_midnight, 18000, 32399 ) ~ "1.AM",
                              between(seconds_after_midnight, 32400, 53999) ~ "2.MID",
                              between(seconds_after_midnight, 54000,  68399 ) ~ "3.PM",
                              between(seconds_after_midnight, 68400, 79199 ) ~ "4.XEV",
                              between(seconds_after_midnight, 79200, 86399 ) ~ "5.XNT",
                              #XNT bumped to top
                              between(seconds_after_midnight, 86400, 108000 )~ "6.OWL",
                              .default = ""))  %>%


    # Sort dataframe, the relevant variable is start time of the trip
    # Note that we use the string start time, to ensure that a trip starting at 25:00:00
    # is sorted at the end of the day an not earlier as a 01:00:00 trip
    #not grouping by period here to make it easier to calculate the headways at/near period boundaries
    group_by(across(all_of(c('route_id', 'direction_id', period_toggle)))) %>% # keep direction_id group by so that headways are measured within directional trips
    arrange(route_id, direction_id, start_time_str) %>%

    # For every corridor calculate the start time of the first trip and the
    # starting time of the last trip
    mutate(start_next_trip = lead(start_time_str)) %>%
    mutate(time_to_next_trip = start_next_trip- start_time_str) %>%
    # round headway to nearest clockface headway value from the clockface_headway_set by
    # calculating the value with the minimum absolute difference between headway value and values from clockface_headway_set
    mutate(clockface_headway = as.numeric(sapply(time_to_next_trip / 60, function(x) clockface_headway_set[which.min(abs(clockface_headway_set - x))]))) %>%
    #filtering out trips that have  multiple trips scheduled due to short turn variants that are less than 4 minutes apart.
    filter(time_to_next_trip > 240 ) %>%

    group_by(across(all_of(c('route_id', direction_toggle, period_toggle)))) %>% # group by direction_id and period can be toggled
    summarise(max_headway = max(time_to_next_trip),
              min_headway = min(time_to_next_trip),
              #adding mean headway based on time to next trip calc 2022.12.21
              mean_headway = mean(time_to_next_trip),
              max_clockface_headway = max(clockface_headway),
              min_clockface_headway = min(clockface_headway)) %>%
    #convert to minutes

    mutate(max_headway =as.numeric(max_headway)/60,
           min_headway =as.numeric(min_headway)/60,
           mean_headway = round(as.numeric((mean_headway)/60),2)) %>%

    # summarise(max_headway = max(max_headway),
    #           min_headway = min(min_headway)) %>%
    mutate(headway_range = paste(min_headway, max_headway, sep= "-"),
           clockface_headway_range = ifelse(max_clockface_headway == min_clockface_headway, as.character(max_clockface_headway), paste(min_clockface_headway, max_clockface_headway, sep = "-")))

  #average headways#####

  trips_wkd <- left_join(trips_wkd, stop_times) %>%
    select(-service_id) %>%
    filter(!(is.na(start_time_str) | is.na(end_time_str) )) %>%
    # Remove potential duplicates to avoid errors in the calculations
    distinct(., route_id, trip_id, direction_id, start_time_str, .keep_all = TRUE)

  trips_wkd <- trips_wkd %>%
    # Extract only the hour integer from the character time and set it as numeric
    #2023.01.20 Changing this to use the str_split hour. Times over 24 are getting coded as zero.
    mutate(hour =  as.numeric(V1) )%>%   #lubridate::hour(start_time_str)) %>%
    # Calculate service hours for every trip
    # A conditional statement is used for those trips that start before midnight and end after it
    mutate(service_hr = ifelse(start_time_str <  end_time_str,
                               difftime(end_time_str, start_time_str, unit = 'hours'),
                               difftime(end_time_str, start_time_str, unit = 'hours') + 24),
           # Define period of day based on the integer hour the trip starts

           # Define period of day based on the second after midnight the trip starts
           #adding in an early AM period so that the XNT calcs look less insane
           period = case_when(between(seconds_after_midnight, 0, 17999)  ~ "0.AAM" ,

                              #shifting the AM start period to be 4:40 AM so that trips scheduled right before 5 AM aren't messing
                              #with overnight service calcs.
                              between(seconds_after_midnight, 18000, 32399 ) ~ "1.AM",
                              between(seconds_after_midnight, 32400, 53999) ~ "2.MID",
                              between(seconds_after_midnight, 54000,  68399 ) ~ "3.PM",
                              between(seconds_after_midnight, 68400, 79199 ) ~ "4.XEV",
                              between(seconds_after_midnight, 79200, 86399 ) ~ "5.XNT",
                              #XNT bumped to top
                              between(seconds_after_midnight, 86400, 108000 )~ "6.OWL",
                              .default = ""))  %>%
    # Sort dataframe, the relevant variable is start time of the trip
    # Note that we use the string start time, to ensure that a trip starting at 25:00:00
    # is sorted at the end of the day an not earlier as a 01:00:00 trip
    arrange(route_id, direction_id, start_time_str) %>%
    group_by(across(all_of(c('route_id', direction_toggle, period_toggle)))) %>% # group by direction_id and period can be toggled
    # For every corridor calculate the start time of the first trip and the
    # starting time of the last trip
    mutate(start_first_trip_period = first(hour),
           start_last_trip_period = last(hour),
           #this is calculating the total hours in the period served by the route. I added a one hour period
           #to solve the issue of both trips in a period starting in the same hour

           period_length = start_last_trip_period - start_first_trip_period+1) %>%
    ungroup() %>%
    group_by(across(all_of(c('route_id', direction_toggle)))) %>%  # group by direction_id can be toggled

    # Calculate the start of first trip and end of last trip in numeric hours
    # (this has the hours as integer and the minutes and seconds in decimals)

    mutate(first = as.difftime(first(start_time_str), format = '%H:%M:%S', units = 'hours'),
           last = as.difftime(last(start_time_str), format = '%H:%M:%S', units = 'hours')) %>%
    # Compute the span of service for all day for every corridor

    mutate(span_gtfs_hrs = ifelse(first <  last,
                                  last - first,
                                  last - first + (24*60*60))) %>%
    mutate(span_gtfs_hrs = hms::hms(seconds = span_gtfs_hrs))

  # Calculate how many hours before 5am a trip starts (Upper)
  # Calulate how many hours after 7pm (19:00) a trip starts (Lower)

  #first and last trips on a route #####################################################

  # Aggregate dataframe by corridor and period


  first_last_trips <- trips_wkd %>%
    group_by(across(all_of(c('route_id', direction_toggle)))) %>% # group by direction_id can be toggled
    arrange(start_time_str, by_group = TRUE) %>%
    mutate(first_trip =  first(start_time_str),
           #trying to find the latest instance of a bus serving a stop
           #20222.01.19 Changing this to first stop of last trip based on feedback from LLink team
           last_trip = last(start_time_str)) %>%
    select(all_of(c('route_id', direction_toggle, 'first_trip', 'last_trip'))) %>% # include direction_id can be toggled
    distinct()



  route_wkd <- trips_wkd %>%
    group_by(across(all_of(c('route_id', period_toggle, 'period_length', 'span_gtfs_hrs', direction_toggle))  #trip_headsign, direction_id,
    )) %>% # group by direction_id can be toggled
    # Calculate trips in period and service hours in period and whether a route is bidirectional or not (this matters when calculating avg_headway_mins)
    summarise(num_trips = n(),
              num_direction = length(unique(direction_id)),
              .groups = 'drop')

  # all day service metrics ####
  # Populated if by_period is set to TRUE. Returns empty data frame if by_period is set to FALSE.
  # Don't need all_day_service calculations when function is already summarizing for all_day when by_period is set to FALSE.
  all_day_service <- route_wkd %>%
    # If period column exists, applies filter below to the period column.
    # If period column is missing, applies filter below to the period_length column, which returns an empty data frame.
    # The matches function will pick the first column that contains period and applies the filter to that column.
    # Replacing matches with any_of returns full data frame instead of an empty data frame.
    filter(if_any(matches('period'), ~.x %in% c("1.AM" , "2.MID"  , "3.PM" , "4.XEV"))) %>%
    group_by(across(all_of(c('route_id', direction_toggle, 'num_direction')))) %>% # group by direction_id can be toggled
    summarise(num_trips = sum(num_trips, na.rm = T)) %>%
    mutate(avg_headway_mins = round(16*60/num_trips*num_direction, 0)) %>%
    mutate(period = "7.main") %>%
    ungroup() %>%
    group_by(across(all_of(c('route_id', direction_toggle, period_toggle)))) %>% # group by direction_id can be toggled
    # 2022.03.29 added this line of code to calculate route-level avg headway by direction
    summarize(avg_headway_mins = mean(avg_headway_mins, na.rm = T),
              num_trips = sum(num_trips, na.rm = T))
  #divided by two because original dataset is bidirectional, don't want to double count span




  # Calculate service hour per trip and adjust minutes of operation in period based on the
  # adjustment values calculated earlier
  # Calcualte average headways based on GTFS processed data
  #period summary ####
  period_summary <- route_wkd %>%
    mutate(avg_headway_mins = round(period_length*60/num_trips*num_direction, 0)) %>%
    ungroup() %>%
    group_by(across(all_of(c('route_id', direction_toggle, period_toggle, 'span_gtfs_hrs')))) %>% # group by direction_id can be toggled
    # 2022.03.29 added this line of code to calculate route-level avg headway by direction
    summarize(avg_headway_mins = mean(avg_headway_mins, na.rm = T),
              num_trips = sum(num_trips, na.rm = T)) %>%
    bind_rows(all_day_service)

  #output df ####
  weekday_trips <- kcm$routes %>%

    select(route_id, route_short_name ) %>%

    left_join(period_summary) %>%
    left_join(first_last_trips) %>%
    left_join(headway_range) %>%
    mutate(day_type = day_type ,
           network = network)
}






#' Combine GTFS Files From NetPlan
#'
#' @param gtfs_filepath Character string in quotes. The location of the folders containing the GTFS files. This should be the parent directory, not the sub-directory.
#' @param designated_start_date Numeric. YYYYMMDD defines start_date in calendar table, as NetPlan does not export it.
#' @param designated_end_date Numeric. YYYYMMDD defines end_date in calendar table, as NetPlan does not export it.
#' @param save_csv T/F Do you want to save csv files of the combined GTFS tables?
#' @param save_RDS T/F Do you want to save an RDS object of the combined GTFS list object?
#' @param output_folder File path of the place to export file exports
#'
#' @return a list object with combined GTFS information. Optionally, exports to csv and RDS.
#'
#' @export
#'
#' @examples
combine_gtfs <- function(gtfs_filepath, designated_start_date, designated_end_date, save_csv = FALSE, save_RDS = FALSE, output_folder = NULL){
  if(!dir.exists(gtfs_filepath)){
    print("Folder does not exist. Make sure the folder path is correct and points to the parent directory of the GTFS you are combining.")
  } else {

gtfs_folders <- list.files(gtfs_filepath)

if(length(stringr::str_subset(gtfs_folders, pattern="txt")) != 0) {

  stop("Filepath should be for the location of the folders to be combined, not a sub-folder.")
} else {

gtfs_list <- list()

for (i in 1: length (gtfs_folders)){
  weekday_gtfs <- list.files(path = here::here(gtfs_filepath, gtfs_folders[i] ), pattern="*.txt")
  weekday_gtfs <- weekday_gtfs[!weekday_gtfs %in% c("stops.txt", "stop_times.txt", "error_warning.txt", "routes.txt")]

  weekday <- lapply(weekday_gtfs, function(x) {
    out <- readr::read_csv(here::here(gtfs_filepath, gtfs_folders[i] ,x), col_names = T, show_col_types = F, progress = F)

    return(out)
  })

  weekday[[6]] <- readr::read_csv( here::here(gtfs_filepath, gtfs_folders[i], "routes.txt"), col_types = list("c", "c", "c", "c", "c", "c", "c", "c",
                                                                                                                  "c", "c", "c", "c", "c", "c", "c", "c"),
                            col_names = T,  show_col_types = F, progress = F)

  weekday[[7]] <- readr::read_csv( here::here(gtfs_filepath, gtfs_folders[i],"stops.txt") , col_types = list("c","c","c","c","n","n","n","n","n","c","c","c"),
                            col_names = T,  show_col_types = F, progress = F)


  weekday[[8]] <- readr::read_csv(here::here(gtfs_filepath, gtfs_folders[i],"stop_times.txt"),  col_types = list("c","c", "c", "c", "n", "n", "n", "n", "n"),
                           col_names = T,  show_col_types = F, progress = F)
  names(weekday) <- c("agency", "calendar", "calendar_dates",  "shapes",
                      "trips","routes", "stops", "stop_times")
  gtfs_list[[i]] <- weekday

}

baseline_gtfs <- flatten(gtfs_list)

}

  }



  agency <-baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "agency")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  calendar <- baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "calendar")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr:: mutate(start_date = designated_start_date,  #add calendar dates
           end_date = designated_end_date)

  calendar_dates <- baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "calendar_dates")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  routes <- baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "routes")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()%>%
    tidyr::separate(route_id, into = c("route_id", "schedule"), sep = "-",  extra = "merge") %>%
    dplyr::select(-schedule) %>%
    dplyr::distinct(route_id, .keep_all = T) %>%
    dplyr::mutate(agency_id = "King County  Metro Transit")

  trips <- baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "trips")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    tidyr::separate(route_id, into = c("route_id", "schedule"), sep = "-",  extra = "merge") %>%
    dplyr::select(-schedule)

  stops <- baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "stops")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct() %>%
    dplyr::distinct(stop_id, .keep_all = T) %>%
    dplyr::filter(!(is.na(stop_lat)))

  stop_times <- baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "stop_times")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()

  shapes <-baseline_gtfs[stringr::str_detect(names(baseline_gtfs), "shapes")] %>%
    dplyr::bind_rows() %>%
    dplyr::distinct()


  combined_gtfs <- list()

  combined_gtfs[[1]] <- agency
  combined_gtfs[[2]] <- calendar
  combined_gtfs[[3]] <- calendar_dates
  combined_gtfs[[4]] <- shapes
  combined_gtfs[[5]] <- trips
  combined_gtfs[[6]] <- routes
  combined_gtfs[[7]] <- stops
  combined_gtfs[[8]] <- stop_times

  names(combined_gtfs) <-  c("agency", "calendar", "calendar_dates",  "shapes",
                                                "trips","routes", "stops", "stop_times")

  if(save_csv == TRUE){

    readr::write_csv(agency, na= "", file = paste0(output_folder, "/", "agency.txt"))
    readr::write_csv(calendar, na= "", file = paste0(output_folder, "/","calendar.txt"))
    readr::write_csv(calendar_dates, na= "", file = paste0(output_folder, "/","calendar_dates.txt"))
    readr::write_csv(routes, na= "", file =paste0(output_folder, "/", "routes.txt"))
    readr::write_csv(trips, na= "", file = paste0(output_folder, "/", "trips.txt"))
    readr::write_csv(stops, na= "", file = paste0(output_folder, "/", "stops.txt"))
    readr::write_csv(stop_times, na= "", file = paste0(output_folder, "/", "stop_times.txt"))
    readr::write_csv(shapes, na= "", file = paste0(output_folder, "/", "shapes.txt"))
    print(paste("CSV exports at", output_folder))
  } else {
    print("No CSVs saved")
  }

  if(save_RDS == TRUE){

    saveRDS(combined_gtfs,  file = paste0(output_folder, "/", "GTFS.RDS"))
    print(paste("RDS export at", output_folder))
  }else {
    print("No RDS saved")
  }

  combined_gtfs

}



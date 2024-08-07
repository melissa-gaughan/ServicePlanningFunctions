#' Determine Service Dates for GTFS
#'
#'
#' @param calendar Dataframe. Should be the calendar.txt file from the GTFS you are working with
#' @param calendar_dates Dataframe. Should be the calendar_dates.txt file from the GTFS you are working with
#' @param netplan_gtfs T/F Is the GTFS you are using exported from NetPlan?
#' @param designated_start_date Numeric. YYYYMMDD defines start_date in calendar table, as NetPlan does not export it. Required if not already defined in calendar.txt.
#' @param designated_end_date Numeric. YYYYMMDD defines end_date in calendar table, as NetPlan does not export it. Required if not already defined in calendar.txt.
#'
#' @return Dataframe of dates that the service_id will be in service.
#' @export
#'
#' @examples
gtfs_calendar_full_dates <- function(calendar, calendar_dates = NULL, netplan_gtfs = FALSE, designated_start_date = NULL, designated_end_date = NULL) {

server <- "kcitazrsqlprp01.database.windows.net"
database = "tbird_dw"

can_connect_tbird <- DBI::dbCanConnect(odbc::odbc(),
             Driver="ODBC Driver 17 for SQL Server",
             Server = server, Database = database,
             Authentication = "ActiveDirectoryIntegrated")
if( can_connect_tbird == FALSE){

cli::cli_abort(c(
  "TBIRD Connection Error:",
  "x" = "You need to connect to the VPN to run this function. Make sure you can connect to T-BIRD."
))
} else if (exists(con) = TRUE) {
  con <- con
} else {
con <- DBI::dbConnect(odbc::odbc(),
                      Driver="ODBC Driver 17 for SQL Server",
                      Server = server, Database = database,
                      Authentication = "ActiveDirectoryIntegrated")
}

  cal_df <- calendar

  # Check and validate arguments
  if (netplan_gtfs == FALSE) {
    if (missing(calendar_dates)) {
      cli::cli_abort(c("X" = "calendar_dates.txt GTFS file must be defined in the calendar_dates argument."))
    } else if (anyNA(cal_df$start_date) | anyNA(cal_df$end_date)) {
      cli::cli_abort(c("X" = 'Calendar GTFS is missing start dates or end dates. Set netplan_gtfs to "TRUE" if GTFS file is from Netplan.'))
    } else {
      cal_dates_df <- calendar_dates
      designated_start_date <- min(as.Date(cal_df$start_date))
      designated_end_date <- max(as.Date(cal_df$end_date))
      message(paste0("Generating dates from ", designated_start_date, " to ", designated_end_date))
    }
  } else if(netplan_gtfs == TRUE) {
     if (!anyNA(cal_df$start_date)) { #combine gtfs function has a way to add cal dates to NetPlan GTFS now. Changing this to use existing to allow that to work.
       cal_dates_df <- calendar_dates
       designated_start_date <- min(as.Date(cal_df$start_date))
       designated_end_date <- max(as.Date(cal_df$end_date))
       message(paste0("Generating dates from ", designated_start_date, " to ", designated_end_date))
  } else if (missing(start_date) || missing(end_date)) {
      cli::cli_abort(c("X" = "Start Date and End Date must be defined when using GTFS files generated from Netplan (YYYY-MM-DD)."))
    } else if (str_detect(start_date,  "^\\d{4}-\\d{2}-\\d{2}$") && str_detect(end_date,  "^\\d{4}-\\d{2}-\\d{2}$")) {
      cli::cli_alert_info("Generating dates from {designated_start_date} to {designated_end_date}.")
      cal_df$start_date <- lubridate::ymd(designated_start_date)
      cal_df$end_date <- lubridate::ymd(designated_end_date)
    } else {
      cli::cli_abort(c("X" = "Start Date and End Date must be set as YYYYMMDD."))
    }
  }

  # Pull all dates from EDW.DIM_DATE from T-Bird between start date and latest end date
  dim_date <- dbGetQuery(con, paste0("DECLARE @startdate as date = '", designated_start_date,
                                     "', @enddate as date = '", designated_end_date, "';
                                     select full_date, lower(day_of_week_long_name) as name from edw.dim_date
                                     where full_date between @startdate and @enddate
                                     ")) %>%
    janitor::clean_names()

  # Join EDW.DIM_DATE to Calendar GTFS File where full_date falls between start date and end date of Service IDs
  cal_full_dates_df <- cal_df %>%
    pivot_longer(cols = monday:sunday) %>%
    left_join(dim_date, join_by(start_date <= full_date, end_date >= full_date, name == name))

  # Prepare calendar_dates.txt dataset
  # If GTFS is from Netplan, create empty dataset
  if (netplan_gtfs == FALSE) {
    cal_dates_df <- cal_dates_df %>%
      left_join(dim_date, by = c("date" = "full_date"))
  } else {
    cal_dates_df <- data.frame(service_id = NA, date = NA, exception_type = NA, name = NA)
  }

  # Calculate number of weeks for each weekday
  cal_week_ct <- cal_full_dates_df %>%
    group_by(service_id, name) %>%
    summarize(num_of_weeks = sum(value), .groups = "keep") %>%
    group_by(name) %>%
    summarize(num_of_weeks = max(num_of_weeks))

  # Join Calendar_Dates GTFS file to Calendar GTFS File and calculate trip count
  cal_comb <- cal_full_dates_df %>%
    left_join(cal_dates_df, by = c("service_id", "full_date" = "date", "name")) %>%
    left_join(cal_week_ct, by = "name") %>%
    mutate(day_of_week = factor(name, levels = c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")),
           ct = coalesce(ifelse(exception_type == 2, 0, exception_type), value)) %>%
    select(!c(name)) %>%
    ungroup()

  cal_comb
}

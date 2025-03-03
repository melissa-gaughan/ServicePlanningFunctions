% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_trips_by_geography.R
\name{count_trips_by_geography}
\alias{count_trips_by_geography}
\title{Count Transit Trips by Geography}
\usage{
count_trips_by_geography(
  gtfs_object,
  begin_time,
  end_time,
  analysis_period,
  day_type,
  project_name,
  netplan_gtfs = F,
  geography,
  save_csv = F,
  save_RDS = F,
  gtfs_type,
  output_folder,
  run_id = 1
)
}
\arguments{
\item{gtfs_object}{A GTFS list-object available in your working environment}

\item{begin_time}{Character. HH:MM:SS. When does the analysis period start?}

\item{end_time}{Character. HH:MM:SS. When does the analysis period end?}

\item{analysis_period}{Character. Name of analysis period. Example: AM, PM}

\item{day_type}{Character. Day of Week. Options are "weekday", "saturday", "sunday", "week", "weekend"}

\item{project_name}{Character. Name of project. Used for naming exports}

\item{netplan_gtfs}{T/F. Is this GTFS from NetPlan?}

\item{geography}{Character. What geography do you want to use to summarise results? Options are "block_group", "tract", "quarter_mile_hex", "eigth_mile_hex"}

\item{save_csv}{T/F. Do you want to export csvs?}

\item{save_RDS}{T/F. Do you want to export RDS?}

\item{gtfs_type}{Character. Does the GTFS reflect the baseline or proposed network? Options are "baseline" or "proposed".}

\item{output_folder}{Character. Where do you want the outputs saved?}

\item{run_id}{Numeric. Useful for using this function with pmap.}
}
\value{
List of trips by route/geography and by geography (summed)
}
\description{
Count Transit Trips by Geography
}

#' Get Title VI Classification
#'
#' @param acs_year Numeric. Year of data to return for American Community Survey
#' @param geography A sf polygon object to use as analysis geography. If null, defaults to 2020 Census Tracts
#' @param state Character. Two character state code for ACS data. Should match geography object. Defaults to "WA"
#' @param county Character. Full name of county for ACS data. Should match geography object. Defaults to "King"
#'
#' @return sf object with fields indicating whether or not there is a significant bipoc or low income population in the tract.
#' @export
#'
#' @examples
get_title_vi_classification <- function( acs_year, geography=NULL, state = "WA", county = "King") {

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

  } else {
    con <- DBI::dbConnect(odbc::odbc(),
                          Driver="ODBC Driver 17 for SQL Server",
                          Server = server, Database = database,
                          Authentication = "ActiveDirectoryIntegrated")
    cli::cli_inform(c(
      "TBIRD Connection Successful"

    ))
  }


  tract_classification <- DBI::dbGetQuery(con,
      paste0("SELECT [ACS_YR]
            ,[GEOID]
            ,[TRACT_NM]
            ,[PCT_BIPOC]
            ,[BIPOC_THRESHOLD]
            ,[BIPOC_TRACT_FLAG]
            ,[PCT_LOW_INCOME]
            ,[LOW_INCOME_THRESHOLD]
            ,[LOW_INCOME_TRACT_FLAG]
            FROM [DP].[CENSUS_TRACT_CLASSIFICATION]
            WHERE ACS_YR =",  acs_year)) %>%
    dplyr::mutate(GEOID = as.character(GEOID))%>%
    janitor::clean_names()



  if(is.null(geography)){
    cli::cli_inform(c(
      "Default Geography",
      "i" = "Using ACS 2020 Tracts with 50 ft edge buffer and water removed for analysis geography."))

    kc_tracts <- sf::read_sf(fs::path_package( "extdata", "2020_Census_Tracts_for_King_County___tracts20_area.shp",
                                               package = "ServicePlanningFunctions"))
    kc_tracts_no_water <- remove_water(polygon = kc_tracts, state_code = "WA",  county_code = "King", crs = 2926) %>%
      sf::st_buffer(50)

    acs <- kc_tracts_no_water %>%
      dplyr::rename(GEOID = GEO_ID_TRT)

  } else if(!(("MULTIPOLYGON" %in% class(geography))| "POLYGON" %in% class(geography))){
    cli::cli_abort(c(
      "Object Type Error",
      "x" = "The geography parameter requires a polygon or multipolygon object. It is currently referencing a {class(geography)} object."
    ))
  } else {
    acs <- geography
  }
  cli::cli_inform(c(
    "before census table" ))

  census_table <- tidycensus::get_acs(geography = "tract",
                          variables = c(total_pop = "B01001_001"),
                          state= "WA",
                          county = "King",
                          year= acs_year,
                          geometry = F,
                          output = "wide") %>%
    janitor::clean_names() %>%
    dplyr::select(geoid, total_pop_e) %>%
    dplyr::rename(total_pop = total_pop_e)
  cli::cli_inform(c(
    "after census table" ))

  tract_info  <-  acs %>%
    janitor::clean_names() %>%
    dplyr::left_join( tract_classification, by = "geoid") %>%
    dplyr:: left_join(census_table) %>%
    dplyr::mutate(low_income_tract_flag = dplyr::case_when(is.na(low_income_tract_flag) ~ 0, TRUE ~ low_income_tract_flag))%>%
    dplyr::mutate(bipoc_tract_flag = dplyr::case_when(is.na(bipoc_tract_flag) ~ 0, TRUE ~ bipoc_tract_flag))

  tract_info
}

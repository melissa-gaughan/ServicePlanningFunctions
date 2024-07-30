#' Clean Service Route Numbers
#'
#' @param route_table The dataframe representation of routes.txt from your GTFS file
#' @param netplan_gtfs T/F Is this GTFS from NetPlan? This impacts how route names are stored and generated
#'
#' @return A route dataframe with a field service_rte_num that representing the route number of Metro routes
#' @export
#'
#' @examples
clean_service_rte_num <- function(route_table, netplan_gtfs = FALSE){

  if (netplan_gtfs == TRUE){

routes <- route_table %>%
  tidyr::separate(route_id, into = c("route", "schedule"), sep = "-",  extra = "merge") %>%
  tidyr::unite(service_rte_num, route_short_name, route_long_name, remove = FALSE, sep = " ")   #deal with cases where names get split in two fields

  } else if (netplan_gtfs == FALSE){
  routes <- route_table %>%  #create blank field for non-NetPlan GTFS
    dplyr::mutate(service_rte_num = NA)
  }

routes_na <- routes %>%
  dplyr::mutate(service_rte_num = dplyr::case_when(
                                route_short_name == "A" ~ "671",
                                route_short_name == "B" ~ "672",
                                route_short_name ==  "C" ~ "673",
                                route_short_name ==  "D" ~ "674",
                                route_short_name ==  "E" ~ "675",
                                route_short_name == "F" ~ "676",
                                route_short_name == "G" ~ "677",
                                route_short_name == "H" ~ "678",
                                route_short_name == "A Line" ~ "671",
                                route_short_name == "B Line" ~ "672",
                                route_short_name ==  "C Line" ~ "673",
                                route_short_name ==  "D Line" ~ "674",
                                route_short_name ==  "E Line" ~ "675",
                                route_short_name == "F Line" ~ "676",
                                route_short_name == "G Line" ~ "677",
                                route_short_name == "H Line" ~ "678",
                                route_short_name == "Link light rail" ~ "599",
                                route_short_name == "First Hill Streetcar" ~ "96",
                                route_short_name == "South Lake Union Streetcar" ~ "98",
                                route_short_name == "SVT" ~ "629",
                                route_short_name == "Duvall-Monroe Shuttle" ~ "628",
                                route_short_name == "Trailhead Direct Mt. Si" ~ "634",
                                route_short_name == "Trailhead Direct Mailbox Peak" ~ "636",
                                route_short_name == "Trailhead Direct Issaquah Alps" ~ "637",
                                route_short_name == "Trailhead Direct Cougar Mt." ~ "639",
                                route_short_name == "Blue" ~ "999",
                                 TRUE ~ route_short_name

  ))


routes_named <- routes_na %>%
  dplyr::filter(is.na(service_rte_num)) %>% #double check that there are no na
  tidyr::unite(service_rte_num, route_short_name, route_long_name, remove = FALSE, sep = " ")


clean_routes <- dplyr::bind_rows( routes_na, routes_named) %>%
  dplyr::mutate(service_rte_num = stringr::str_remove(service_rte_num, "S|C|B|E")) %>%
  dplyr::distinct(service_rte_num, .keep_all = TRUE) %>%

na_routes <- routes %>%
  dplyr::mutate(service_rte_num = as.numeric(service_rte_num)) %>%
  dplyr::filter(is.na(service_rte_num) | is.null(service_rte_num))

if (nrow(na_routes)>0) {
  print(na_routes)
  stop("STOP! Route names need to be fixed")
} else {
  print("All routes have valid service route numbers.")
  print(routes$service_rte_num)
  }
 return(clean_routes)
}

#' Remove water bodies from polygon
#'
#' @param polygon sf object representing your polygon shapefile.
#' @param state_code The two-letter state code for the location in quotes. Washington is "WA".
#' @param county_code The name of the county your polygons are in. Used to fetch the water bodies in the area.
#' @param crs The four digit CRS (AKA EPSG) code for the projection system your data is in. King County's is 2926. Web Mercator is 4326
#'
#' @return sf object with erased water geometries through spatial intersection.
#' @export
#'
#' @examples

remove_water <- function(polygon, state_code = "WA", county_code = "King", crs = 2926 ){

  valid_polygon <- sf::st_make_valid(polygon) %>%
    sf::st_transform(crs = crs)



  water <- tigris::area_water(state = state_code, county = county_code, class = "sf") %>%
    sf::st_transform(crs = crs)

  print("Begining water erasing. This can take a while.")

polygon_erase <- ServicePlanningFunctions::st_erase(valid_polygon, water)

}


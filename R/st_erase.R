#' st_erase
#'
#' @param x shapefile to remove shapes from
#' @param y Shapefile of shapes to be removed
#'
#' @return returns union of the shapes, with y geometries removed
#' @export
#'
#' @examples
#' polygon = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
#create polygon to subset from
#' big_box <- sf::st_polygon(x= list(polygon))
#' plot(sf::st_geometry(big_box))
#' shape_to_remove <- matrix(c(1,1,1,2,2,2,2,1,1,1),ncol=2, byrow=TRUE)
#create smaller polygon to subset
#' polygon_to_remove <- sf::st_polygon(x = list(shape_to_remove))
#' plot(sf::st_geometry(polygon_to_remove))
# Polygon with smaller area removed
#' clipped_polygon <- st_erase(big_box, polygon_to_remove)
#' plot(sf::st_geometry(clipped_polygon))

st_erase <- function(x, y) {

  sf::st_difference(x, sf::st_union(y))
}

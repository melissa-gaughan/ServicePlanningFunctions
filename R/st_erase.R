#' st_erase
#'
#' @param x shapefile to remove shapes from
#' @param y Shapefile of shapes to be removed
#'
#' @return returns union of the shapes, with y geometries removed
#' @export
#'
#' @examples
st_erase <- function(x, y) {

  sf::st_difference(x, sf::st_union(y))
}

#' Save Objects from R Environment to Disc
#'
#' @param object_name Character. The name of the object you want to save.
#' @param file_location The folder location on your computer that you want to write to. Current requires full file path.
#'
#' @return Returns an RDS object containing the R object from the global environment.
#' @export
#'
#' @examples
save_objects <- function(object_name, file_location) {
  object <- get(object_name, envir = .GlobalEnv)
  saveRDS(object,  paste0(file_location, "/",object_name, ".RDS"))

}


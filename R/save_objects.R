#' Save Objects from R Environment to Disc
#'
#' @param object_name Character. The name of the object you want to save.
#' @param file_location The folder location on your computer that you want to write to. Currently requires full file path.
#'
#' @return Returns an RDS object containing the R object from the global environment.
#' @import purrr
#' @export
#'
#' @examples
#'
#' a <- as.data.frame("letters") #create sample data
#' b<- as.data.frame(c(1,2,3,4))

#' object_list <- ls() # create character vector of all objects in environment
#' #remove any you don't want written to disk
#' object_list <- object_list[! object_list %in% c("object_list")]
#' #use purrr to iterate
#' purrr:::map(object_list,
#' file_location ="C:/Users/mgaughan/OneDrive - King County/Documents/test", save_objects)

save_objects <- function(object_name, file_location) {
  object <- get(object_name, envir = .GlobalEnv)
  saveRDS(object,  paste0(file_location, "/",object_name, ".RDS"))

}


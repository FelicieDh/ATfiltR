#' myView
#'
#'
#' @param x What you want to View
#' @param title the title of what you want to View
#'
#' @return A View of a dataframe
#' @export

myView <- function(x, title)
  get("View", envir = as.environment("package:utils"))(x, title)




#' loadRData
#'
#'
#' @param fileName the file you are loading
#'
#' @return Load an RData file into a specified variable name
#' @export



loadRData <- function(fileName){
  #loads an RData file, and returns it
  load(fileName)
  get(ls()[ls() != "fileName"])
}

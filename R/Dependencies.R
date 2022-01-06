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

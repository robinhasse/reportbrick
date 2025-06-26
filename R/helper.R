#' Split dimension names
#'
#' Split each entry of a character vector and return one unnested character
#' vector.
#'
#' @author Robin Hasse
#'
#' @param x character vector
#' @param split character used to split \code{x}
#' @returns character vector with each dimension as an own entry

.split <- function(x, split = "\\.") {
  if (is.null(x)) {
    return(NULL)
  }
  unlist(strsplit(x, split))
}






#' All Combinations of dimension elements
#'
#' @param lst names list of dimension entries
#' @returns character vector with all combinations of the dimension elements
#'   each separated by \code{.}
#'
#' @author Robin Hasse
#'
#' @importFrom dplyr everything %>%
#' @importFrom tidyr unite

.combinations <- function(lst) {
  do.call(expand.grid, lst) %>%
    unite("combinations", everything(), sep = ".") %>%
    getElement("combinations")
}






#' Prepare variable
#'
#' Convert values by multiplying with given factor (usually 1/1000 to get from
#' million m2 to billion m2) and select the area quantity.
#'
#' @param x MAgPIE object of brick variable
#' @param factor numeric, scalar factor that \code{x} is multiplied with
#' @param onlyArea logical, if TRUE, select 'area' quantity and collapse
#'   quantity dimension
#' @returns scales and filtered MAgPIE object
#'
#' @author Robin Hasse
#'
#' @importFrom magclass getSets mselect collapseDim
#' @importFrom dplyr %>%

.prepVar <- function(x, factor = 1E-3, onlyArea = TRUE) {
  stopifnot("factor needs to be a scalar" =
              is.numeric(factor) && length(factor) == 1)
  x <- x * factor
  if (isTRUE(onlyArea) && "qty" %in% getSets(x)) {
    x <- x %>%
      mselect(qty = "area") %>%
      collapseDim(dim = "qty")
  }
  return(x)
}

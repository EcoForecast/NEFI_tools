#' crib_fun
#' converts a vector of [0,1] values to (0,1) a la Cribari-Neto & Zeileis 2010
#' @param x a vector of values on the interval [0,1]
#'
#' @return  a vector of values on the interval (0,1)
#' @export
#'
#' @examples
crib_fun <- function(x){(x * (length(x) - 1) + 0.5) / length(x)}
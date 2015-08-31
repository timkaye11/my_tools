#' A lookup for various regression methods
#'
#' This function allows you to lookup which package/method corresponds to a given regression method. Returns a data frame
#'  with the package and method that each method matches up with, as well as a short description of each method. 
#' @keywords reference
#' @export
#' lm_ref()

`%<<%` <- function(x, y) { bitwShiftL(x, y) }

#' bitwise right shift
#'
#' This function does a right shift without typing out "bitShiftR"
#' @param x and y to evaluate shift against
#' @export

`%>>%` <- function(x, y) { bitwShiftR(x, y) }

#' bitwise XOR
#'
#' This function does an XOR without typing out "bitwXor"
#' @param x and y to evaluate shift against
#' @export

`%^%` <- function(x, y) { bitwXor(x, y) }

#' bitwise AND
#'
#' This function does a left shift without typing out "bitwAnd"
#' @param x and y to evaluate shift against
#' @export

`%&%` <- function(x, y) { bitwAnd(x, y) }

#' bitwise OR
#'
#' This function does a left shift without typing out "bitwOr"
#' @param x and y to evaluate shift against
#' @export

`%|%` <- function(x, y) { bitwOr(x, y) }


#' @import Rcpp
#' @useDynLib 'FancyBar', .registration = TRUE
NULL

.onLoad <- function(libname, pkgname) {
  options(FancyBar.AddSymbolName = TRUE)
}

#' @import Rcpp
#' @useDynLib 'FancyBar', .registration = TRUE
NULL

.onLoad <- function(libname, pkgname) {
  options(FancyBar.AddSymbolName = TRUE)
}

utils::globalVariables(c(
  '.',
  'Group',
  'Timestamp',
  'Price',
  'Size',
  'Open',
  'High',
  'Low',
  'Close',
  'Volume',
  'VWAP',
  'TickCount',
  '..tsName',
  'ID',
  'multiplier'
))


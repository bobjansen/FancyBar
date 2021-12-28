calculateTimeBucket <- function(datetime, seconds) {
  timestamps <- as.integer(datetime)
  input_attrs <- attributes(datetime)
  timestamps <- timestamps - (timestamps %% seconds)
  for (attr_name in names(input_attrs)) {
    attr(timestamps, attr_name) <- input_attrs[[attr_name]]
  }
  timestamps
}

#' Make time based OHLC bars
#'
#' @param ticks The original trade data.
#' @param align_period Length of the time period to align by.
#' @param align_by Unit of the time period, can be:
#'   * seconds: one of `'s', 'secs', 'seconds`
#'   * minutes: one of `'m', 'mins', 'minutes`
#'   * hours: one of `'h', 'hours`
#'   * days: one of `'d', 'days'`
#'
#' @details
#' Bars are aligned on multiples of the time period since midnight, that is a
#' tick happening at 01:06:41 will be in a bar starting on:
#'
#'   * 01:06:40 if the bar length is 5 seconds
#'   * 01:06:30 if the bar length is 30 seconds
#'   * 01:06:00 if the bar length is 1 minute
#'   * 01:05:00 if the bar length is 5 minutes
#'   * 01:00:00 if the bar length is 1 hour
#'
#' The `highfrequency` uses the same logic but the timestamp of the bar is the
#' end of the bar period.
#'
#' @return Time based OHLC bars.
#' @import data.table
#' @export
makeTimeOHLCV <- function(ticks, align_period = 5L, align_by = 'minutes') {
  if (align_by %in% c('s', 'secs', 'seconds')) {
  } else if (align_by %in% c('m', 'mins', 'minutes')) {
    align_period <- align_period * 60L
  } else if (aligin_by %in% c('h', 'hours')) {
    align_period <- align_period * 60L * 60L
  } else if (aligin_by %in% c('d', 'days')) {
    align_period <- align_period * 60L * 60L * 24L
  } else {
    stop('align_by ', align_by, ' not known')
  }
  groups = calculateTimeBucket(ticks[['timestamp']], align_period)
  bars <- applyGroup(ticks, groups)
  bars[, timestamp := unique(groups)]
  bars
}

#' Make OHLC bars containing at least a certain amount of volume
#'
#' @param ticks Tick trade data.
#' @param target_volume The target volume for each bar.
#' @param split_large_trades Split trades larger than target_volume into 2 or
#' more bars.
#'
#' @import data.table
#' @return OHLCV bars of similar volume.
makeVolumeOHLCV <- function(
  ticks, target_volume, split_large_trades = FALSE
) {
  if (split_large_trades) {
    ticks <- ticks[, ':='(
      ID = 1:.N, multiplier = ceiling(size / target_volume)
    )]
    ticks <- ticks[rep(ID, times = multiplier)]
    ticks[
      multiplier > 1,
      size := c(rep(target_volume, .N - 1L), .SD[1L, size] %% target_volume),
      ID
    ]
  }
  groups <- find_volume_groups(ticks[, size], target_volume)
  applyGroup(ticks, groups)
}

applyGroup <- function(ticks, groups) {
  ticks <- ticks[, .(
    timestamp = first(timestamp),
    open = first(price),
    high = max(price),
    low = min(price),
    close = last(price),
    volume = sum(size)
  ), by = groups]
  ticks[, groups := NULL]
  ticks
}

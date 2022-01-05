#' Make time based OHLC bars
#'
#' The traditional OHLC bars based on buckets of time.
#'
#' @param ticks The original trade data.
#' @param align_period Length of the time period to align by.
#' @param align_by Unit of the time period, can be:
#'   * seconds: one of `'s', 'secs', 'seconds`
#'   * minutes: one of `'m', 'mins', 'minutes`
#'   * hours: one of `'h', 'hours`
#'   * days: one of `'d', 'days'`
#' @param name Name of the symbol.
#' @param add_name Should the name of the symbol be added to the column names?
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
#' The `highfrequency` package uses the same logic but the timestamp of the bar
#' is the end of the bar period.
#'
#' @return Time based OHLC bars.
#' @import data.table
#' @export
timeOHLCV <- function(
  ticks,
  align_period = 5L, align_by = 'minutes',
  name = NULL,
  add_name = getOption('FancyBar.AddSymbolName')
) {
  align_period <- if (align_by %in% c('s', 'secs', 'seconds')) {
    align_period
  } else if (align_by %in% c('m', 'mins', 'minutes')) {
    align_period * 60L
  } else if (aligin_by %in% c('h', 'hours')) {
    align_period * 60L * 60L
  } else if (aligin_by %in% c('d', 'days')) {
    align_period * 60L * 60L * 24L
  } else {
    stop('align_by ', align_by, ' not known')
  }
  groups = calculateTimeBucket(ticks[['timestamp']], align_period)
  bars <- applyGroup(ticks, groups)
  bars[, timestamp := unique(groups)]
  if (!is.null(name) && add_name) {
    data.table::setnames(bars, names(bars), paste(name, names(bars), sep = '.'))
  }
  bars
}


#' Tick based OHLC bars
#'
#' @param ticks Input trades
#' @param num_ticks The number of ticks to group in.
#' @param prev_bar Bar previous to this set, will be extended with `trades` if
#' not complete.
#' @import data.table
#' @export
tickOHLCV <- function(ticks, num_ticks, prev_bar = NULL) {
  if (!is.null(prev_bar)) {
    remaining_ticks = num_ticks - prevbar[, tickCount]
    prev_bar <- mergeBar(prev_bar, oneBarOHLCV(ticks[1:remaining_ticks]))
    ticks <- ticks[(remaining_ticks + 1L):.N]
  }
  if (nrow(ticks) > 0L) {
    groups <- ceiling(1:nrow(ticks) / num_ticks)
    rbind(prev_bar, applyGroup(ticks, groups))
  } else {
    data.table::as.data.table(prev_bar)
  }
}

#' Volume based OHLC bars
#'
#' Make OHLC bars containing at least a certain amount of volume.
#'
#' @param ticks Tick trade data.
#' @param target_volume The target volume for each bar.
#' @param split_large_trades Split trades larger than `target_volume` into 2 or
#' more bars.
#'
#' @import data.table
#' @return OHLCV bars of similar volume.
volumeOHLCV <- function(
  ticks, target_volume, split_large_trades = FALSE
) {
  if (split_large_trades) {
    ticks <- ticks[, ':='(
      ID = 1:.N, multiplier = ceiling(size / target_volume)
    )]
    ticks <- ticks[rep(ID, times = multiplier)]
    ticks[
      multiplier > 1,
      size := c(rep(target_volume, .N - 1L), first(size) %% target_volume),
      ID
    ]
  }
  groups <- find_volume_groups(ticks[, size], target_volume)
  applyGroup(ticks, groups)
}

#' Convert all ticks into one bar.
#'
#' @param ticks The ticks to convert
#' @return All ticks convert to one OHCLV bar
#' @export
oneBarOHLCV <- function(ticks) {
  ticks <- ticks[, .(
    timestamp = first(timestamp),
    Open = first(price),
    High = max(price),
    Low = min(price),
    Close = last(price),
    Volume = sum(size),
    VWAP = sum(price * size) / sum(size),
    TickCount = .N
  )]
  ticks
}

applyGroup <- function(ticks, groups) {
  ticks <- ticks[, .(
    Timestamp = first(timestamp),
    Open = first(price),
    High = max(price),
    Low = min(price),
    Close = last(price),
    Volume = sum(size),
    VWAP = sum(price * size) / sum(size),
    TickCount = .N
  ), by = groups]
  ticks[, groups := NULL]
  ticks
}

calculateTimeBucket <- function(datetime, seconds) {
  timestamps <- as.integer(datetime)
  input_attrs <- attributes(datetime)
  timestamps <- timestamps - (timestamps %% seconds)
  for (attr_name in names(input_attrs)) {
    attr(timestamps, attr_name) <- input_attrs[[attr_name]]
  }
  timestamps
}

mergeBar <- function(bar1, bar2) {
  if (bar1[1L, timestamp] < bar2[1L, timestamp]) {
    bar1[, ':='(
      Low = min(bar1[, low], bar2[, low]),
      High = max(bar1[, high], bar2[, high]),
      Close = bar2[, close],
      Volume = bar1[, volume] + bar2[, volume],
      VWAP = (bar1[, vwap] * bar1[, volume] + bar2[, vwap] * bar2[, volume]) /
        (bar1[, volume] + bar2[, volume]),
      TickCount = bar1[, tickCount] + bar2[, tickCount]
    )]
    bar1
  } else {
    bar2[, ':='(
      Low = min(bar1[, low], bar2[, low]),
      High = max(bar1[, high], bar2[, high]),
      Close = bar1[, close],
      Volume = bar1[, volume] + bar2[, volume],
      VWAP = (bar1[, vwap] * bar1[, volume] + bar2[, vwap] * bar2[, volume]) /
        (bar1[, volume] + bar2[, volume]),
      TickCount = bar1[, tickCount] + bar2[, tickCount]
    )]
    bar2
  }
}

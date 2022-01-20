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
#' @param prev_bar Bar from a previous call to `timeOHLCV()` with an earlier
#' set of trades.
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
  prev_bar = NULL,
  add_name = getOption('FancyBar.AddSymbolName')
) {
  if (!add_name) {
    name <- NULL
  }

  align_period <- if (align_by %in% c('s', 'secs', 'seconds')) {
    align_period
  } else if (align_by %in% c('m', 'mins', 'minutes')) {
    align_period * 60L
  } else if (align_by %in% c('h', 'hours')) {
    align_period * 60L * 60L
  } else if (align_by %in% c('d', 'days')) {
    align_period * 60L * 60L * 24L
  } else {
    stop('align_by ', align_by, ' not known')
  }

  groups = calculateTimeBucket(ticks[['Timestamp']], align_period)
  bars <- applyGroup(ticks, groups)
  bars[, Timestamp := NULL]
  data.table::setnames(bars, 'Group', 'Timestamp')
  if (!is.null(name)) {
    data.table::setnames(bars, names(bars), paste(name, names(bars), sep = '.'))

    tsName <- paste(name, 'Timestamp', sep = '.')
  } else {
    tsName <- 'Timestamp'
  }
  if (!is.null(prev_bar) && prev_bar[, ..tsName] == bars[1L, ..tsName]) {
    prev_bar <- data.table::copy(prev_bar)
    bars[1L] = mergeBar(prev_bar, bars[1L], name)
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
    remaining_ticks = unlist(num_ticks - prev_bar[, .(TickCount)])[[1L]]
    prev_bar <- mergeBar(prev_bar, oneBarOHLCV(ticks[1:remaining_ticks]))
    ticks <- ticks[(remaining_ticks + 1L):.N]
  }
  if (nrow(ticks) > 0L) {
    groups <- ceiling(1:nrow(ticks) / num_ticks)
    bars <- applyGroup(ticks, groups)
    bars[, Group := NULL]
    rbind(prev_bar, bars)
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
      ID = 1:.N, multiplier = ceiling(Size / target_volume)
    )]
    ticks <- ticks[rep(ID, times = multiplier)]
    ticks[
      multiplier > 1,
      Size := c(rep(target_volume, .N - 1L), first(Size) %% target_volume),
      ID
    ]
  }
  groups <- find_volume_groups(ticks[, Size], target_volume)
  bars <- applyGroup(ticks, groups)
  bars[, Group := NULL]  # The group equals the row number and is meaningless
  bars
}

#' Dollars (or other monies) traded based OHLC bars
#'
#' Make OHLC bars containing at least a certain amount of money traded.
#'
#' @param ticks Tick trade data.
#' @param target_volume The target volume for each bar.
#'
#' @import data.table
#' @return OHLCV bars of similar money traded.
dollarOHLCV <- function(ticks, target_volume) {
  groups <- find_volume_groups(ticks[, Size * Price], target_volume)
  bars <- applyGroup(ticks, groups)
  bars[, Group := NULL]  # The group equals the row number and is meaningless
  bars
}

#' Convert all ticks into one bar.
#'
#' @param ticks The ticks to convert
#' @return All ticks convert to one OHCLV bar
#' @export
oneBarOHLCV <- function(ticks) {
  ticks <- ticks[, .(
    Timestamp = first(Timestamp),
    Open = first(Price),
    High = max(Price),
    Low = min(Price),
    Close = last(Price),
    Volume = sum(Size),
    VWAP = sum(Price * Size) / sum(Size),
    TickCount = .N
  )]
  ticks
}

applyGroup <- function(ticks, Group) {
  bars <- ticks[, .(
    Timestamp = first(Timestamp),
    Open = first(Price),
    High = max(Price),
    Low = min(Price),
    Close = last(Price),
    Volume = sum(Size),
    VWAP = sum(Price * Size) / sum(Size),
    TickCount = .N
  ), by = Group]
  bars
}

calculateTimeBucket <- function(datetime, seconds) {
  timestamps <- as.integer(datetime)
  input_attrs <- attributes(datetime)
  timestamps <- timestamps - (timestamps %% seconds)
  attributes(timestamps) <- input_attrs
  timestamps
}

dropName <- function(bar, name) {
  oldColNames <- names(bar)
  data.table::setnames(
    bar, oldColNames, substring(oldColNames, nchar(name) + 2L)
  )
  bar
}

mergeBar <- function(bar1, bar2, name = NULL) {
  if (!is.null(name)) {
    bar1 <- dropName(bar1, name)
    bar2 <- dropName(bar2, name)
  }

  bar <- if (bar1[1L, Timestamp] <= bar2[1L, Timestamp]) {
    bar1[, ':='(
      High = max(bar1[, High], bar2[, High]),
      Low = min(bar1[, Low], bar2[, Low]),
      Close = bar2[, Close],
      Volume = bar1[, Volume] + bar2[, Volume],
      VWAP = (bar1[, VWAP] * bar1[, Volume] + bar2[, VWAP] * bar2[, Volume]) /
        (bar1[, Volume] + bar2[, Volume]),
      TickCount = bar1[, TickCount] + bar2[, TickCount]
    )]
    bar1
  } else {
    bar2[, ':='(
      High = max(bar1[, High], bar2[, High]),
      Low = min(bar1[, Low], bar2[, Low]),
      Close = bar1[, Close],
      Volume = bar1[, Volume] + bar2[, Volume],
      VWAP = (bar1[, VWAP] * bar1[, Volume] + bar2[, VWAP] * bar2[, Volume]) /
        (bar1[, Volume] + bar2[, Volume]),
      TickCount = bar1[, TickCount] + bar2[, TickCount]
    )]
    bar2
  }
  if (!is.null(name)) {
    data.table::setnames(bar, names(bar), paste(name, names(bar), sep = '.'))
  }
  bar
}

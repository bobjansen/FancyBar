library(data.table)
library(highfrequency)
library(microbenchmark)
library(quantmod)
library(xts)

set.seed(20220102)

start_price <- 100L
trades_per_second <- 10L
period_length <- 60L * 60L * 24L

arrival_times <- function(rate, period_length) {
  gaps <- rexp(rate * period_length, rate)
  # period_length - 1 so the whole period fits within one day.
  normalized <- gaps / (sum(gaps) / (period_length - 1L))
  normalized
}

gaps <- arrival_times(trades_per_second, period_length)
timestamps <- as.POSIXct(
  as.integer(as.POSIXct('2022-01-02')) + cumsum(gaps),
  origin = '1970-01-01'
)
N <- length(timestamps)
message('# trades: ', N)

price_delta <- rnorm(N, 0, sd = 0.04 * gaps)
prices <- start_price + cumsum(price_delta)
sizes <- as.integer(rgamma(N, 4, 0.01))
trades <- data.table(
  timestamp = timestamps,
  symbol = 'AAA',
  price = prices,
  size = sizes
)
bars <- timeOHLCV(trades)
chartSeries(bars)

xts_trades = trades[, .(timestamp, price, volume = size)]
hf_trades = trades[,
  .(DT = timestamp, SYMBOL = symbol, PRICE = price, VOLUME = size)
]

print(microbenchmark(
  fb = timeOHLCV(trades),
  xts = xts::to.minutes(xts_trades, 5L, name = 'AAA'),
  hf = highfrequency::makeOHLCV(hf_trades),
  times = 100L
))


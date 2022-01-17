library(data.table)
library(highfrequency)
library(microbenchmark)
library(quantmod)
library(xts)
library(FancyBar)

num_threads <- data.table::getDTthreads()
data.table::setDTthreads(threads = 1L)
on.exit(data.table::setDTthreads(threads = num_threads))

set.seed(20220102)

simulateTrades <- function(
  start_price, trades_per_second, period_length, symbol
) {
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

  price_delta <- rnorm(N, 0, sd = 0.04 * gaps)
  prices <- start_price + cumsum(price_delta)
  sizes <- as.integer(rgamma(N, 4, 0.01))
  trades <- data.table(
    Timestamp = timestamps,
    Symbol = symbol,
    Price = prices,
    Size = sizes
  )
}

# Benchmarking for 1 symbol ----
aaaTrades <- simulateTrades(
  start_price = 100L, trades_per_second = 10L, period_length = 60L * 60L * 24L,
  'AAA'
)
bbbTrades <- simulateTrades(
  start_price = 100L, trades_per_second = 10L, period_length = 60L * 60L * 24L,
  'BBB'
)

# Create bars and a plot when run manually.
if (interactive()) {
  bars <- timeOHLCV(aaaTrades, name = 'AAA')
  chartSeries(bars)
}

xts_trades = aaaTrades[, .(Timestamp, Price, volume = Size)]
hf_trades = aaaTrades[,
  .(DT = Timestamp, SYMBOL = Symbol, PRICE = Price, VOLUME = Size)
]

print(microbenchmark(
  fb = timeOHLCV(aaaTrades),
  xts = xts::to.minutes(xts_trades, 5L, name = 'AAA'),
  hf = highfrequency::makeOHLCV(hf_trades),
  times = 100L
))

# Benchmarking for 2 symbols ----
trades <- rbind(aaaTrades, bbbTrades)
xts_trades = trades[, .(Timestamp, Symbol, Price, volume = Size)]
hf_trades = trades[,
  .(DT = Timestamp, SYMBOL = Symbol, PRICE = Price, VOLUME = Size)
]

print(microbenchmark(
  fb = timeOHLCV(aaaTrades, name = 'AAA'),
  times = 10L
))
print(microbenchmark(
  fb = aaaTrades[, timeOHLCV(.SD, name = first(Symbol)), Symbol],
  times = 10L
))
print(microbenchmark(
  fb = trades[, timeOHLCV(.SD, name = first(Symbol)), Symbol],
  xts = xts_trades[,
    xts::to.minutes(.SD, k = 5L, symbol = first(Symbol)), Symbol
  ],
  hf = highfrequency::makeOHLCV(hf_trades),
  times = 10L
))

# Benchmarking for 20 symbols ----
trades <- rbindlist(lapply(LETTERS[1:20], function(symbol) {
  simulateTrades(
    start_price = 100L,
    trades_per_second = 5L,
    period_length = 60L * 60L * 24L,
    symbol
  )
}))
xts_trades = trades[, .(Timestamp, Symbol, Price, volume = Size)]
hf_trades = trades[,
  .(DT = Timestamp, SYMBOL = Symbol, PRICE = Price, VOLUME = Size)
]
print(microbenchmark(
  fb = trades[, timeOHLCV(.SD, name = first(Symbol)), Symbol],
  xts = xts_trades[,
    xts::to.minutes(.SD, k = 5L, Symbol = first(Symbol)), Symbol
  ],
  hf = highfrequency::makeOHLCV(hf_trades),
  times = 10L
))
print(microbenchmark(
  fb = trades[, timeOHLCV(.SD, name = first(Symbol)), Symbol],
  xts = xts_trades[,
    xts::to.minutes(.SD, k = 5L, Symbol = first(Symbol)), Symbol
  ],
  times = 25L
))

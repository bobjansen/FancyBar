library(data.table)

# One trade ----
jan1_2020 <- as.POSIXct('2020-01-01')
jan2_2020 <- as.POSIXct('2020-01-02')
jan3_2020 <- as.POSIXct('2020-01-03')
jan4_2020 <- as.POSIXct('2020-01-04')
test_trades <- data.table(
  timestamp = jan1_2020,
  symbol = 'AAA',
  price = 100,
  size = 10L
)
bar <- tickOHLCV(test_trades, num_ticks = 2L)
expect_equal(names(bar), c(
  'timestamp', 'open', 'high', 'low', 'close', 'volume', 'vwap', 'tickCount')
)
expect_equal(bar[, open], 100)
expect_equal(bar[, low], 100)
expect_equal(bar[, high], 100)
expect_equal(bar[, close], 100)
expect_equal(bar[, vwap], 100)
expect_equal(bar[, volume], 10L)
expect_equal(bar[, tickCount], 1L)

# Two trade ----
test_trades <- data.table(
  timestamp = c(jan1_2020, jan2_2020),
  symbol = 'AAA',
  price = c(100, 110),
  size = c(10L, 20L)
)
bar <- tickOHLCV(test_trades, num_ticks = 2L)
expect_equal(names(bar), c(
  'timestamp', 'open', 'high', 'low', 'close', 'volume', 'vwap', 'tickCount')
)
expect_equal(bar[, open], 100)
expect_equal(bar[, low], 100)
expect_equal(bar[, high], 110)
expect_equal(bar[, close], 110)
expect_equal(bar[, vwap], 320 / 3)
expect_equal(bar[, volume], 30L)
expect_equal(bar[, tickCount], 2L)

# Four trade ----
test_trades <- data.table(
  timestamp = c(jan1_2020, jan2_2020, jan3_2020, jan4_2020),
  symbol = 'AAA',
  price = c(120, 100, 110, 105),
  size = c(10L, 20L, 30L, 40L)
)
bar <- tickOHLCV(test_trades, num_ticks = 2L)
expect_equal(names(bar), c(
  'timestamp', 'open', 'high', 'low', 'close', 'volume', 'vwap', 'tickCount')
)
expect_equal(bar[1L, open], 120)
expect_equal(bar[1L, low], 100)
expect_equal(bar[1L, high], 120)
expect_equal(bar[1L, close], 100)
expect_equal(bar[1L, vwap], 320 / 3)
expect_equal(bar[1L, volume], 30L)
expect_equal(bar[1L, tickCount], 2L)

expect_equal(bar[2L, open], 110)
expect_equal(bar[2L, low], 105)
expect_equal(bar[2L, high], 110)
expect_equal(bar[2L, close], 105)
expect_equal(bar[2L, vwap], 750 / 7)
expect_equal(bar[2L, volume], 70L)
expect_equal(bar[2L, tickCount], 2L)

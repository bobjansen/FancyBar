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
bar <- oneBarOHLCV(test_trades)
expect_equal(names(bar), c(
  'Timestamp', 'Open', 'High', 'Low', 'Close', 'Volume', 'VWAP', 'TickCount')
)
expect_equal(bar[, Open], 100)
expect_equal(bar[, Low], 100)
expect_equal(bar[, High], 100)
expect_equal(bar[, Close], 100)
expect_equal(bar[, VWAP], 100)
expect_equal(bar[, Volume], 10L)
expect_equal(bar[, TickCount], 1L)

# Two trade ----
test_trades <- data.table(
  timestamp = c(jan1_2020, jan2_2020),
  symbol = 'AAA',
  price = c(100, 110),
  size = c(10L, 20L)
)
bar <- oneBarOHLCV(test_trades)
expect_equal(names(bar), c(
  'Timestamp', 'Open', 'High', 'Low', 'Close', 'Volume', 'VWAP', 'TickCount')
)
expect_equal(bar[, Open], 100)
expect_equal(bar[, Low], 100)
expect_equal(bar[, High], 110)
expect_equal(bar[, Close], 110)
expect_equal(bar[, VWAP], 320 / 3)
expect_equal(bar[, Volume], 30L)
expect_equal(bar[, TickCount], 2L)

# Four trade ----
test_trades <- data.table(
  timestamp = c(jan1_2020, jan2_2020, jan3_2020, jan4_2020),
  symbol = 'AAA',
  price = c(120, 100, 110, 105),
  size = c(10L, 20L, 30L, 40L)
)
bar <- oneBarOHLCV(test_trades)
expect_equal(names(bar), c(
  'Timestamp', 'Open', 'High', 'Low', 'Close', 'Volume', 'VWAP', 'TickCount')
)
expect_equal(bar[, Open], 120)
expect_equal(bar[, Low], 100)
expect_equal(bar[, High], 120)
expect_equal(bar[, Close], 105)
expect_equal(bar[, VWAP], 107)
expect_equal(bar[, Volume], 100L)
expect_equal(bar[, TickCount], 4L)

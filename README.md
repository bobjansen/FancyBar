# Introduction

Create OHLC bars based on more than just time. Currently, bars based on the
following dimensions are supported:

- Time with `timeOHLCV()`
- Volume with `volumeOHLCV()`
- Number of ticks `tickOHLCV()`
- Dollar Volume (Price times Size) `dollarOHLCV()`

as is converting a set of trades into one bar using `oneBarOHLCV()`. The bars
contain additional columns compared to regular OHLCV data:

- VWAP
- TickCount: Number of ticks/trades in the bar

# Installation

```R
devtools::install_github('bobjansen/FancyBar')
```

# Benchmark

On my machine, performance of creating time-based bars is slightly faster than
`to.period()` in the
[`xts`](https://CRAN.R-project.org/package=xts) package and
clearly faster than `makeOHLCV()` in the
[`highfrequency`](https://CRAN.R-project.org/package=highfrequency)
package.

The benchmarking script can be found in `inst/benchmark.R`.

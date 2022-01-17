# Introduction

Create OHLC bars based on more than just time. Currently, bars based on the 
following dimensions are supported:

- Time with `timeOHLCV()`
- Volume with `volumeOHLCV()`
- Number of ticks `tickOHLCV()`

as is converting a set of trades into one bar using `oneBarOHLCV()`. The bars
contain additional columns compared to regular OHLCV data:

- VWAP
- TickCount: Number of ticks/trades in the bar

# Installation

```R
devtools::install_github('bobjansen/FancyBar')
```

# Benchmark

On my machine, performance of creating time-based bars is comparable to
`to.period()` in the
[`xts`](https://cran.r-project.org/web/packages/xts/index.html) package and
clearly faster than `makeOHLCV()` in the
[`highfrequency`](https://cran.r-project.org/web/packages/highfrequency/index.html)
package.

# Introduction

Create OHLC bars based on more than just time. Currently, bars based on the 
following dimensions are supported:

- Time with `timeOHLCV()`
- Volume with `volumeOHLCV()`
- Number of ticks `tickOHLCV()`

as is converting a set of trades into one bar using `oneBarOHLCV()`.

# Installation

```R
devtools::install_github('bobjansen/FancyBar')
```


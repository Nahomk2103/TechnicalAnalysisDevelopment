# Exponential Moving Average (EMA)

ema <- function(data, period) {
  
  multiplier <- 2 / (period + 1)
  
  ema_values <- numeric(length(data))
  
  ema_values[1] <- data[1]
  
  for(i in 2:length(data)) {
    
    ema_values[i] <-
      (data[i] - ema_values[i - 1]) *
      multiplier +
      ema_values[i - 1]
  }
  
  return(ema_values)
}

# Moving Average Convergence Divergence (MACD)

macd <- function(data,
                 short_period,
                 long_period,
                 signal_period) {
  
  # Calculate EMAs
  short_ema <- ema(data, short_period)
  
  long_ema <- ema(data, long_period)
  
  # MACD line
  macd_line <- short_ema - long_ema
  
  # Signal line
  signal_line <- ema(macd_line, signal_period)
  
  # Histogram
  histogram <- macd_line - signal_line
  
  result <- list(
    macd_line = macd_line,
    signal_line = signal_line,
    histogram = histogram
  )
  
  return(result)
}

data <- c(
  100, 105, 110, 115, 120,
  125, 130, 128, 132, 135,
  140, 138, 142, 145, 150
)

macd_result <- macd(
  data,
  short_period = 3,
  long_period = 5,
  signal_period = 2
)

print(macd_result)
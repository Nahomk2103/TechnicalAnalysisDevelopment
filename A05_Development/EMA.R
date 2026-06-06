ema <- function(data, period) {
  
  # Check if data length is sufficient
  if(length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Calculate smoothing factor
  multiplier <- 2 / (period + 1)
  
  # Initialize EMA vector
  ema_values <- numeric(length(data))
  
  # First EMA value is SMA of first period
  ema_values[period] <- sum(data[1:period]) / period
  
  # Calculate remaining EMA values
  for(i in (period + 1):length(data)) {
    
    ema_values[i] <- ((data[i] - ema_values[i - 1]) * multiplier) +
      ema_values[i - 1]
  }
  
  return(ema_values[period:length(data)])
}

# Sample data

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate EMA

ema_result <- ema(data, period = 3)

print(ema_result)
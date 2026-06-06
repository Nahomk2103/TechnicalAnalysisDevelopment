sma <- function(data, period) {
  
  # Check if data length is sufficient
  if(length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize vector for SMA values
  sma_values <- numeric(length(data) - period + 1)
  
  # Calculate SMA
  for(i in 1:(length(data) - period + 1)) {
    
    current_window <- data[i:(i + period - 1)]
    
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}

# Sample data

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate SMA

sma_result <- sma(data, period = 3)

print(sma_result)
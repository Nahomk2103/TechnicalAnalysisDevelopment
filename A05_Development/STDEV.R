stdev <- function(data, period) {
  
  # Check if data length is sufficient
  if(length(data) < period) {
    stop("Data length should be greater than or equal to the period")
  }
  
  # Initialize vector
  stdev_values <- numeric(length(data) - period + 1)
  
  # Calculate rolling standard deviation
  for(i in 1:(length(data) - period + 1)) {
    
    current_window <- data[i:(i + period - 1)]
    
    mean_value <- sum(current_window) / period
    
    variance <- sum((current_window - mean_value)^2) / period
    
    stdev_values[i] <- sqrt(variance)
  }
  
  return(stdev_values)
}

# Sample data

data <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

# Calculate STDEV

stdev_result <- stdev(data, period = 3)

print(stdev_result)
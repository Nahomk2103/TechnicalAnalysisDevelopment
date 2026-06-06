# Simple Moving Average (SMA)

sma <- function(data, period) {
  
  sma_values <- numeric(length(data) - period + 1)
  
  for(i in 1:(length(data) - period + 1)) {
    
    current_window <- data[i:(i + period - 1)]
    
    sma_values[i] <- sum(current_window) / period
  }
  
  return(sma_values)
}

# Relative Strength Index (RSI)

rsi <- function(data, period) {
  
  diff_values <- diff(data)
  
  gains <- numeric(length(diff_values))
  losses <- numeric(length(diff_values))
  
  for(i in 1:length(diff_values)) {
    
    if(diff_values[i] > 0) {
      
      gains[i] <- diff_values[i]
      
    } else {
      
      losses[i] <- abs(diff_values[i])
    }
  }
  
  avg_gain <- sum(gains[1:period]) / period
  avg_loss <- sum(losses[1:period]) / period
  
  rsi_values <- rep(NA, length(data))
  
  for(i in (period + 1):length(data)) {
    
    avg_gain <- ((avg_gain * (period - 1)) +
                   gains[i - 1]) / period
    
    avg_loss <- ((avg_loss * (period - 1)) +
                   losses[i - 1]) / period
    
    if(avg_loss == 0) {
      
      rsi_values[i] <- 100
      
    } else {
      
      rs <- avg_gain / avg_loss
      
      rsi_values[i] <- 100 - (100 / (1 + rs))
    }
  }
  
  return(rsi_values)
}

# Stochastic RSI

stoch_rsi <- function(data, period, k_period, d_period) {
  
  rsi_values <- rsi(data, period)
  
  valid_rsi <- rsi_values[!is.na(rsi_values)]
  
  min_rsi <- min(valid_rsi)
  
  max_rsi <- max(valid_rsi)
  
  k_values <- (valid_rsi - min_rsi) /
    (max_rsi - min_rsi)
  
  k_line <- sma(k_values, k_period)
  
  d_line <- sma(k_line, d_period)
  
  result <- list(
    k_line = k_line,
    d_line = d_line
  )
  
  return(result)
}

data <- c(
  45, 50, 48, 55, 52,
  49, 58, 60, 65, 62,
  67, 69, 66, 71, 74,
  72, 76, 78, 80, 79
)

stoch_result <- stoch_rsi(
  data,
  period = 5,
  k_period = 3,
  d_period = 3
)

print(stoch_result)
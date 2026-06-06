linreg <- function(regressionSource, regressionLength, regressionOffset) {
  
  # Total number of elements
  n <- length(regressionSource)
  
  # Validation checks
  if(regressionLength > n) {
    stop("regressionLength cannot be greater than the number of elements in regressionSource")
  }
  
  if(regressionOffset >= regressionLength) {
    stop("regressionOffset must be less than regressionLength")
  }
  
  # Calculate start and end positions
  start_index <- max(1, n - regressionLength + regressionOffset)
  
  end_index <- min(n, n - regressionOffset)
  
  # Extract subset
  source_subset <- regressionSource[start_index:end_index]
  
  # Create index values
  index_values <- 1:length(source_subset)
  
  # Means
  mean_index <- sum(index_values) / length(index_values)
  
  mean_source <- sum(source_subset) / length(source_subset)
  
  # Numerator and denominator
  numerator <- sum(
    (index_values - mean_index) *
      (source_subset - mean_source)
  )
  
  denominator <- sum(
    (index_values - mean_index)^2
  )
  
  # Slope and intercept
  slope <- numerator / denominator
  
  intercept <- mean_source - slope * mean_index
  
  # Predicted values
  predicted_values <- slope * index_values + intercept
  
  # Return results
  result <- list(
    slope = slope,
    intercept = intercept,
    predicted_values = predicted_values
  )
  
  return(result)
}

# Sample data

data <- c(100, 105, 110, 115, 120, 125, 130)

linreg_result <- linreg(
  regressionSource = data,
  regressionLength = 5,
  regressionOffset = 0
)

print(linreg_result)
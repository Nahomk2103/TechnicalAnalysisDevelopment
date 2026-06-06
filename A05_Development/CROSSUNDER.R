crossunder <- function(arr1, arr2) {
  
  # Check if arrays have same length
  if(length(arr1) != length(arr2)) {
    stop("Both arrays should have the same length")
  }
  
  # Initialize signal vector
  crossunder_signals <- character(length(arr1))
  crossunder_signals[1] <- "None"
  
  # Check for crossunder signals
  for(i in 2:length(arr1)) {
    
    if(arr1[i] < arr2[i] &&
       arr1[i - 1] >= arr2[i - 1]) {
      
      crossunder_signals[i] <- "True"
      
    } else {
      
      crossunder_signals[i] <- "False"
    }
  }
  
  return(crossunder_signals)
}

arr1 <- c(10, 12, 15, 20, 18, 22, 25, 24, 21)

arr2 <- c(18, 20, 22, 18, 15, 12, 10, 11, 13)

crossunder_result <- crossunder(arr1, arr2)

print(crossunder_result)
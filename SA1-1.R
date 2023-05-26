# Input validation functions
validate_Xn <- function(x) {
  return(x >= 0.10 && x <= 0.40)
}

validate_Yn <- function(y) {
  return(y >= 0.01 && y <= 0.05)
}

# Read input for Xn
read_Xn <- function() {
  cat("Enter Xn values for factories (0.10 <= Xn <= 0.40 and sum(Xn) = 1): ")
  xn <- as.numeric(strsplit(trimws(readline()), " ")[[1]])
  if (length(xn) != 3 || any(is.na(xn)) || !all(sapply(xn, validate_Xn)) || sum(xn) != 1) {
    cat("Invalid input. Please try again.\n")
    return(read_Xn())
  }
  return(xn)
}

# Read input for Yn
read_Yn <- function() {
  cat("Enter Yn values for defective rates (0.01 <= Yn <= 0.05 and sum(Yn) = 0.12): ")
  yn <- as.numeric(strsplit(trimws(readline()), " ")[[1]])
  if (length(yn) != 3 || any(is.na(yn)) || !all(sapply(yn, validate_Yn)) || sum(yn) != 0.12) {
    cat("Invalid input. Please try again.\n")
    return(read_Yn())
  }
  return(yn)
}

# Main program
xn_values <- read_Xn()
yn_values <- read_Yn()

defective_probability <- sum(xn_values * yn_values)
cat("The probability that a randomly selected product is defective is:", defective_probability)




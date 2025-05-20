##################################################
# These functions are developed by               #  
# MATH1318 students                              #
# Le Van Tra Tran and Tin Trung Pham             #
# in 2024. WE thank them for their contribution! #
##################################################



# Helper function ---------------------------------------------------------------------

helper <- function(class = c("acf", "pacf"), ...) {
  
  # Capture additional arguments
  params <- match.call(expand.dots = TRUE)
  params <- as.list(params)[-1]
  
  # Calculate ACF/PACF values
  if (class == "acf") {
    acf_values <- do.call(acf, c(params, list(plot = FALSE)))
  } else if (class == "pacf") {
    acf_values <- do.call(pacf, c(params, list(plot = FALSE)))
  }
  
  # Extract values and lags
  acf_data <- data.frame(
    Lag = as.numeric(acf_values$lag),  
    ACF = as.numeric(acf_values$acf)   
  )
  
  # Identify seasonal lags to be highlighted
  seasonal_lags <- acf_data$Lag %% 1 == 0
  
  # Plot ACF/PACF values
  if (class == "acf") {
    do.call(acf, c(params, list(plot = TRUE)))
  } else if (class == "pacf") {
    do.call(pacf, c(params, list(plot = TRUE)))
  }
  
  # Add colored segments for seasonal lags
  for (i in which(seasonal_lags)) {
    segments(x0 = acf_data$Lag[i], y0 = 0, x1 = acf_data$Lag[i], y1 = acf_data$ACF[i], col = "red")
  }
}


# seasonal_acf ------------------------------------------------------------

seasonal_acf <- function(...) {
  helper(class = "acf", ...)
}


# seasonal_pacf -----------------------------------------------------------

seasonal_pacf <- function(...) {
  helper(class = "pacf", ...)
}


# Data for Testing -----------------------------------------------------------------

# Generate time series data
set.seed(123)
n <- 120 # Number of observations
time <- seq(1, n) # Time index
seasonal_pattern <- sin(2 * pi * time / 12) # Seasonal pattern with a period of 12 months
trend <- 0.1 * time # Linear trend
noise <- rnorm(n, mean = 0, sd = 0.5) # Random noise

# Create a time series object
ts_data <- ts(seasonal_pattern + trend + noise, frequency = 12, start = c(2024, 1))



# seasonal_acf Testing ---------------------------------------------------------------------

# Minimal arguments
acf(ts_data)
seasonal_acf(ts_data)

# Extended arguments
acf(ts_data, lag.max=24, main = "ACF of the Time Series Data", demean = TRUE)
seasonal_acf(ts_data, lag.max=24, main = "ACF of the Time Series Data", demean = TRUE)


# seasonal_pacf Testing --------------------------------------------------------------------

# Minimal arguments
pacf(ts_data)
seasonal_pacf(ts_data)

# Extended arguments
pacf(ts_data, lag.max=120, main = "PACF of the Time Series Data")
seasonal_pacf(ts_data, lag.max=120, main = "PACF of the Time Series Data")







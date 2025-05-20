residual.analysis <- function(model, std = TRUE,start = 2, shift = 0, class = c("ARIMA","GARCH","ARMA-GARCH", "garch", "fGARCH")[1]){
  # If you have an output from arima() function use class = "ARIMA"
  # If you have an output from garch() function use class = "GARCH". 
  # If you have an output from garchFit() function use class = "fGARCH" - added by HD - 5/5/21 
  # If you have an output from garch() function from tseries package use class = "garch" - added by HD - 20/5/21 
  # Please note that you should use tseries package to be able to run this function for GARCH models.
  # If you have an output from ugarchfit() function use class = "ARMA-GARCH"
  library(TSA)
  library(FitAR) 
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = Lag(rstandard(model),shift)
      res.model = na.omit(res.model)
    }else{
      res.model = Lag(residuals(model),shift)
      res.model = na.omit(res.model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "garch"){
    res.model = model$residuals[start:model$n.used]  
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else if (class == "fGARCH"){
    res.model = model@residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  par(mfrow=c(3,2))
  plot(res.model,type='o',ylab='Standardised residuals', main="Time series plot of standardised residuals")
  abline(h=0)
  hist(res.model,main="Histogram of standardised residuals")
  qqnorm(res.model,main="QQ plot of standardised residuals")
  qqline(res.model, col = 2)
  acf(res.model,main="ACF of standardised residuals")
  print(shapiro.test(res.model))
  k=0
  if (length(res.model) < 30){
    lagM <- length(res.model) - 1
  } else {
    lagM <- 29
  }
  LBQPlot(res.model, lag.max = lagM, StartLag = k + 1, k = 0, SquaredQ = FALSE)
  par(mfrow=c(1,1))
}

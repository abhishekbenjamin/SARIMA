library(tseries)
library(forecast)
library(zoo)
library(lmtest)

ppi <- timeseries_ppi

class(ppi)

# Transforming into Time-Series format

ppi_time <- ts(ppi$ppi, start = 1960, frequency = 4)
class(ppi_time)

autoplot(ppi_time)
autoplot(decompose(ppi_time))
autoplot(ppi_time)
boxplot(ppi_time ~ time(ppi_time))

BoxCox.lambda(ppi_time)

autoplot(Acf(ppi_time),main="ACF")
autoplot(Pacf(ppi_time),main="PACF")

adf.test(ppi_time)

model <- auto.arima(ppi_time, ic="aic", trace=T)

sarima_model <-arima(ppi_time, order = c(1,1,1), seasonal = list(order = c(0, 0, 2), period = 4))

autoplot(Acf(ts(sarima_model$residuals)), main ="ACF Residual")
autoplot(Acf(ts(sarima_model$residuals)), main ="PACF Residual")
autoplot(sarima_model$residuals)

ppi_forecast <- forecast(sarima_model, level = c(95), h = 10*4)
ppi_forecast
autoplot(ppi_forecast)

Box.test(ppi_forecast$residuals, lag = 5, type = "Ljung-Box")
Box.test(ppi_forecast$residuals, lag = 10, type = "Ljung-Box")
Box.test(ppi_forecast$residuals, lag = 15, type = "Ljung-Box")

rm(list=ls())

library(forecast)
library(TSA)
library(tseries)
library(car)


### https://support.sas.com/documentation/cdl/en/etsug/60372/HTML/default/viewer.htm#etsug_arima_sect029.htm

alpha <- -0.5
y1 <- arima.sim(n = 10000, list(ma = alpha), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y)
eacf(y1)

alpha <- -0.5
y2 <- arima.sim(n = 10000, list(ar = alpha), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y)
eacf(y2)


alpha <- 0.8
y3 <- arima.sim(n = 10000, list(ma = alpha), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y)
eacf(y3)

alpha <- -0.8
y4 <- arima.sim(n = 10000, list(ar = alpha), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y)
eacf(y4)


alpha <- -0.8
theta <- 0.5
y5 <- arima.sim(n = 10000, list(ar = alpha, ma = theta), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y)
eacf(y5)


alpha <- 0.8
theta <- -0.5
y6 <- arima.sim(n = 10000, list(ar = alpha, ma = theta), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y)
eacf(y6)


y7 <- arima.sim(n = 10000, list(ar = c(0.8, -0.4858), ma = c(-0.2279, 0.2488,-0.1)), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y7)
eacf(y7)


y8 <- arima.sim(n = 10000, list(ar = c(0.8, -0.4, 0.5), ma = c(-0.7)), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y)
eacf(y8)


y9 <- arima.sim(n = 10000, list(ma = c(0.8, -0.4, 0.5), ar = c(-0.7)), innov=rnorm(10000))

par(mfrow = c(2,1))
# Correlograma
acf(y)
# Gràfic de les Correlacions parcials  
pacf(y)
eacf(y9)







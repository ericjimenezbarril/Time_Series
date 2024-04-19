library(zoo)
library(forecast)

dades <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac3TS.txt", header=T)
start <- as.yearmon("1916-01-01")
end   <- as.yearmon("2015-01-01")
dades.ts <- ts(dades$Quakes, start=start, end=end, frequency=1)
ts.plot(dades.ts)
### Treure tendencia
t <- 1:length(dades$Quakes) 
### temps indexat
dades.lm=lm(Quakes~t , data=dades)
summary(dades.lm)
beta <- dades.lm$coefficients
plot(t, dades$Quakes);
lines(t, beta[1]+beta[2]*t,col="orchid2", lwd=2)
plot(t, dades.lm$residuals, main="Noise process (residuals)"); 
abline(h=0)
par(mfrow = c(2,1))
acf(dades.lm$residuals)
pacf(dades.lm$residuals)
auto.arima(dades.lm$residuals)


### AR(1) process
nobs <- 10000
serie <- vector()
serie[1] <- 0
alpha <- 0.6
for (i in 2:nobs)
{
  serie[i] <- alpha*serie[i-1]+rnorm(1) 
}
par(mfrow = c(1,1))
plot(serie)
par(mfrow = c(2,1))
acf(serie)
pacf(serie)
fitarima = auto.arima(serie, allowdrift=FALSE)
fitarima
coeftest(fitarima)

arima1<-arima(serie,c(1,0,0))
arima1
coeftest(arima1)


arima2<-arima(serie,c(1,0,0),  include.mean = FALSE)
arima2
coeftest(arima2)

alpha <- -0.3
y <- arima.sim(n = 10000, list(ar = alpha), innov=rnorm(10000))
par(mfrow = c(1,1))
plot(y)
par(mfrow = c(2,1))
acf(y)
pacf(y)
fitarima = auto.arima(y, allowdrift=FALSE)
fitarima
coeftest(fitarima)


arima2<-arima(y,c(1,0,0),  include.mean = FALSE)
arima2
coeftest(arima2)





### MA(1) process
nobs <- 1000
serie <- vector()
zt    <- vector()
serie[1] <- 0
zt[1]    <- 0
alpha <- 0.6
for (i in 2:nobs)
{
  zt[i]    <- rnorm(1)
  serie[i] <- zt[i]+alpha*zt[i-1] 
}
par(mfrow = c(1,1))
plot(serie)
par(mfrow = c(2,1))
acf(serie)
pacf(serie)
fitarima = auto.arima(serie, allowdrift=FALSE)
fitarima
coeftest(fitarima)
y <- arima.sim(n = 1000, list(ma = alpha), innov=rnorm(1000))


alpha <- -0.5
y <- arima.sim(n = 10000, list(ma = alpha), innov=rnorm(10000))
par(mfrow = c(1,1))
plot(y)
par(mfrow = c(2,1))
acf(y)
pacf(y)
fitarima = auto.arima(y, allowdrift=FALSE)
fitarima
coeftest(fitarima)


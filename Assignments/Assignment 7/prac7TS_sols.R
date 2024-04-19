### El company que m'ha demanat aquest script que es posi en contacte amb mi a: alopezrat@uoc.edu


library(forecast)
library(TSA)
library(tseries)
library("lmtest")

data(AirPassengers)
ts.plot(AirPassengers)
AirPassengers

adf.test(AirPassengers, alternative="stationary")

### Sembla que t? tend?ncia i estacionalitat. No ?s un proc?s estacionari. 
### Per aplicar el test de D-F cal primer treure la component estacional. 
### Diferenciaciem en componente estacional d'ordre 12 per ser de 12 meses 
### Difer?ncies estacionals (ordre 12)

AP2   <- diff(AirPassengers, lag=12) 
AP2
ts.plot(AP2)

## El test de Dickey-Fuller
adf.test(AP2, alternative="stationary") 
## El test de Dickey-Fuller confirma que la s?rie no ?s estacion?ria


mod1  <- auto.arima(AirPassengers) 
mod1
coeftest(mod1)

## Ara anem a fer el que fa l'auto.arima nosaltres directament 

### Difer?ncia d'ordre 1
AP2_1 <- diff(AP2, lag=1) 
plot.ts(AP2_1)


### Es estacionaria?
adf.test(AP2_1, alternative="stationary")
## A l'aplicar la difer?ncia d'ordre 1, el test de Dickey-Fuller confirma que la s?rie ?s estacionaria

par(mfrow = c(2,1))
acf(AP2_1)
pacf(AP2_1)
eacf(AP2_1)

fitarima= auto.arima(AP2_1)
fitarima
coeftest(fitarima)

arima1<-arima(AP2_1,c(1,0,0), include.mean = FALSE)
arima1
coeftest(arima1)
# Z(n) = Epsilon(n) -0.307618*Z(n-1) 
# Escrit d'una altra manera amb el polinomi de retards: Z = (1 - 0.307618*B) Epsilon 

arima2<-arima(AP2_1,c(0,0,1), include.mean = FALSE)
arima2
coeftest(arima2)
# Z(n) = Epsilon(n) -0.318439*Epsilon(n-1) 
# Escrit d'una altra manera amb el polinomi de retards: Z = (1 - 0.318439*B) Epsilon 





### Predicci? per al seg?ent any (1961)
par(mfrow = c(1,1))
pred <- forecast(AirPassengers, h=10, model=mod1, level = c(80, 95), fan=TRUE)
pred
plot(pred)

### Simulem un proc?s com l'anterior: 

phi1 <- mod1$coef[1]
phi1 
phi2 <- mod1$coef[2]
phi2
theta1 <- mod1$coef[3]
theta1

sigma <- mod1$sigma2
sigma

simu <- arima.sim(model=list(order=c(2,1,1), ar=c(phi1, phi2), ma=c(theta1), seasonal=list(order=c(0, 1, 0), period=12)), n=144, innov=rnorm(144, 0, sqrt(sigma)))
plot.ts(simu)

### Ens quedem amb les 100 primeres observacions i intentem fer prediccions per les 44 darreres

train_serie <- AirPassengers[1:100]
pred2 <- forecast(train_serie, h=44, model=mod1)
pred2
plot(pred2)

### Pintem a la gr?fica els valors correctes
lines(seq(101, 144, 1), AirPassengers[101:144], col="red")

### Comptem en quants casos el valor real est??fora de l'interval de confian?a del 95%:
df <- data.frame(real=AirPassengers[101:144], lower=pred2$lower[, 2], pred=pred2$mean, upper=pred2$upper[, 2])
df 

head(pred2)
head(df)

### Creem una nova variable que indica si l'observaci? est? fora de l'interval 
df$fora <- ifelse(df$real >= df$lower & df$real <= df$upper, 0, 1) 
df$fora
### I comptem
table(df$fora)

### Veiem que totes les observacions cauen dins de l'interval del 95%.

library(forecast)
library(TSA)
library(tseries)

data(robot)
plot(robot)
head(robot)
adf.test(robot)
auto.arima(robot)

arima1<-arima(robot,c(1,0,1))
arima1


#### PRACTICA 6

## Exercici 1

library(forecast)
library(TSA)
library(tseries)

serie1 <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac6TS_1.txt", header=T)
head(serie1)
ts.plot(serie1, col="darkblue",lwd=2)

### Test de Dickey-Fuller Aumentado “adf.test”: 
### http://finanzaszone.com/analisis-y-prediccion-de-series-temporales-con-r-ii-estacionariedad-y-raices-unitarias/
### H0: No es estacionaria
adf.test(serie1$x)
### Si rechazamos la hipotesis nula, rechazamos no estacionariedad

# Correlograma y función de autocorrelación parcial
par(mfrow = c(2,1))
acf(serie1$x)
pacf(serie1$x)

eacf(serie1$x)

# Mirant l'ACF i PACF vei
em que un bon candidat pot ser un model MA(1): 
# Z(n) = Epsilon(n) + Theta(1)*Epsilon(n-1) + Constante 
# on Epsilon és el soroll blanc
# Qui és Theta(1)? 
arima1<-arima(serie1,c(0,0,1))
arima1
coeftest(arima1)
# Z(n) = Epsilon(n) + 0.8037*Epsilon(n-1) + 12.0225
# Escrit d'una altra manera amb el polinomi de retards: Z = (1+0.8037B) Epsilon + 12.0225





/* La funció auto.arima ens dóna el mateix 
fitarima= auto.arima(serie1)
library("lmtest")
coeftest(fitarima)

### Mirem els residus
par(mfrow = c(1,1))
plot.ts(arima1$residuals,sub="Residuales del modelo MA(1)", xlab="Tiempo",ylab="Residuales")
par(mfrow = c(2,1))
acf(arima1$residuals)
pacf(arima1$residuals)

par(mfrow = c(1,1))
qqnorm(arima1$residuals, sub="Gráfico Q para evaluar normalidad");

qqPlot(arima1$residuals, dist="norm", id=list(method="y", n=2))
qqline(arima1$residuals)


checkresiduals(arima1)

### H0: És soroll blanc
Box.test(arima1$residuals)

auto.arima(arima1$residuals)




ma1.r <- arima.sim(model=list(ma=c(0.8037)), n=1000)
plot(ma1.r)
fit <- arima(ma1.r, order=c(0,0,1))
autoplot(fit)


### Exercici 2

serie2 <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac6TS_2.txt", header=T)
head(serie2);
par(mfrow = c(1,1))
ts.plot(serie2, col="darkblue",lwd=2)


### Test de Dickey-Fuller Aumentado “adf.test”: 
adf.test(serie2$x)
### Si rechazamos la hipotesis nula, rechazamos no estacionariedad


par(mfrow = c(2,1))
acf(serie2)
pacf(serie2)
eacf(serie2$x)

eacf(serie2)
# Mirant l'ACF i PACF veiem que un bon candidat pot ser un model AR(3): 
# Z(n) = Phi(1)*Z(n-1) + Phi(2)*Z(n-2) + Phi(3)*Z(n-3) + Epsilon(n) + Constante
# on Epsilon és el soroll blanc
# Qui és Phi(1), Phi(2) i Phi(3)? 
arima2<-arima(serie2,c(3,0,0))
arima2
coeftest(arima2)

arima3<-arima(serie2,c(1,0,1))
arima3
coeftest(arima3)

a <-auto.arima(serie2)
a
coeftest(a)

checkresiduals(arima2)
### Soroll blanc: https://rpubs.com/Meca/376836
### H0: És soroll blanc
Box.test(arima2$residuals)



/* La funció auto.arima ens dóna el mateix? 
a <-auto.arima(serie2)
a
checkresiduals(a)

### Com el coeficient de la ma1 i de la ma3 si construim l'interval de confiança amb el se inclou el 0
### Ens està dient que no és significatiu.
### Si els descartéssim ens donaria el model de l'estil Z(n) = Phi(1)*Z(n-1) + Theta(2)*Epsilon(n-2) + Epsilon

### En aquest cas els quatre models proposats podem dir que ajusten prou bé!!!


### Exercici 3
### Les equacions de Yule-Walker son si fos un AR(3):
### http://esfm.egormaximenko.com/students/Garcia_Leon_2015_ecuaciones_de_Yule_Walker.pdf pàgina 3
autocors <- as.numeric(acf(serie2, plot=F)[[1]])[1:3]
A <- matrix(c(1, autocors[1], autocors[2], autocors[1], 1, autocors[
1], autocors[2], autocors[1], 1), 3, 3)
A
b <- matrix(c(autocors[1], autocors[2], autocors[3]), 3, 1)
b
phis <- solve(A, b)
phis
arima2<-arima(serie2,c(3,0,0))
arima2
### Equivalentment,
ar.yw(serie2)



### Exercici 4
serie3 <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac6TS_3.txt", header=T)
ts.plot(serie3, col="darkblue",lwd=2)

### Test de Dickey-Fuller Aumentado “adf.test”: 
adf.test(serie3$x)
### Si rechazamos la hipotesis nula, rechazamos no estacionariedad


par(mfrow = c(2,1))
acf(serie3)
pacf(serie3)
### L'ACF sembla decreixer exponencialment, indicatiu d'un proces AR pero² l'ACF tambe ho sembla: Podria tractar-se d'un proces ARMA. Per estimar l'ordre, fem eacf():
### Función de autocorrelación extendida
eacf(serie3$x)
### L'eacf sembla indicar un proces ARMA(1,2)

arima3<-arima(serie3,c(1,0,2))
arima3
checkresiduals(arima1)
### Soroll blanc: https://rpubs.com/Meca/376836
### H0: És soroll blanc
Box.test(arima1$residuals)
### Rebutgem soroll blanc


# La funció auto.arima ens dóna el mateix? 
a <-auto.arima(serie3)
a
checkresiduals(a)





### Exercici 5

serie4 <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac6TS_4.txt", header=T)
head(serie4)
ts.plot(serie4$serie)    ### Es un proces clarament no estacionari perquè té tendencia

### Test de Dickey-Fuller Aumentado “adf.test”: 
adf.test(serie4$serie)
### Si rechazamos la hipotesis nula, rechazamos no estacionariedad

auto.arima(serie4$serie) ### auto.arima proposa un model amb diferencia de primer ordre (MA(1))

# Fem primeres diferencies
timeseries_diff1 <- diff(serie4$serie)
ts.plot(timeseries_diff1)    

### La primera diferencia si que hauria de ser estacionaria
adf.test(timeseries_diff1)   
### El test de Dickey-Fuller confirma que la serie diferenciada es estacionaria

### El model proposat per auto.arima ara es el mateix que abans sense necessitat de diferenciar (MA(1))
auto.arima(timeseries_diff1)

# Correlograma
acf(timeseries_diff1)
# Gràfic de les Correlacions parcials  
pacf(timeseries_diff1)

# Mirant l'ACF i PACF veiem que un bon candidat pot ser un model MA(1): 

arima5<-arima(timeseries_diff1,c(0,0,1))

checkresiduals(arima5)
### H0: És soroll blanc
Box.test(arima5$residuals)

 
### primer forecast basat en estructura MA(1)
auto.arima(timeseries_diff1)
delta <- 1.2186
v_eps <- 30.98
eps <- vector()
eps[1] <- rnorm(1, 0, sqrt(v_eps))
for (i in 2: length(timeseries_diff1))
{
  eps[i] <- timeseries_diff1[i]-delta+0.3794*eps[i-1]
}

x_50 <- delta-0.3794*eps[49]
### IC es pot construir tenint en compte que la distribuciÃ³ marginal de x_50 Ã©s normal
alpha <- 0.05
x_50-qnorm(1-alpha/2)*sqrt(v_eps)
x_50+qnorm(1-alpha/2)*sqrt(v_eps)

### Equivalentment, per la serie diferenciada fent servir la llibreria forecast
mod1 <- auto.arima(timeseries_diff1)
pred <- forecast(timeseries_diff1, h=1, model=mod1)
pred
plot(pred)

### Per treure la diferÃ¨ncia:
x_51 <- x_50 + serie4$serie[50]

### Equivalentment, fent servir la llibreria forecast
### https://rpubs.com/palominoM/series
mod1 <- auto.arima(serie4$serie)
pred <- forecast(serie4$serie, h=10, model=mod1)
pred
plot(pred)

pred <- forecast(serie4$serie, h=10, model=mod1, level = c(80, 95))
pred
plot(pred)

pred <- forecast(serie4$serie, h=10, model=mod1, level = c(80, 95), fan=TRUE)
pred
plot(pred)




### Desestacionalizar series temporales
### http://finanzaszone.com/desestacionalizar-series-temporales-con-r/

### Enlaces interesante: http://www.diegocalvo.es/analisis-de-series-temporales-en-r-arima/
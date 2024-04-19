

library(forecast)
library(TSA)
library(tscount)
library(tseries)
library(lmtest)

### Exercici 1
### Dades de crims anuals a Espanya 1994-2007
dades <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac10TS_1.txt", sep=";", header=T)
dades
## Com ve donada la s?rie
head(dades) 

# La convertim en una s?rie anual de classe ts
dades1.ts <- ts(dades$Crimes.recorded.by.the.police, start=1994, end=2007, freq=1)


### Apartat a)
## Dibuixem el gr?fic
ts.plot(dades1.ts) 
# No t? el dibuix d'una s?rie estacion?ria donat que no visualitzem 
# una mitjana constant ni una variaci? constant

## Test de Dickey-Fuller
adf.test(dades1.ts) 
### No ?s estacionari, el test ho confirma.

### Apartat b)
# D'acord amb el que s'observa al gr?fic, sembla tenir dues tend?ncies 
#(creixent fins al 2001 i decreixent de 2001 a 2007)


### Apartat c)
### No sembla tenir estacionalitat. 

### Per comprovar-ho, un truc basat en el likelihood ratio test:

### ETS: Modelos de suavizado exponencial 
### Molt interessant el seg?ent enlla?:
### https://otexts.com/fpp2/arima-ets.html

### Prova del log-likelihood per comparar dos models:
### https://en.wikipedia.org/wiki/Likelihood-ratio_test

fit1 <- ets(dades1.ts)
fit1
 
fit2 <- ets(dades1.ts, model="ZZN")
fit2

c(logLik(fit1) - logLik(fit2))

deviance <- 2*c(logLik(fit1) - logLik(fit2))
deviance
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df
#P value
1-pchisq(deviance,df) 
### La component estacional no ?s significativa.



### Apartat d) Quin ?s el model que proposa auto.arima()?
mod1 <- auto.arima(dades1.ts)
mod1
coeftest(mod1)
summary(mod1)

### El model que proposa S(t) = 1.3033 * S(t-1) - 0.5937 * S(t-2) + 127.539 +E(t) 
#### E(t) es un soroll blanc amb vari?ncia 109112137 

### Proposa una s?rie estacionaria, un AR(2).
mod2 <- arima(dades1.ts, order=c(2,0,0)) 
#Model proposat per auto.arima
mod2
summary(mod2)

### En base a l'ACF i PACF, pots proposar un model alternatiu m?s simple?
# Correlograma
par(mfrow=c(2,1))
acf(dades1.ts)
# Correlacions parcials
pacf(dades1.ts)


### Descartem fer l'eacf perqu? no tenir suficients dades: eacf(dades1.ts)

### L'ACF sembla tenir un pic important en el primer lag, indicatiu d'un proc?s AR 
### Per? no decau exponencialment cap a zero donat que tamb? presenta un segon pic en el lag 6 que pod?a ser indicatiu d'un proc?s no estacionari
# Podriem proposar un AR(1). De fet, 
# en el model proposat per l'auto-arima l'estimador del segon lag ?s no significatiu
mod2 <- arima(dades1.ts, order=c(1,0,0)) # Model raonable en base a ACF/PACF
mod2
coeftest(mod2)
mod1

### Apartat e)
AIC(mod1); AIC(mod2) ### El primer model ?s millor en termes d'AIC


### Apartat f)
par(mfrow=c(2,1))
pred1 <- forecast(subset(dades1.ts, start=1, end=12), h=2, model=mod1)
pred1
plot(pred1); 

Crimes <- dades$Crimes.recorded.by.the.police
Crimes
lines(x=seq(2006,2007,1), Crimes[13:14], col="red")
pred2 <- forecast(subset(dades1.ts, start=1, end=12), h=2, model=mod2)
plot(pred2); 
lines(x=seq(2006,2007,1), Crimes[13:14], col="red")

pred2
pred1

### Apartat g)
acf(residuals(mod1)); acf(residuals(mod2))

residuals(mod1)

tsdiag(mod1); 
tsdiag(mod2)
checkresiduals(mod1); 
checkresiduals(mod2)
###n apartat h) En termes de predicci? i residus els dos models s?n bastant limitats, el segon encara sembla una mica millor.


### Soroll blanc: https://rpubs.com/Meca/376836
### H0: ?s soroll blanc
Box.test(residuals(mod1))
Box.test(residuals(mod2))
### Acceptem soroll blanc

acf(residuals(mod1),lag.max=140)
#install.packages("FitAR")
library(FitAR)
boxresult=LjungBoxTest(residuals(mod1),k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
qqnorm(residuals(mod2))
qqline(residuals(mod2))


### Exercici 2

dades2 <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac10TS_2.txt", sep=";", header=T)
dades2
## Com ve donada la s?rie
head(dades2) 

### Apartat a)
## Dibuixem el gr?fic
par(mfrow=c(1,1))
ts.plot(dades2$x) 
### Sembla un proc?s estacionari, sense tendencia ni estacionalitat

## Test de Dickey-Fuller
adf.test(dades2$x) 
### ?s estacionari, el test ho confirma.

### Apartat b) i c)
No sembla tenir tend?ncia ni estacionalitat

### Per comprovar-ho, un truc basat en el likelihood ratio test
fit1 <- ets(dades2$x)
fit1
fit2 <- ets(dades2$x, model="ZZN")
fit2
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df
df 
#P value
1-pchisq(deviance,df) ### La component estacional no ?s significativa
?ets
### Apartat d) Quin ?s el model que proposa auto.arima()?
mod2_1 <- auto.arima(dades2$x)
mod2_1 
coeftest(mod2_1)
### Proposa una s?rie estacionaria, un ARMA(2,2).
mod2_2 <- arima(dades2$x, order=c(3,0,1), include.mean = FALSE) # Model proposat per auto.arima
mod2_2
coeftest(mod2_2)

AIC(mod2_1); AIC(mod2_2)

?arima

### En base a l'ACF i PACF, pots proposar un model alternatiu m?s simple?
# Correlograma
par(mfrow=c(2,1))
acf(dades2$x)
# Correlacions parcials
pacf(dades2$x)


### Tant ACF com PACF semblen decr?ixer exponencialment, sembla un ARMA. 
### Per? amb quins par?metres?

### Propuesta del modelo de autocorrelaci?n extendida
eacf(dades2$x) 
### Sembla un ARMA(2,2)


checkresiduals(mod2_1);

### Apartat f)
dev.off()
pred2 <- forecast(dades2$x[1:9980], h=20, model=mod2_1)
pred2
plot(pred2); 
lines(seq(9981,10000,1), dades2$x[9981:10000], col="red") 
### No es veu res, cal pintar nom?s la part final que ?s el que ens interessa


pred3 <- as.data.frame(pred2)
pred3
plot(pred3[, 1], type="l", ylim=c(-5,5)); 
lines(dades2$x[9981:10000], col="red") 
### Sembla que la predicci? no ?s bona

### Pintem els intervals de confian?a del 95%
plot(pred3[, 4], type="l", ylim=c(-5,5)); 
lines(dades2$x[9981:10000], col="red"); 
lines(pred3[, 5], col="black")
lines(pred3[,1], col="blue")  

### Doncs en realitat no est?? malament, tots els valors cauen dins l'interval!

### Comptem en quants casos el valor real est??fora de l'interval de confian?a del 95%:
df <- data.frame(real=dades2$x[9981:10000], lower=pred2$lower[, 2], pred=pred2$mean, upper=pred2$upper[, 2])
df 

### Creem una nova variable que indica si l'observaci? est? fora de l'interval 
df$fora <- ifelse(df$real >= df$lower & df$real <= df$upper, 0, 1) 
df$fora
### I comptem
table(df$fora)

### Veiem que totes les observacions cauen dins de l'interval del 95%.
### S?, sembla que crea bones prediccions!!!


### Apartat f)
# Correlograma dels residus
par(mfrow=c(2,1))
acf(residuals(mod2_1))
# Correlacions parcials
pacf(residuals(mod2_1))

tsdiag(mod1);
#### El test de Ljung-Box nos hace el contraste H0: independientes (correlaci?n = 0)

checkresiduals(mod2_1); 

### Soroll blanc: https://rpubs.com/Meca/376836
### H0: ?s soroll blanc
Box.test(residuals(mod2_1))
### Acceptem soroll blanc

## Q-QPlot
dev.off()
qqnorm(residuals(mod2_1))
qqline(residuals(mod2_1))



### Exercici 3
### Nombre mensual de morts en accident de tr?nsit a Gran Bretanya ( 1969-1984)

### Apartat a)
dades3 <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac10TS_3.txt", sep=";", header=TRUE)
head(dades3)

dades3.ts <- ts(dades3$DriversKilled, start=c(1969, 1), end=c(1984, 12), freq=12)
head(dades3.ts)

dev.off()
ts.plot(dades3.ts)

## Test de Dickey-Fuller
adf.test(dades3$DriversKilled) 
### El test ens diu que ?s estacionari.


### Apartat b)

### T? tend?ncia? 

dades3D<-dades3$DriversKilled
dades3D
t= 1:192
t
dades3D.lm=lm(dades3D~t)
summary(dades3D.lm)
#El coefficient que acompanya a la variable independent t surt significativa i t? valor negatiu;  



#### I estacionaliat? Confirmem amb el truc anterior.
fit1 <- ets(dades3.ts)
fit2 <- ets(dades3.ts, model="AZN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#P value
1-pchisq(deviance,df) 
### H0: No estacional / H1: Estacional
### La component estacional ?s significativa per al nombre de morts. 

dades3D<-dades3$DriversKilled
dades3D
t= 1:192
t
dades3D.lm=lm(dades3D~t)
summary(dades3D.lm)

dades3

### Apartat c)

### Fem la regressi?
dades3D.lm=lm(dades3$DriversKilled~dades3$law)
summary(dades3D.lm)


### Fem-ho amb una auto.arima introduint la variable law com a regressor
mod1 <- auto.arima(dades3.ts, xreg=dades3$law) ### Model amb la llei com a covariable.
mod1
coeftest(mod1)

mod2 <- auto.arima(dades3.ts, xreg=dades3$law,allowdrift = FALSE) ### Model amb la llei com a covariable.
mod2
coeftest(mod2)

### Generem un interval de confiança del 95% per l'efecte de l
a llei:
#### -18.0647-qnorm(0.975)*8.9045; -18.0647+qnorm(0.975)*8.9045 
-19.5561-qnorm(0.975)*7.6754; -19.5561+qnorm(0.975)*7.6754 

### Hi ha un efecte significatiu de la llei


### Apartat d)

dades3.ts
length(dades3$DriversKilled)

mod2 <- auto.arima(subset(dades3.ts, start=1, end=180), xreg=dades3$law[1:180])
mod2

### Ajustem el model per a les dades de 1969-1983
pred <- forecast(mod2, xreg=dades3$law[181:192], h=12)
plot(pred); lines(x=seq(1984, 1984.99, 1/12), dades3$DriversKilled[181:192], col="red") 
### La predicci? sembla molt bona, excepte al darrer punt.

### Comptem en quants casos el valor real est??fora de l'interval de confian?a del 95%:
df <- data.frame(real=dades3$DriversKilled[181:192], lower=pred$lower[, 2], pred=pred$mean, upper=pred$upper[, 2])
df 

## Creem una nova variable que indica si l'observaci? est? fora de l'interval 
df$fora <- ifelse(df$real >= df$lower & df$real <= df$upper, 0, 1) 
df$fora
### I comptem
table(df$fora)




### Apartat e)
# Correlograma dels residus
par(mfrow=c(2,1))
acf(residuals(mod2))
# Correlacions parcials
pacf(residuals(mod2))

tsdiag(mod2);
#### El test de Ljung-Box nos hace el contraste H0: independientes (correlaci?n = 0)

checkresiduals(mod2) 

### La bondat de l'ajust ?s raonable, tenint en compte els residus s?n horrorosos.

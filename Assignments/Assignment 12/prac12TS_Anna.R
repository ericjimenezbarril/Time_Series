library(forecast)
library(tseries)
library(TSA)
library(fGarch)
library(lmtest)

### Pr?ctica 1


setwd("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 12 (281123)/")
dstORIG  <- read.table("dst.txt",  sep=";",  header=T)

head(dstORIG)

### Apartat a)
ts.plot(dstORIG$x)

## Test de Dickey-Fuller
adf.test(dstORIG$x) 
### Es estacionaria segons el test de Dickey-Fuller 


### Apartat b)
#### No sembla tenir tend?ncia 

### Apartat c)
###  ni estacionalitat clara, 

### Per comprovar-ho, un truc basat en el likelihood ratio test
fit1 <- ets(dstORIG$x)
fit2 <- ets(dstORIG$x, model="AZN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#P value
1-pchisq(deviance,df) ### H0: No ?s estacional / H1: ?s estacional
### La component estacional no ?s significativa.

### Apartat d)

### En base a l'ACF i PACF
# Correlograma
par(mfrow=c(2,1))
acf(dstORIG$x)
# Correlacions parcials
pacf(dstORIG$x)

### En base a l'eacf
eacf(dstORIG$x)

### Apartat e)
mod_dst1 <- auto.arima(dstORIG$x) 
mod_dst1

checkresiduals(mod_dst1)
Box.test(residuals(mod_dst1), type="Ljung-Box")
### Els residus s?n independents
auto.arima(residuals(mod_dst1)

### Apartat e)

### Els residus quadrats no son independents!
Box.test(residuals(mod_dst1)^2, type="Ljung-Box") 

# Correlograma
par(mfrow=c(2,1))
acf(residuals(mod_dst1)^2)
# Correlacions parcials
pacf(residuals(mod_dst1)^2)

eacf(residuals(mod_dst1)^2)

### Apartat f)
###Ajustem GARCH

### Primer que ho ajusti la funci? garch de R directament
mod_dst2 <- garch(dstORIG$x)
mod_dst2
mod_dst2_1 <- garchFit(~garch(1, 1), data=dstORIG$x)
mod_dst2_1

### El model que proposa ?s un GARCH(1,1)
#### Model: Y(t) = -0.01004 + E(t) on E(t) segueix una normal amb esperan?a 0 i vari?ncia sigma^2(t)
#### on sigma^2(t) = 0.02794 + 0.6778 * E(t-1) + 0.000000001 * sigma^2(t-1)

### Apartat g)
mod_dst3_1 <- garchFit(~garch(1, 0), data=dstORIG$x)
mod_dst3_1

### Comparamos los dos AICs
mod_dst2_a <- garch(dstORIG$x, order=c(1, 1))
mod_dst2_a

mod_dst2_b <- garch(dstORIG$x, order=c(0, 1))
mod_dst2_b

AIC(mod_dst2_a)
AIC(mod_dst2_b)

### el millor model ?s el GARCH(1,0) que el model GARCH(1,1)


### Apartat h)
###
pred <- predict(mod_dst2_b)
pred
par(mfrow=c(1,1))
ts.plot(dstORIG$x)
lines(seq(1, length(dstORIG$x), 1), pred[, 1], col="red")
lines(seq(1, length(dstORIG$x), 1), pred[, 2], col="red")

### Comptem en quants casos el valor real est??fora de l'interval de confian?a del 95%:
df <- data.frame(real=dstORIG$x, lower=pred[, 2], upper=pred[, 1])

## Creem una nova variable que indica si l'observaci? est? fora de l'interval 
df$fora <- ifelse(df$real >= df$lower & df$real <= df$upper, 0, 1) 

head(df)

### I comptem
table(df$fora)

### Un 38% dels punts se'n van fora de l'interval del 95%.

Box.test(residuals(mod_dst2_b), type="Ljung-Box") 
Box.test(residuals(mod_dst2_b)^2, type="Ljung-Box") 

par(mfrow=c(2,2))
acf(residuals(mod2)[2:length(residuals(mod2))]); 
pacf(residuals(mod2)[2:length(residuals(mod2))])

acf(residuals(mod2)[2:length(residuals(mod2))]^2); 
pacf(residuals(mod2)[2:length(residuals(mod2))]^2)



### Pr?ctica 3


setwd("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 12 (281123)/")
prc2 <- read.table("PRC2_data2.csv", sep=";", header=T)

head(prc2)

### Apartat a)
par(mfrow=c(1,1))
ts.plot(prc2$serie2)

t= 1:length(prc2$serie2)
t
prc2.lm=lm(prc2$serie2~t)
summary(prc2.lm)

beta=prc2.lm$coefficients
beta

ts.plot(prc2$serie2)
lines(t,beta[1]+beta[2]*t,col="blue",lwd=2)

MediaMovil = ma(prc2$serie2, order = 12, centre = T)
lines(MediaMovil,col="pink",lwd=2)
### lines(MediaMovil,col="darkcyan",lwd=2)

MediaMovil3 = ma(prc2$serie2, order = 33, centre = T)
lines(MediaMovil3,col="red",lwd=2)
 

### Apartat b)
### Per comprovar-ho, un truc basat en el likelihood ratio test
fit1 <- ets(prc2$serie2)
fit2 <- ets(prc2$serie2, model="AZN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#P value
1-pchisq(deviance,df) ### H0: No ?s estacional / H1: ?s estacional
### La component estacional no ?s significativa.

### Apartat c)

#### Un cam? seria diferenciar per treure la tend?ncia 
### i veure si el resultat ?s estacionari
### La funci? auto.arima ens pot donar una idea:

### Resultat diferenciant la serie
mod_autoarima = auto.arima(prc2$serie2)
mod_autoarima
coeftest(mod_autoarima)

### Resultat restant la regressi? a la serie original i la serie obtinguda li busquem una serie ARMA
mod_reg <- auto.arima(prc2$serie2, xreg=t) ### Model amb el temps com a covariable.
mod_reg
coeftest(mod_reg)

### Mirem que els residus i els residus al quadrat no tinguin estructura

Box.test(residuals(mod_autoarima), type="Ljung-Box") 
Box.test(residuals(mod_autoarima)^2, type="Ljung-Box") 

par(mfrow=c(2,2))
acf(residuals(mod_autoarima)[2:length(residuals(mod_autoarima))]); 
pacf(residuals(mod_autoarima)[2:length(residuals(mod_autoarima))])

acf(residuals(mod_autoarima)[2:length(residuals(mod_autoarima))]^2); 
pacf(residuals(mod_autoarima)[2:length(residuals(mod_autoarima))]^2)

### Apartat d)
pred1 <- forecast(prc2$serie2, h=30, model=mod_autoarima)
pred1
par(mfrow=c(1,1))
plot(pred1);

length(prc2$serie2)



a= prc2$serie2
head(a)
train= a[1:170]
mod_autoarima2 = auto.arima(train)
mod_autoarima2

pred2 <- forecast(train, h=30, model=mod_autoarima2)
pred2
plot(pred2);
lines(a, col="red");







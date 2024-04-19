library(forecast)
library(TSA)
library(tscount)
library(tseries)
library(lmtest)


## EXERCICI 1
data1 <- read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 10(141123)/prac10TS_1.txt", sep=";", header=T)
data1
data1.ts <- ts(data1$Crimes.recorded.by.the.police, start=1994, end=2007, freq=1)

#(a) DIbujo + ¿es estacionariio? + ADF
plot(data1.ts)

#No es un modelo estacionario. Se observan dos tendencias claras (una creciente y una decreciente)

#Veamoslo con el test adf
adf.test(data1.ts)

#el pvalor es mayor que 0.05 por lo que no descartamos la hipótesi nula de que tenga una raíz unitaria.

#Observamos que tiene dos tendencias.


#(c) A priori no parece tener estacionalidad

### No sembla tenir estacionalitat. 

### Per comprovar-ho, un truc basat en el likelihood ratio test:

### ETS: Modelos de suavizado exponencial 
### Molt interessant el seg?ent enlla?:
### https://otexts.com/fpp2/arima-ets.html

### Prova del log-likelihood per comparar dos models:
### https://en.wikipedia.org/wiki/Likelihood-ratio_test


fit1 <- ets(data1.ts)
fit1

fit2 <- ets(data1.ts, model="ZZN")
fit2

c(logLik(fit1) - logLik(fit2))

deviance <- 2*c(logLik(fit1) - logLik(fit2))
deviance
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df
#P value
1-pchisq(deviance,df) 
### La component estacional no ?s significativa.

#Miremos la tendencia con esto
fit3 <- ets(data1.ts, model="ZNZ")
c(logLik(fit1) - logLik(fit3))

deviance <- 2*c(logLik(fit1) - logLik(fit3))
deviance
df <- attributes(logLik(fit1))$df - attributes(logLik(fit3))$df
#P value
1-pchisq(deviance,df) 

### Apartat d) Quin ?s el model que proposa auto.arima()?
mod1 <- auto.arima(data1.ts)
mod1
coeftest(mod1)
summary(mod1)

### El model que proposa S(t) = 1.3033 * S(t-1) - 0.5937 * S(t-2) + 127.539 +E(t) 
#### E(t) es un soroll blanc amb vari?ncia 109112137 


### En base a l'ACF i PACF, pots proposar un model alternatiu m?s simple?
# Correlograma
par(mfrow=c(2,1))
acf(data1.ts)
# Correlacions parcials
pacf(data1.ts)


### Descartem fer l'eacf perqu? no tenir suficients dades: eacf(dades1.ts)

# Podriem proposar un AR(1). De fet, 
# en el model proposat per l'auto-arima l'estimador del segon lag ?s no significatiu
mod2 <- arima(data1.ts, order=c(1,0,0)) # Model raonable en base a ACF/PACF
mod2
coeftest(mod2)
mod1

### Apartat e)
AIC(mod1); AIC(mod2) ### El primer model ?s millor en termes d'AIC


### Apartat f)
par(mfrow=c(2,1))
pred1 <- forecast(subset(data1.ts, start=1, end=12), h=2, model=mod1)
pred1
plot(pred1); 

Crimes <- data1$Crimes.recorded.by.the.police
Crimes
lines(x=seq(2006,2007,1), Crimes[13:14], col="red")
pred2 <- forecast(subset(data1.ts, start=1, end=12), h=2, model=mod2)
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
qqnorm(residuals(mod2))
qqline(residuals(mod2))


## EXERCICI 2
dades2<-read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 10(141123)/prac10TS_2.txt", sep=";", header=T)
dades2
## Com ve donada la s?rie
head(dades2) 

#(a) Dibujar serie, estacionario, xk? adf.test
plot.ts(dades2)

#A priori, qualitativamente parece estacionario, con media cero y varianza 6
da
#adf-test
adf.test(dades2$x)
#Se rechaza la H0 por lo que a priori parece estacionario,.


#(b) Tendencia?
#A priori no parece tener tendencia
#Hagamos la regresión para verlo.
t=1:length(dades2$x)
dades2.lm=lm(dades2$x ~ t)
summary(dades2.lm)
#Hay una cierta tendencia negativa, pero prácticamente nula


#Veamos la tendencia y estacionalidad basándonos en el LRT
fit1 <- ets(dades2$x)
fit1
fit2 <- ets(dades2$x, model="ZZN")
fit2
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df
df 
#P value
1-pchisq(deviance,df) ### La component estacional no ?s significativa
#Descartem estacionalitat.

#Miremos la tendencia
fit3 <- ets(dades2$x, model="ZNZ")
fit3
deviance <- 2*c(logLik(fit1) - logLik(fit3))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit3))$df
df 
#P value
1-pchisq(deviance,df) ### La component estacional no ?s significativa
#Descartem tendencia.

#NO HACER MUCHO CASO A ESTO, DUDO QUE ESTÉ BIEN

#(d) ACF, PACF. mira que propone
par(mfrow=c(1,1))
acf(dades2$x)
pacf(dades2$x) #No parece ser ni AR ni MA, es un modelo ARMA

#Miremos auto.arima
mod21<-auto.arima(dades2$x)

#Propone un modelo ARMA(2,2)

#(f) Analizad los residuos generados por el modelo usando checkresiduals().
checkresiduals(mod21)
#Parece un buen modelo
acf(mod21$residuals)
pacf(mod21$residuals) #parecen ruido blanco

#(e) Generad prediccón para los 20 últimos valores de la serie.
#Són buenas 

dev.off()
pred2 <- forecast(dades2$x[1:9980], h=20, model=mod21)
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


##Exercici 3
# Bondad de ajuste y selección de modelo (II)

data3 <- read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 10(141123)/prac10TS_3.txt", sep=";", header=T)
data3
#Corresponde al número mensual de muertes en accidentes de tráfico en Gran Bretaña entre 1969-1984

#(a) Dibujad la serie. ¿Es un proceso estacionario? ¿Por qué? Prueba el test de Dickey-Fuller.
data3.kills <- data3$DriversKilled
data3.ts<-ts(data3.kills, start=as.yearmon(as.Date("1969-01-01")), end=as.yearmon(as.Date("1984-12-01")),frequency = 12)
plot(data3.ts)

plot(data3.ts)
#En total hay 16 años
#No parece una serie estacionaria.Parece haber una cierta tendencia decreciente además de cierta estacionalidad.

#Hagamos el Test de Dickey-Fuller
adf.test(data3.ts)
#Obtenemos un pvalor menor que 0.01 por lo que rechazamos la hipótesi nula de que el proceso sea estacionario.

#(b) Tiene tendencia?
#Hagamos la recta de regresión para ver si tiene tendencia
t<- 1:length(data3.ts)
data3.lm=lm(as.vector(data3.ts) ~ t)
summary(data3.lm)
#Notamos que la tendencia es significativamente diferente de 0.
#Tiene tendencia decreciente.
plot(decompose(data3.ts)$trend)
#Se observa una tendencia decreciente.



#(c) Veamos la estacionalidad.

#En primer lugar miramos el decompose
plot(decompose(data3.ts)$seasonal)

#Hay una estacionalidad clara con periodo anual

#Observémoslo con los boxplot
matrix_data=matrix(data=as.vector(data3.ts), nrow=12)
matrix_data_t=t(matrix_data)
boxplot(matrix_data_t, notch=TRUE)  #Se observa una clara estacionalidad

#Veamos con otros periodos
matrix_data2=matrix(data=as.vector(data3.ts), nrow=6)
matrix_data_t2=t(matrix_data2)
boxplot(matrix_data_t2, notch=TRUE) #No se ve un patron tan claro


#Miramos además que
fit1 <- ets(data3.ts)
fit1
fit2 <- ets(data3.ts, model="ZZN")
fit2
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df
df 
#P value
1-pchisq(deviance,df) ### La component estacional no ?s significativa
#No podem descartar la estacionalitat.


#(d) Ajustad un modelo que permita determinar si la let que obliga a usar cinturón
# de seguridad influye de manera considerable en el número de muertres de accidentes de tráfico.
#Hagamos en primer lugar una regresion
data3d.lm=lm(data3$DriversKilled ~ data3$law)
summary(data3d.lm)

#Si que depende del cinturón.
#Hagamos un modelo auto.arima introduciendo la variable law como regresor
mod31 <- auto.arima(data3.ts, xreg=data3$law) #Modelo con la ley como variable
mod31

#La ecuación del modelo se le une un \beta X_t (ver foto)
#Generamos un intervalo de confianza 95% para el efecto de la lei

coeftest(mod31)
#Obtenemos un pvalor de 0.01 por lo que descartamos la hipótesi nula, es diferrente de 0 por tanto tiene un efecto significativo

#(e) Generad preidcciones correspondientes al último año 1984 en base al modelo 
# ajustado en el punto anterior. Dibujad las predicciones generadas junto con los valores observados
data3.ts
length(data3$DriversKilled)

mod32 <- auto.arima(subset(data3.ts, start=1, end=180), xreg=data3$law[1:180])
mod32

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

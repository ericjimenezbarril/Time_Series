# PRÁCTICA 11: ARCH Y GARCH
library(forecast)
library(TSA)
library(tscount)
library(tseries)
library(lmtest)
library(fGarch)

#Producto interior bruto EEUU por trimestres de 1947 a 2001
data <- read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 11(211123)/prac11TS.txt", header=TRUE)
data.ts <- ts(data$x, start=c(1947,1), end=c(2001,4), frequency =4)
data.ts
plot(data.ts)

#A priori no parece que la volatilidad sea constante por lo que no es estacionaria
#Hacemos el test de Dickey-Fuller

adf.test(data.ts) #A priori dice que es estacionaria
#pero cualitativamente vemos que no lo es

#Veamos la tendencia
#A priori cualitativamente no tiene tendencia
#Hagamos una regresión
t <- 1:length(data.ts)
data.lm<-lm(data$x ~ t)
data.lm #Veamos si es significativo
coeftest(data.lm) #Observamos que el coeficiente de la t no es significativo por lo que 
#no podemos rechazar la hipótesi nula de que sea 0

#Por tanto conluimos que no tiene teendencia.


#Veamos la estacionalidad
#En caso de tener, seria de orden 4
matrix_data=matrix(data=data$x[1:220], nrow=4)
matrix_data_t=t(matrix_data)

boxplot(matrix_data_t, notch=TRUE) 

#Podemos verlo si no con el decompose
decompose(data.ts)$seasonal
plot(decompose(data.ts)$seasonal) #Vemos que la estacionalidad que nos da es de +/- 0.3
#y los datos son de 2 ordenes de magnitud mayor, por lo que podemos considerar que no hay estacionalidad
plot(decompose(data.ts))
#La tendencia que nos da es devia a que la volatilidad (variancia) no es constante


#Si concluimos que no hay tendencia ni estacionalidad, es que el problema no está en la media
# por lo que el modelo no es estacionario a causa de la varianza

mod1<-auto.arima(data.ts)
mod1 #ARMA(1,3) con media diferente de 0
#AIC=888.29, BIC=908.65, AICc=888.68
#Veamos si los coeficientes son significativos
coeftest(mod1) #Except 1 de ellos, son significativos

par(mfrow=c(2,1))
acf(data.ts)
pacf(data.ts)
eacf(data.ts)
# En base a estas 3 gráficas no podemos deducir gran cosa, parecen de un modelo no estacionario
# No proponemos modelo

#Fijamos eso si un modelo con la ma2 fijada en 0 (como nos sugiere coeftest())
mod2 <- arima(data.ts, order=c(1,0,3), fixed=c(NA, NA, 0, NA, NA) ) # Model proposat per auto.arima
mod2; BIC(mod2)
#AIC= 884.66, BIC=903.6287 #el modelo mejora

#proponemos este segundo modelo mod2

checkresiduals(mod2)
tsdiag(mod2)
#Los residuos parecen ruido blanco,

shapiro.test(mod2$residuals)
#Rechazamos la hipótesi nula de que sean normales pero a priori son WN


#Analizamos los residuos al cuadrado de la serie
Box.test(residuals(mod2)^2, type="Ljung-Box") 
#Rechazamos la hipótesi nula de que sean no correlacionados, es decir, estan correlacionados
# Correlograma
par(mfrow=c(2,1))
acf(residuals(mod2)^2)
# Correlacions parcials
pacf(residuals(mod2)^2)
#No parecen ser WN, por lo que deberíamos ajustar un modelo GARCH

mod3 <- garch(data.ts)

mod4 <- garchFit(~garch(1, 1), data=data.ts)
# omega = 0.58111508 , alpha1=0.72553301 , beta1= 0.02279883 
summary(mod4) #AIC=3.191985, BIC=3.253687 

mod5 <- garchFit(~garch(1, 0), data=data.ts)
summary(mod5)
#AIC= 3.183, BIC=3.229 (es algo mejor)

mod5 <- garchFit(~garch(2, 0), data=data.ts)
summary(mod5) #AIC= 3.189712, BIC= 3.251415 
#es peor que el anterior y además tiene más coeficientes

mod6 <- garchFit(~garch(2, 1), data=data.ts)
summary(mod6) #AIC= 3.198803 , BIC= 3.275931 
#Se observa que contra más aumentamos los coeficientes empeoran los modelos


# El mejor modelo con los AIC es el GARCH(1,0) es decir, un ARCH(1)
mod_bo <- garch(data$x, order=c(0, 1))
# Utilizando la función predict() de tseries,, dibujad un intervalo de confianza para 
# las estimaciones en todo el periodo juntamente con las observaciones reales.
# ¿ Que´podéis decir sobre la bondad del modelo?
pred <- predict(mod_bo)
par(mfrow=c(1,1))
length(pred[,1])
plot(data.ts)
lines(seq(1947, 2001.75, 0.25), pred[, 1], col="red")
lines(seq(1947, 2001.75, 0.25), pred[, 2], col="red")

#Notem que hi ha més d'un valor que se surt dels intervals de confianza
#Veiem quants
df <- data.frame(real=data$x, lower=pred[, 2], upper=pred[, 1])
head(df) 

## Creem una nova variable que indica si l'observaci? est? fora de l'interval 
df$fora <- ifelse(df$real >= df$lower & df$real <= df$upper, 0, 1) 
head(df$fora)
### I comptem
table(df$fora)
79/(79+140) #El 36% estan fora


#(l) Una serie en que els residus al cuadrat no presentin estructura.
prova <- arima.sim(model=list(ar=0.7), n=10000) ### AR(1) simulat
mod.prova <- auto.arima(prova)
mod.prova
coeftest(mod.prova)
Box.test(residuals(mod.prova), type="Ljung-Box")
Box.test(residuals(mod.prova)^2, type="Ljung-Box")

par(mfrow=c(1,2))
acf(residuals(mod.prova)^2); pacf(residuals(mod.prova)^2)
#No presentan estructura.


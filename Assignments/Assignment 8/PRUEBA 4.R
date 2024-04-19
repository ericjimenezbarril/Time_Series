### PRÁCTICA SERIES TEMPORALES ###
require(zoo)
library(tseries)
library(lmtest)
library(forecast)
library(TSA)
library(stats)
par(mfrow=c(1,1))
datos<-read.table(file = "C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/LLIURAMENT/data.txt", header = FALSE, dec=",")
datos                               ## me aparecen en orden inverso
datos<-datos$V1
data <- rev(as.numeric(datos)) #Los datos venían al revés en orden cronológico

start=as.yearmon(as.Date("2000-01-01"))
end=as.yearmon(as.Date("2023-04-01"))

data.ts=ts(data,start=start, end=end, frequency = 4)
data.ts
plot(data.ts)
points(data.ts)
h=c(1300, 1467, 1634, 1800, 1967, 2134, 2301, 2467, 2634, 2801)
abline(h=h)

len=length(data.ts)

#Se observa una clara estacionalidad anual (s=4)

# Por tanto en primer lugar hacemos la diferenciación por trimestre
data.df<-diff(data.ts, lag=4)

plot(data.df)

#Realmente no se observa una tendencia clara de los datos  una vez diferenciados.

#Notemos que 
t=1:length(data.df)
data.df.lm<-lm(unclass(data.df) ~t) 
plot(t, unclass(data.df))
abline(data.df.lm, col="blue", lwd=2)

#Existe una pequeña tendencia decreciente de los datos una vez diferenciados 4 veces

#Veamos si los datos diferenciados son estacionarios
adf.test(data.df, alternative="stationary")
#Obtenemos un pvalor=0.6795 por tanto no es estacionaria.

kpss.test(data.df)       #Obtenim un pvalor de 0.05676 por tanto no podemos rechazar la hipótesi nula de que la serie sea estacionaria en niveles.

#Tomamos por tanto la primera diferencia
data.df2 <- diff(data.df)

plot(data.df2) #Observamos que ahora puede que la media sea fija pero no lo es la varianza

adf.test(data.df2, alternative = "stationary")
#Pero rechazamos la hipótesi nula por tanto son estacionarios los datos,

#Por tanto podemo estudiar un modelo para estos
par(mfrow=c(2,1))
acf(data.df2)
pacf(data.df2)  #No queda del todo claro a que modelo se ajusta.

eacf(data.df2) #Podría tratarse de un ARIMA(0,1,0)(1,1,1)


#Veamos que nos propone la función auto.arima
(fit1<-auto.arima(data.ts))
#Propone un modelo ARIMA(0,1,1)(1,1,0)[4]
#AIC=936.63   y BIC=944.1, s2=2049


#Provemos el otro
fit2<-arima(data.ts, order=c(0,1,0), seasonal=list(order=c(1,1,1), period=4))
fit2$sigma2
#Obtenemos un AIC=947.87 y BIC= 957.3331 y s2=2316.905 es peor modelo y usa las mismas variables

#Probamos un
fit3<-arima(data.ts, order=c(0,1,1), seasonal=list(order=c(0,1,0), period=4))
fit3$sigma2
#AIC=960.22   y BIC=967.1929, s2=2765.962

fit4<-arima(data.ts, order=c(0,1,1), seasonal=list(order=c(1,1,1), period=4))
BIC(fit4)
#AIC= 935.42, BIC =947.3766, sigma2= 1972

#Es bastante mejir aunque use una variable más.
#Veamos los residuos de fit1 y fit4

checkresiduals(fit1)
#Tiene un pvalor de 0.6771 por lo que no rechazamos la hipótesi nula de que los datos son independientes

#Veamos si tienen media 0
t.test(fit1$residuals) #Tenemos un pvalor = 0.6146 por lo que no podemos rechazar la hipótesi nula de que la media es 0

shapiro.test(fit1$residuals)
#No obstante no son normales  --> El t.test NO ES validdo



#Veamos para el modelo 4
checkresiduals(fit4) #De nuevo los residuos son independientes

t.test(fit4$residuals) #No podemos rechazar la hipótesi nula de que la media es 0

shapiro.test(fit4$residuals)
#Pero no se distribuyen con normalidad   -> El t.test NO ES validdo




### Hagamos una predicción con nuestro modelo de los últimos datos para ver si se ajustan a los reales
par(mfrow=c(1,1))
train_serie <- data.ts[1:60]
pred1 <- forecast(train_serie, h=34, model=fit1)
pred1
plot(pred1)
### Pintem a la gr?fica els valors correctes
lines(seq(60, 94, 1), data.ts[60:94], col="red")

#Estan todos dentro del intervalo de confianza dde predicción

#Veamos con el otro modelo

train_serie <- data.ts[1:60]
pred2 <- forecast(train_serie, h=34, model=fit4)
pred2
plot(pred2)
### Pintem a la gr?fica els valors correctes
lines(seq(60, 94, 1), data.ts[60:94], col="red")

#No detectan la última subida.















#Lo que podemos ver es si se ajusta mejor a una regresión + modelo para la componente estacional.
plot(data.ts)
t=1:94
reg_mod<-lm(data~t)
plot(t,data)
abline(reg_mod, col="blue", lwd=2)

reg_mod    
itcp= 1776.960
a_t= 8.368  
#Ahora el modelo para la componente estacional se puede ajustar como
fit_est<-arima(data.ts, order=c(0,0,0), seasonal=list(order=c(1,1,0), period=4))


# Veamos como estima la serie una mezcla de ambos
train_est <- data.ts[1:60]
pred_est <- forecast(train_serie, h=34, model=fit_est)
v.pred_est<-c(pred_est$mean)
t=61:94
pred= v.pred_est + a_t*t
plot(1:94, data)
lines(61:94, pred)



# SE QUE LO ESTOY HACIENDO MAL, NO HACE NADA.


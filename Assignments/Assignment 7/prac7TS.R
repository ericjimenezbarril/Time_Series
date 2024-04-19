##### Componente Estacional /Predicción
library(lmtest)
library(zoo)
library(forecast)
library(TSA)
library(tseries)
library(stats)
#Práctica 1.1
data("AirPassengers")
AirPassengers
plot(AirPassengers)

#No parece ser un proceso estacionario pues la media en particular, la media no es cosntante 
# (se aprecia una cierta tendencia y estacionalidad en los datos) por lo que no es un proceso estacionario

#Se observa una clara tendencia creciente y además la estacionalidad es anual con los picos más bajos en
#enero y noviembre y los más altos  en ulio y agosto
plot(decompose(AirPassengers))

#Para aplicar el test tenemos que quitar la parte estacional del proceso, es decir, hay que diferenciarlo
data_df=diff(AirPassengers, lag=12)
plot(data_df)

#Veamos si los datos diferenciados son un proceso estacionario
adf.test(data_df, alternative = "stationary") 
#Obtenemos un pvalor mayor que 0.05 por lo que los datos no podemos rechazar la hipótesi nula y por tanto NO son estacionaris.


# Cuál es la estacionalidad que propone la función auto.arima?
fit1<-auto.arima(AirPassengers)
fit1 #Modelo ARIMA(2,1,1)(0,1,0)[12], AIC=1017.85, BIC=1029.35



#Hagamos nosotros el proceso que sigue el auto.arima

#Diferenciamos de nuevo la serie de orden 1
data_df2 = diff(data_df, lag=1)

#Veamos que ahora es estacionaria
adf.test(data_df2, alternative = "stationary")
#Ahora el pvalor es menor que 0,01 por tanto rechazamso la hipótesi nula por lo que el modelo es estacionario,


par(mfrow=c(2,1))
acf(data_df2)
pacf(data_df2)
eacf(data_df2)   #No nos da mucha información sobre que modelo podría ser

fit2<-auto.arima(data_df2)
fit2                  #Obtenemos en efecto un modelo ARIMA(2,0,1) con AIC=1017.85 y BIC=1029.35 (igual que antes)
coeftest(fit2)
#Notamos quee los coeficientes son significativos todos.

#Veamos si un modelo más simple ajusta mejor nuestros datos
fit3<-arima(data_df2, c(1,0,0), include.mean = FALSE)
BIC(fit3)            #AIC=1018.39  BIC=1026.144         #El AIC aumenta el 0.6 y el BIC disminuye en casi 3 unidades, además usamos 2 variables menos po rlo que este modelo es mejor

fit4<-arima(data_df2, c(0,0,1), include.mean = FALSE)
fit4                  # AIC=1018.64, BIC=1026.39

fit5<- arima(data_df2,c(1,0,1), include.mean = FALSE)
BIC(fit5)                #AIC=1020.39, BIC= 1031.019       -> mucho peor

# Por tanto nos quedamos con el modelo ARIMA(1,0,0) para data_df2



#Utilizamos fit2 de auto.arima, veamos la predicción para el nuevo año

par(mfrow=c(1,1))
pred<-forecast(AirPassengers, h=12, model=fit1, level=c(80,95), fan=TRUE)
plot(pred)



#Simulemos un proceso como el anterior

(phi1<- fit1$coef[1])
(phi2<- fit1$coef[2])
(theta1<-fit1$coef[3])

sigma2<-mod1$sigma2

simu <- arima.sim(model= list(order=c(2,1,1), ar=c(phi1, phi2), ma=c(theta1), seasonal=list(order=c(0,1,0), period=12)), n=144, innov=rnorm(144,0,sqrt(sigma2)))
plot(simu)




# Quedaros con las 100 primeras observaciones de la serie
# generar una predicción para los 44 valores restantes
# Dibujad la serie real junto com las predicciones. 
# ¿Qué dirías de la bondad de ajuste?
train_serie=AirPassengers[1:100]
pred2<-forecast(train_serie, h=44, model=fit1)
plot(pred2)
lines(101:144, AirPassengers[101:144], col="red", lwd=2)



#Comprovem en quants casos el valor real està forma de l'interval de confianza 95% de predicció
df<- data.frame(real=AirPassengers[101:144], lower=pred2$lower[,2], pred=pred2$mean, upper=pred2$upper[,2])

#Creamos una nueva variable que indica si la observación esta o no dentro del intervalo
df$fora <- ifelse(df$real >=df$lower & df$real<=df$upper, 0, 1)

table(df$fora)
#Totes les observacions estan dins d'un interval de 95% de confianza.


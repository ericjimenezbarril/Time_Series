#### PRÁCTICA 5 ####
## 1. IDENTIFICACIÓN DE PROCESOS ARMA(p,q)
#1. Datos relativos a la media diaria de cientes que compran en una tienda online 
#cada hora. Proponed un modelo que parezca razonable para ajustar estos datos, 
# basándose en las herramientas que tenemos disponibles y el sentido común.
library(lmtest)
library(zoo)
library(forecast)
data<-read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 5 (031023)/prac5TS.txt", header=T)
data <- data.frame(
  "Values" = data$Values
)
length(data$Values) #2000

par(mfrow=c(1,1))
online_values=data$Values
plot(online_values)

#A priori no sabemos si son datos estacionarios. 
abline(h=mean(online_values), col="red", lwd=2)
#la variancia no parece constante


#Veamos las funciones acf y pacf

par(mfrow=c(2,1))
acf(online_values)
pacf(online_values)


#Veamos que nos dice la función arima

fitarima<- auto.arima(online_values)
fitarima

# A priori nos dice que será un modelo ARiMA(3,0,1) con media 99.9227
# tiene un AIC=7362,19 y BIC=7395.79

coeftest(fitarima)
#Notamos que el coeficiente ar3 no es significativo, lo que nos lleva a pensar que un modelo ARIMA(2,0,1)
# mejora nuestro modelo

fitarima2<- arima(online_values, order=c(2,0,1))
fitarima2
#Observamos que tiene un AIC=7360,2 por tanto mejora algo el modelo y además usa una variable menos
#por lo que mejora el modelo
#ahora
coeftest(fitarima2)
#Todos los valores son significativos,
#Notemos que
(fitarima3<-arima(online_values, order=c(1,0,1)))
#Nos da un AIC de 7933 lo que empeora mucho el modelo
(fitarima4<-arima(online_values, order=c(2,0,0)))
#Nos da un AIC de 7651 lo que empeora mucho el modelo
(fitarima5<-arima(online_values, order=c(0,0,2)))
#Nos da un AIC de 8759.05 lo que empeora mucho el modelo

#En definitiva, el modelo que mejor aproxima nuestros datos es un modelo ARMA(2,1)
#y obtenemos el modelo
#Xt = 99.9227 + 1.7887 X_{t-1} -0.8936X_{t-2} +e_t -0.6406 e_{t-1}  
#Donde e_t ~ N(0, 2.307)

# Nos quedamos con el modelo ARMA(2,1)
#Veamos como es la función EACF
library(TSA)
eacf(online_values) #Veamos si la EACF que obtenemos es parecida a una ARMA(2,1) ->Podría serlo, sí


#### SIMULACIÓN DE UN PROCESO ARMA(p,q)
#2.1 Simulad un proceso con las características de los del apartado anterior.
#Comprobad que las características num (esperanza y varianza) del proceso coinciden 
#con las originales y mediante la función auto.arima() se recuperan los parámetros usados en la simulación

(phi1   <- fitarima2$coef[[1]])
(phi2   <- fitarima2$coef[[2]])
(theta1 <- fitarima2$coef[[3]])
(cons   <- fitarima2$coef[[4]])
x <- c()
#Notemos que
# E(Xt) = cons + phi1 E(X_{t-1} + phi2 E(X_{t-2}))
# si el proceso es estacionario, las esperanzas son iguales
# -> Ex = cons/(1-phi1-phi2)
x[1] <- rnorm(1,cons/(1-phi1-phi2), 1)
x[2] <- rnorm(1, cons/(1-phi1-phi2), 1)
(sigma2<-fitarima2$sigma2)
e <- rnorm(2000, sd=sqrt(sigma2))
for (i in 3:2000){
  x[i] <- cons + phi1*x[i-1] + phi2*x[i-2]+theta1*e[i-1]+e[i]
}
y <- x - cons/(1-phi1-phi2) +cons
plot(y)



par(mfrow=c(2,1))
acf(y);pacf(y)
#Las funciones acf y pacf son similares




simu_arima<- auto.arima(y)
simu_arima
#Notamos que nos da un proceso ARMA(2,1) con mismos (casi) coeficientes

mean(online_values) #99.93483

# -> En la simulación 99.849

var(online_values) #21,26708
var(y)
# -> En la simulación 24.72129

#Podemos suposer que son iguales aprox.



## PRÁCTICA 2.2
# Simulad procesos con diferentes valores para los parámetros y observad como son 
# las correspondientes funciones de autocorrelación y autocorrelación parcial.


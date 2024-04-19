#### 6. ANÁLISI D'UNA SÈRIE TEMPORAL
library(lmtest)
library(zoo)
library(forecast)
library(TSA)
library(tseries)
library(stats)
### 6.1 Función de AutoCorrelación Extendida
# -> Para decidir orden AR/MA que tiene la serie
#  eacf()


### 6.2 Test de Dickey-Fuller Aumentado
# Comprobar la presencia de tendencia en series temporales

# H0: La serie tiene alguna raíz unitaria (No es estacionaria).
# H1: La serie no tiene ninguna raíz unitaria (Es estacionaria)


### 6.3 Diferenciación de una serie temporal
# En R
# D_{t,k}^(m) = diff(X_t, lag=k, order=m)

### 6.4 ANÁLISI DE LOS RESIDUOS
#Funciones para estudiar los residuos:
# -> checkresiduals()
# -> Test de Box-Pierce
# -> Test de Ljung-Box

# PRÁCTICA 6.1
data1<-read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 6 (101023)/prac6TS_1.txt", header=TRUE)
ts1 <- ts(data1)
plot(ts1)
#A priori parece que la media es constante y está centrada en 12, además podemos pensar que la varianza es constante.
# No obstante, podemos hacer un ADF para ver si lo es
adf.test(ts1)
#Obtenemos un pvalor menor que 0.01 por tanto rechazamos la hipótesi nula, es decir la serie es estacionaria

#Veamos sus funciones acf, pacf y eacf
par(mfrow=c(2,1))
acf(ts1)
pacf(ts1)
#Notamos que tenemos que la PACF descience exponencialmente y p(0)=1, p(1)>0, p(h)=0 h>1
# Por tanto podriamos pensar que tenemos un modelo MA(1)
#Vemos la función eacf
eacf(ts1)
#Recordemos que si tenemos un modelo (teorico) ARMA(i,j) la tabla será un triangulo de ceros a partir de la casilla (i,j)
# En este caso podemos observar que como el modelo no es teorico, la tabla no es exactamente teórica
# pero podemos observar como se ajusta bien a un modelo MA(1)

# Veamos que nos recomienda la función auto.arima
(fit1 <- auto.arima(ts1))

# En efecto, nos recomienda coger un modelo ARIMA(0,0,1) es decir MA(1) con media 12.0225 como podíamos intuir con el primer dibujo.
# Veamos la significación del coeficientes (aunque habiendo solo uno debería ser significativo)
coeftest(fit1) # Tenemos que en efecto el coeficiente que acompaña a la variable MA es significativo
 # la media no es un coeficiente que acompañe a una variable si no que es constnate.

# Por último, chequeemos los residuos de nuestro modelo
checkresiduals(fit1)
# En el l-jung box-test obtenemos un pvalor de 0.2885 por lo que no podemos rechazar la hipótesis nula 
# por lo que los residuos son independientes. 
#Veamos por último que son WN como queremos
auto.arima(fit1$residuals)
#Obtenemos un ARIMA(0,0,0) con media 0 como queríamos
#Así, podemos pensar que MA(1) es un modelo que aproxima bien nuestros datos


















# PRÁCTICA 1.2
data2<-read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 6 (101023)/prac6TS_2.txt", header=TRUE)
ts2<-ts(data2)
plot(ts2)
# De nuevo parece que tenemos una serie estacionaria centrada en 20. Veamos  si lo es
adf.test(ts2)   #Obtenemos un pvalor menor que 0.01 por lo que rechazamos H0 y por tanto concluimos que es estacionaria
par(mfrow=c(2,1))
acf(ts2)
pacf(ts2)
#En este caso de nuevo según los gráficos acf y pacf parece que tendremos un modelo AR(3)
eacf(ts2)
# Segun su eacf parece que tendremos un modelo ARMA(1,3) o AR(3)
#Veamos que nos recomienda la función auto.arima
(fit2<-auto.arima(ts2))
# En efecto, obtenemos un modelo ARMA(1,3)  


#Veamos si los coeficientes són significativos
coeftest(fit2)
# Parece que hay dos coeficientes del MA que no son significativos. 
fit2 # Tiene un AIC=28342.91 y BIC=28386.16

#Veamos otros modelos
fit21 <- arima(ts2, c(1,0,0))  # AIC=28505.84, BIC=28527.47
fit22 <- arima(ts2, c(1,0,1))  # AIC=28387.32, BIC=28416.16
fit23 <- arima(ts2, c(1,0,2))  # AIC=28343.55, BIC=28379.6
fit24 <- arima(ts2, c(1,0,4))  # AIC=28344.17, BIC=28394.64
#Obtenemos que el mejor modelo es un ARIMA(1,0,2)
#Veiem si té tots els coeficients significatius
coeftest(fit23) #En efecto todos los coeficientes son significativos

#Veamos por último si los residuos son no correlacionados
checkresiduals(fit23)
#De nuevo el pvalor es 0.5643 por lo que  podemos aceptar que son idependientes

#Veamos por último
auto.arima(fit23$residuals)
#Lo estima con un ARIMA(0,0,0) con una media cero y sigma^2 =1 por lo que indica que el modleo es bueno


# Probemos con el AR(3) que parecoa de el ACF y PACF y veamos si es mejor (además tiene menos variables lo que es potivo)
fit25 <- arima(ts2, c(3,0,0))   # AIC=28340.11  #BIC=28376.16
coeftest(fit25)   #Todos los coeficientes son significativos
# Además tiene AIC y BIC menores, menos variables y se ajusta a lo visto en ACF y PACF  (y también a uno de los dos modelos observamos en EACF)
# Veamos los residuos
checkresiduals(fit25$residuals)
#Obtenemos un pvalor de 0.9919 por lo que no podemos rechazar la hipótesi nula y por tanto confirmamos que son incorrelacionadas
qqPlot(c(fit25$residuals))
#Parece que de hecho los datos serán normales.
#Por tanto apodemos apicar las ecuaciones de Yule-Walker de los parámetros correspondientes.
#Calculemos las rho's
par(mfrow=c(1,1))
autocors <- as.numeric(acf(data2, plot=F)[[1]])[1:3]
Gamma = matrix(c(1, autocors[1], autocors[2], autocors[1], 1, autocors[1], autocors[2], autocors[1], 1), 3,3)
gamma = c(autocors[1], autocors[2], autocors[3])
Phi=solve(Gamma)%*%gamma
Phi
#Els estimadors son phi1=0.76785332, phi2=-0.16224775, phi3=0.08650022.
fit25     #que coincideixen en els 2 primers decimals amb els valors estimats per la funció arima.
# EJERCICIO 4 
par(mfrow=c(1,1))
data4<-read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 6 (101023)/prac6TS_4.txt", header=TRUE)
ts4<-ts(data4$serie)
plot(ts4)
plot(data4)
#Los datos no són estacionarios, se ve una clara tendencia, por tanto se ve claro que hay que diferenciarla 
diff_data4<-diff(ts4)

plot(diff_data4)
adf.test(diff_data4) # Tiene un pvalor de 0.041 por tanto rechazamos la hipótesi nula por lo que la serie es estacionaria.

#Veamos como podemos estimar la serie diferenciada
par(mfrow=c(2,1))
acf(diff_data4)
pacf(diff_data4)   #Observamos como podríamos tener o bien un MA(1), AR(1) o ARMA(1,1)
#Es decir podríamos estudiar el modelo general seria un ARIMA(1,1,0), ARIMA(1,1,1), ARIMA(0,1,1)

#Ahora veamos que 
eacf(diff_data4)   #De nuevo dudamos si pude ser un MA(1) o un ARMA(1,1) para la serie diferenciada

#Comparemos estos dos
fit_diff1<-arima(diff_data4, c(0,0,1))     # AIC= 309.4,    BIC=317.0763
fit_diff2<-arima(diff_data4, c(1,0,0))     # AIC= 312.8677, BIC=318.5432
fit_diff3<-arima(diff_data4, c(1,0,1))     # AIC= 313.3643, BIC=320.9316

#Veamos que nos dice el autoarima
(fit4 <- auto.arima(ts4))                   # AIC=311.4   BIC=317.08
#Obtenemos un ARIMA(0,1,1)  # que coincide con lo anteerior.
#Veamos que los coeficientes tienen significación
coeftest(fit4)            #En efecto todos los coeficientes tienen significación estadística
#Veamos si el modelo ajusta bien los datos
checkresiduals(fit4$residuals)
#Obtenemos un pvalor de 0.20 por tanto no rechazamos la hipótesi nula de que los residuos sean incorrelacionados
# Además la función ACF es claramente de un WN y si buscamos un modelo para los residuos obtenemos
auto.arima(fit4$residuals)
#Obtenemos un ARIMA(0,0,0) con media 0 
#Además si hacemos un 
shapiro.test(fit4$residuals)
#Obtenemos que el pvalor es mayor que 0.05 por lo que no podemos rechazar la hipótesi nula de que los datos sean normales

#Hagamos una predicción para la última estimación
#Hemos obtenido un modelo MA(1) para la serie diferneciada con \theta=-0.37942
# El modelo es
fit4
# Y_t = \phi_t - \theta \phi_{t-1}, donde Y_t son los datos diferenciados

var_dd = sd(diff_data4)^2
theta= -0.3794
sd2 <- var_dd/(1+theta^2)   # 30.5871
#Notamos que haciendo 
summary(fit4)
# Tenemos sd^2 = 30.98 que es la aproximación que buscábamos
#COmo obtenemos un drift de 1.2186
set.seed(12345)
sd2= 30.98
drift= 1.2186
errors<-c()
errors[1]=rnorm(1,0,sqrt(sd2))
for (i in 2:50){
  errors[i] = diff_data4[i]-drift-theta*errors[i-1]
}


#Hagamos la predicción con la librería forecast
### Equivalentment, per la serie diferenciada fent servir la llibreria forecast
pred <- forecast(diff_data4, h=1, model=fit4)
pred

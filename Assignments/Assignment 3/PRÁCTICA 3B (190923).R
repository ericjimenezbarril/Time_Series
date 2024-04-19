### PRÁCTICA 3B ###
# SIMULACIÓN E IDENTIFICACIÓN DE PROCESOS AR Y MA


#SERIE ESTACIONARIA: Una serie es estacionaria si su media y su varianza son constantes en el tiempo y la covarianza 
#en periodos equidistantes es invariante respecto al tiempo. 


## PROCESO AR (1)
# Un proceso {Xn} es "AUTOREGRESIVO DE ORDEN 1" AR(1) si cumple una relación del tipo

#  Xn= \phi * X{n-1} + Zn

# Zn es el ruido blanco: Un ruido blanco es una Serie en que
#### 1. Todas las variables son independientes
#### 2. E(Zn)=0, Var(Zn)=sigma^2 (cst)                        para todo n, h
#### 3. Las variables son incorrelacionadas (cov(Zn, Zn+h)=0).

# \phi es una constante con |\phi|<1. 
# Si |\phi|=1 tendriamos un paseo aleatorio y no estacionario.
# Si |\phi|>1 tampoco seria estacionario

# La  función de autocorrelación (ACF) de lag k>=0 es p_x(k)=(\phi)^k



## CORRELOGRAMA (ACF)
#Herramienta gráfica que visualiza las autocorrelaciones.
# Estudiando esta gráfica hay posibilidad de identificar estacionalidad, tendencia y estacionariedad.

# En el correlograma se muestra el "COEFICIENTE DE CORRELACIÓN" para el eje Y y el número de periodos de retardo en el eje X.
# -> Si un correlograma tiene coeficientes de correlación cerca de 0, sugieren que la serie de datos es aleatoria.




## AUTOCORRELACIÓN PARCIAL (PACF)

# La función de autocorrelación parcial es la autocorrelación entre dos periodos de tiempo equidistantes k:
# \alpha(k) = \rho[X(n), X(n+k)]
# Para un AR(n): "Xn=\phi_1 X_(n-1)+...+ phi_p X_(n-p)+Zn
#la función de autocorrelación parcial es
# alpha(1),...,alpha(p) !=0; alpha(k)=0, k>p.
# Y su correlograma tiene forma exponencial o sinusoidal




## FUNCIÓN AUTOARIMA() de R
# autoarima(): devuelve el mejor modelo ARIMA(m,n,s) según el valor AIC, AICc o BIC.
# La función realiza una búsqueda sobre el modelo posible dentro de las restricciones de orden proporcionadas. 

# Un modelo ARIMA es una combinación de AR(m) + MA(s) más una diferenciación de grado n: 
#veremos estos modelos en la siguiente práctica



## PRÁCTICA 1.1 
# Consideramos un proceso AR(1): Xt=a*X_(t-1) +Et. 
#Simula un proceso como el anterior (por ejemplo n=10000) y representa las autocorrelaciones y autocorrelaciones parciales
#COmo cambian si cambia el valor de "a"?
#En cada caso, mira que modelo propone la función auto.arima(). ¿Es razonable?
#Et ~ N(0,1), X0=0

# PROBAMOS CON |a|<1 para que sea estacionaria
library(forecast)
library(openxlsx)
library(lmtest)

# CASO 1: \phi=0.6
#Modelo teórico: Xt=0.6X_{t-1} + E_t

nobs <- 10000 
serie <- vector() 
serie[1] <- 0 
alpha <- 0.6 
for (i in 2:nobs) 
{ 
  serie[i] <- alpha*serie[i-1]+rnorm(1) 
} 
plot(serie) 
#La serie que obtenemos de la simulación está centrada en el 0, sin tendencia ni estacionalidad, 
#con una varianza manteniéndose constante a lo largo del tiempo. Por lo que estamos ante una 
#serie estacionaria. 


par(mfrow=c(2,1))
acf(serie)
pacf(serie)
par(mfrow=c(1,1))
# En el correlograma (ACF) observamos que p(1)=0,6, p(k)=0.6*k.
# Por lo que vemos, como la función desciende potentemente a cero a medida que va aumentando el lag
#EN la función (PACF) autocorrelación parcial el primer lag es en 0.6, a(1)=0.6, en el resto de lags los toma muy cercanos a 0.



####Si el primero baja de forma exponencial y el segundo tiene "n" lineas fuera de las azules, es un AR(n)
#En este caso es un AR(1).
#Veamos con un arima.




fitarima = auto.arima(serie, allowdrift=FALSE) 
fitarima 
coeftest(fitarima) 

         
#Nuestra simulación de la función autoarima() propone descomponer en un AR(1) 
#con coeficiente 0.614250    + una media movil MA(1) con coeficiente -0.039387

# Xt = 0.6327*X_{t-1} -0.0461 * E_{t-1} + E_t
# donde E_t es el ruido blanco

# Ahora bien, si miramos los test sobre los coeficientes, el coeficiente que acompaña
#al componente autoregresivo (0.614250) es signficativamente distinto de 0, puesto que el
# pvalor es <2.2e-16 podemo afirmar con un 5% de significación que es distinto de 0.

# Mientras que el coeficiente que acopaña la media movil MA(1) (-0.039387) se podria considerar
#nulo siempre que tengamos un nivel de significación del 2% pues el pvalor es 0.02261>0.1

#Si anulamos la media movil MA(1) tenemos
arima1<-arima(serie, c(1,0,0))
arima1
coeftest(arima1)

#Obtenemos Xt= 0.0018535 + 0.5885293*X_{t-1} + E_t 
#donde E_t es ruido blanco.

#Nuevamente el coeficiente autoregresivo es distinto de 0 (pvalor <2e-16) pero nos 
#aparece una constante "0.0018536" que es 0 pues, si hacemos el test
#H0: intercept=0; H1: intercept!=0; el pvalor es 0.9393>0.05 por tanto no podemos rechazar la hipótesi nula.

#Así podemos quitar la constante
arima2<-arima(serie, c(1,0,0), include.mean = FALSE)
arima2
coeftest(arima2)
#Obtenemos finalmente:
#Xt=0.6034271*X_{t-1} + E_t

#Observamos que el AIC del primer modelo tenia un AIC= 28.553,03 que no está lejos
# del AIC=28.558,64 del modelo que acabamos de hacer.

#Teniendo en cuenta el principio de parsimonia (el modelo más simple es el mejor si explica casi lo mismo)
#El modelo Xt=0.603471X_{t-1}+Et sería el adecuado para esta situación.

#AIC: Contra más bajo mejor es el modelo


# CASO 2: \phi=-0.3
# Xt= -0.3X_{t-1} + Et
alpha<- -0.3
y <- arima.sim(n = 10000, list(ar = alpha), innov=rnorm(10000)) 
plot(y)

#Observamos que la serie está centrada en el 0, sin a priori tendencia ni estacionalidad
# y la variancia parece permanecer constante a lo largo del tiempo, por lo que paraece una serie estacionaria.

par(mfrow=c(2,1))
acf(y); pacf(y)

#En el correlograma (gráfica ACF) observamos que p(0)=1 (esto siempre), p(1)=-0.3, 
# i podriamos decir en general p(k)=(-0.3)^k pues es hay un decrecimiento exponencial
# i además losp icos van alternando positivo y negativo descenciendo potencialmente a 0 cuando aumenta el lag.

#En la función de autocorrelograma parcial solo el primer lag toma valor -0.3
# \alpha(1)=-0.3 mientras que el resto de lags toman valores dentro de las bandas.
#Esto probablemente indique (como sabemos) que estamos ante un proceso AR(1)

#Veamos el test arima

fitarima=auto.arima(y, allowdrift = FALSE)
fitarima
#Obtenemos un AIC=28166.65

coeftest(fitarima)
#En efecto nos dice que estamos ante un modelo AR(1) con \phi_1=-0.2999135 y pvalor menor que 2.2e-16
# por tanto comn un 5% de significación se trata de este modelo.




### PRÁCTICA 1.2
# Consideramos un proceso MA(1) dado por 
# Xt = Zt + \alpha * Z_{t-1} con Z_t ~ N(0, \sigma^2)
#Simula un proceso como el anterior con n=1000 observaciones y representa el autocorrelaciones y autocorrelaciones parciales.
#Como cambia en función de \alpha
# Es razonable el modelo auto.arima()?

#### MA(1) process

#Caso 1-> \theta = -0.8
nobs <- 1000
serie <- c()
zt <- rnorm(1000)
serie[1] <- 0
alpha<- -0.8
for (i in 2:nobs){
  serie[i]<- zt[i]+alpha*zt[i-1]
}
plot(serie)
#La serie está centrada en 0, sin tendencia ni estacionalidad, la variancia se mantiene consstante a lo largo del tiempo.
# Por lo que estamos ante una serie estacionaria.

#Veamos la autocorrelacion y autocorrelacion parcial.

par(mfrow=c(2,1))
acf(serie)
#En la serie vemos que el primer lag es negativo y distinto de 0 mientras que el restoestá dentro de las bandas
# y bajan de forma exponencial. 
# Observamos que p0=1, p1=-0.8/(1+(-0.8)^2)=-0.488
# y p_k=0 k>1
pacf(serie)
#observamos que va descendiendo a medida que aumenta el lag.

fitarima=auto.arima(serie, allowdrift = FALSE)
coeftest(fitarima)
#Observamos que en efecto obtenemos un modelo MA(1) con coeficientes -0.81394 y pvalor menor a 2.2e-16




##CASO 2-> \phi=0.6

nobs <- 1000
serie <- c()
zt <- rnorm(1000)
serie[1] <- 0
alpha<- 0.6
for (i in 2:nobs){
  serie[i]<- zt[i]+alpha*zt[i-1]
}
plot(serie)
#La serie está centrada en 0, sin tendencia ni estacionalidad, la variancia se mantiene consstante a lo largo del tiempo.
# Por lo que estamos ante una serie estacionaria.

#Veamos la autocorrelacion y autocorrelacion parcial.

par(mfrow=c(2,1))
acf(serie)
#En la serie vemos que el primer lag es positivo y distinto de 0 mientras que el restoestá dentro de las bandas
# y bajan de forma exponencial. 
# Observamos que p0=1, p1=0.6/(1+(0.6)^2)=0.44
# y p_k=0 k>1  (la función desciente potencialmente a 0 a medida que aumenta el lag)
pacf(serie)
#observamos que va descendiendo a medida que aumenta el lag.

fitarima=auto.arima(serie, allowdrift = FALSE)
fitarima
coeftest(fitarima)
#Observamos que en efecto obtenemos un modelo MA(1) con coeficientes -0.81394 y pvalor menor a 2.2e-16

# Por tanto se trata de un modelo MA(1) con coef 0.629737 y es
# Xt = 0.6334*E_{t-1} + Et (Et ruido blanco)




### DESCRIPCIÓN Y EXPLORACIÓN DE UNA SERIE TEMPORAL
# Datos relacionados con la magnitud de terremotos alrededor del mundo de 1916 a 2015
# (i) Convierte los datos a una serie temporal y representa el gráfico
datos <- read.table("C:/Users/ERIC/Desktop/SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 3 (190923)/Prac3TS.txt", header = TRUE, sep = "\t")
datos
start<- as.yearmon("1916-01-01")
end <- as.yearmon("2015-01-01")
serie<-ts(datos$Quakes, start=start, end=end,frequency=1)
plot(serie)

#(ii) ¿Tiene tendencia?
#Se detecta una (muy) ligera tendencia ascendiente en el tiempo, sobretodo en la parte final (80-actualidad)

#(iii) ¿Tiene estacionalidad?
#No se detecta una estacionalidad destacada a priori,

#(iv) Relizad una regresión lineal en el tiempo. ¿Cuál es la bondad de ajuste?
t <- 1:length(datos$Quakes)
datos.lm<- lm(Quakes~t, data=datos)
summary(datos.lm)
#Se obtiene el siguiente modelo
coefs <- datos.lm$coefficients
#Quakes = 9.41333 + 0.06330 *t + E_t    #-> E_t son los residuos
#Los coeficientes son significativos pues tienen pvalores menores a 0.01.
 
#Además el modelo tiene un coeficiente de determinación de 18.8% lo que dice que la variable "tiempo"
#expica un 19.8% de la variabilidad de la serie.
#Todo y que la variable "t" explica la tendencia creciente, la bondad de ajuste de 18.8% es pequeña.

#El "Adjusted R-squared" es una versión ajustada de "Multiple R-squared"

plot(t, datos$Quakes)
lines(t, coefs[1]+coefs[2]*t, col="orchid2", lwd=2)



#Tomad los residuos de la regresión lineal y  dibuja autocorrelaciones y 
#autocoreelaciones parciales. ¿Qué tipo e proceso te parece que estamos tratando?

#tomemos los residuos
plot(t, datos.lm$residuals, main="Noise process(Residuals)")
abline(h=0)
#Parece que los datos están centrados en 0 y no tienen estacionalidad ni tendencia (los residuos)
# Además parece que la variancia es constante por lo que puede se una serie estacionaria

#Veamos el autocorrelograma y autocorrelograma parcial
acf(datos.lm$residuals)
pacf(datos.lm$residuals)
#Parece que el modelo es de datos aleatorios por tanto tanto el correlograma como correlograma parcial
# muestran ruido blanco. En ambos, los valores estan dentreo de los inervalo de valores cercanos a "cero"

#Veamos que propone la función arima()
fitarima<-auto.arima(datos.lm$residuals, allowdrift = FALSE)
fitarima
#La función auto.arima nos confirma que en efecto, la serie de los residuos siguen un ruido blanco  como esperábamos.
# con una variancia de 14.12 y un AIC=552.64.


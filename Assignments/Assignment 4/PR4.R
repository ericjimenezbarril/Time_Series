########### SERIES TEMPORALES Y ESTIMACIÓN DE LOS PARÁMETROS
library(forecast)
##### 1. RAÍCES DEL POLINOMIO CARACTERÍSTICO Y ESTACIONARIEDAD
library(polynom)
# Estacionariedad (idea): el comportamiento probabilístico de la serie en el futuro
#serà análogo al del pasado.

# Serie estadionaria: la media y la variabilidad son constantes a lo largo del tiempo.
# (se refleja en las gráficas ya que las series tienden a oscilar alrededor de una media constante)
# (y la variablilidad con respeto esta media permanece constante en el tiempo)


# PRÁCTICA 1.1
# Considera el proceso AR(1) dado por X_t = 0.6*X_{t-1} + S_t. ¿Es un proceso estacionario?

# ->Para que un proceso AR(1) sea estacionario, es necesario que |\phi|<1 o les arrels del P.Característic siguin de modul >1.
# En nuestro caso, tenemos que:
# (1-0.6B)Xt = St
# -> Como |\phi| = 0.6<1, el proceso es estacionario.

# -> Otra forma de pensarlo es buscando las raíces del P.Característico
# -> Las raíz es B=1/0.6 = 10/6 > 1 --> El proceso es estacionario.


# PRÀCTICA 1.2
# Consideramos el proceso AR(2) dado por X_t = 1.095445X_{t-1} - 0.3X_{t-2} + S_t
# Busquemos las raíces, en este caso el polinomio característico es
# (1-1.095445B+0.3B^2)X_t = St
roots=solve(polynomial(coef=c(1,-1.095445, 0.3)))
roots
Mod(roots[1]) #ambas raíces tienen raíz fuera del círuclo unidad por lo que el 
#proceso sí es estacionario.

#Hagamos el correlograma
phi0=1
phi1= 1.095445
phi2=-0.3

#CORRELOGRAMA
phi1=1.095445
phi2=-0.3
rho=NULL;
rho1= phi1/(1-phi2)
rho2= (phi1^2 + phi2*(1-phi2))/(1-phi2)
rho[1]=rho1; rho[2]=rho2
max.lag=20
for (k in 3:max.lag){
  rho[k]=phi1*rho[k-1]+phi2*rho[k-2]
}
rho #valores de las rho
plot(y=rho, x=1:max.lag, type="h", ylab='ACF', xlab='lag', ylim=c(-1,1));
abline(h=0)



##OTRO CAMINO: (de R)
x<-c()
x[1]<-0; x[2]<-0
for (i in 3:10000){
  x[i] <- phi1*x[i-1]+phi2*x[i-2]+rnorm(1)
}
par(mfrow=c(2,1))
acf(x); pacf(x)



### 1.3. Consideramos el proceso AR(2) dado por Xt=X{t-1}-0.5X{t-2} + St
#El polinomio característico es \phi(x) = 1-x+0.5x^2
roots=solve(polynomial(coef=c(1,-1, 0.5)))
roots
Mod(roots[1]) #ambas raíces tienen raíz fuera del círuclo unidad por lo que el 
#proceso sí es estacionario.


#CORRELOGRAMA
phi1= 1
phi2=-0.5
rho=NULL;
rho1= phi1/(1-phi2)
rho2= (phi1^2 + phi2*(1-phi2))/(1-phi2)
rho[1]=rho1; rho[2]=rho2
max.lag=20
for (k in 3:max.lag){
  rho[k]=phi1*rho[k-1]+phi2*rho[k-2]
}
rho #valores de las rho
plot(y=rho, x=1:max.lag, type="h", ylab='ACF', xlab='lag', ylim=c(-1,1));
abline(h=0)



##OTRO CAMINO: (de R)
x<-c()
x[1]<-0; x[2]<-0
for (i in 3:10000){
  x[i] <- phi1*x[i-1]+phi2*x[i-2]+rnorm(1)
}
par(mfrow=c(2,1))
acf(x); pacf(x)


## PRÁCTICA 1.4
#Suposemos que las raíces de un pol.car de un modelo AR(2) son 0.6 y 0.3
# ¿Es estacionario?->No, las raíces tienen que estar fuera del círculo unidad.
par(mfrow=c(1,1))

## PRÁCTICA 1.5
#Consideramos el proceso AR(2) Xt=1.6X_{t-1}+0.3X_{t-2}+St

#Raíces del polinomio característico:
roots=solve(polynomial(coef = c(1, -1.6, -0.3)))
roots
#NO es un proceso estacionario. 

# Calculemos el correlograma
##OTRO CAMINO: (de R)
phi1=1.6
phi2=0.3
rho=NULL; 
rho1=phi1/(1-phi2); rho2=(phi2*(1-phi2)+phi1^2)/(1-phi2) 
rho[1]=rho1; rho[2]=rho2 
max.lag=20 
for (k in 3:max.lag) rho[k]=phi1*rho[k-1]+phi2*rho[k-2] 
rho # presentar los valores 
plot(y=rho,x=1:max.lag,type='h',ylab='ACF',xlab='Lag',ylim=c(-1,+1)); abline(h=0)
#No tiene sentido pues tiene valores mayores a 1 ya que no es estacionaria.
#Veamos que nos dice ARIMA

ar2<-arima.sim(model=list(ar=c(phi1,phi2)),n=100)
#No podemos simular con la función arima porque el proceso NO es estacionario.


#Si hacemos una simulación vemos que crece rápidamente a números grandes

x<-c()
x[1]<-0
x[2]<-0
for (i in 3:3000){
  x[i]<-phi1*x[i-1]+phi2*x[i-2]+rnorm(1)
}
head(x)
plot(x)



### ESTIMACIÓN DE LOS PARÁMETROS 
#Práctica 2.1
# Proponed un modelo que ajuste los datos de "prac4TS"
library(zoo)
data<-read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 4 (260923)/prac4TS.txt",header = FALSE)

data <- ts(data, start=as.yearmon(as.Date("2008-01-01")), end=as.yearmon(as.Date("2012-12-01")), frequency =12)

# En primer lugar investigamos la serie.
plot(data)
#A priori no hay tendencia (creciente ni decreciente) y parece estacionaria.
# con media cercana o igual a 0 y variancia que podemos considerar a priori constante 
# (se mueve dentro de unas bandas tocándolas repetidamente en las diferentes subidas y bajadas)
abline(h=mean(data), col="red") ##Vemos que los datos están centrados en 0 y además parece tener una variancia constante por lo que a priori parece estacionaria
## Podemos hacer la media móbil para ver si tiene tendencia 
lines(ma(data, order=12, centre=T), col="orchid") #podemos observar cierta tendencia ascendiente
lines(ma(data,order=4, centre=T), col="gold")


## Veamos si una recta de regresió és un model que s'ajusta a les dades
time=1:length(data)
reg_model=lm(data~time)
summary(reg_model)
#Observamos que R2 = 0.0387 por tanto la recta solo explica menos del 3.9% de mis datos
library(lmtest)
coeftest(reg_model) #Los coeficientes tienen pvalor mayor a 0.05 por tanto no tienen significación estadística 
# por lo que no explica el modelo



# ¿Tiene estacionalidad? Veamos
par(mfrow=c(1,1))
boxplot(data~cycle(data)) # Notamos que no todas las cajas están al mismo nivel por lo que la variancia no es constante 
# por tanto NO hay estacionalidad
##Si no hubiera estacionalidad todas las cajas estarían al mismo nivel

decompose=decompose(data)
plot(decompose)                         ## Notamos que hay una cierta estacionalidad.
decompose$seasonal



# Podemos plantearnos modelo AR o MA pues es estacionaria.

#Construimos el correlograma y función de AutoCorrelación Parcial
par(mfrow=c(2,1))
acf(data)
pacf(data)

# El correlograma presenta solo una autocorrelación positiva distinsta de 0 rho(1)>0
# y el resto dentro de las bandas de confianza de 0.
#NO decae de forma exponencial por lo que parece ser un modelo MA(1)

#El autocorrelograma parcial presenta la primera negativa y distina de 0, \alhpa(1)>0
# y el resto distinto de 0 --> Podría wser un AR(2)



## Veamos que función propone auto.arima(2)
fitarima<-auto.arima(data)
library(lmtest)
coeftest(fitarima)
#Como tienen pvalores menor a 0.05, los dos coeficientes son significativos y tenemos
# que el modelo propuesto por auto.arima es
#Xt = 0.69484 X{t-1} - 0.32210 X{t-2} + Et




# Si quisieramos hacerlo con un nivel de significación de 0.01, entonces el coeficiente de X{t-2} seria 0.
#Por lo que podemos proponer mejorar el modelo con un AR(1) a ver que pasa

arima2<-arima(data,c(1,0,0), include.mean = FALSE)
coeftest(arima2)
# Tenemos el proceso:

# Xt = 0.51579X{t-1} + Et
#donde el coeficiente que acompaña la componente X_{t-1} es significativamente distinta de 0

#Por tanto podríamos considerar que es un modelo AR(1)
arima2
#Además tiene un AIC=186.26

#En cambio el arima1 tiene un AIC= 182.16
# es algo mejor pero tampoco mucho, y tenemos que considerar que un modelo más simple siempre es mejr



#En todo el estudio NO nos hemos preguntado si tiene estacionalidad
# ¿Tiene? Veamos
par(mfrow=c(1,1))
boxplot(data~cycle(data))
decompose=decompose(data)
plot(decompose)
decompose$seasonal


#Observamos que tiene algo de estacionalidad
data_sin = data-decompose$seasonal
par(mfrow=c(2,1))
plot(data)
plot(data_sin)
#Hemos suavizado algo la serie per oapenas se nota nada


acf(data_sin); pacf(data_sin) # son muy parecidos, parece que será el mismo modelo

fitarima2=auto.arima(data_sin)
fitarima2
coeftest(fitarima2) #Notamos que la 2a componente no tiene significación estadística
arima2 <- arima(data,c(0,0,1), include.mean = FALSE) #QUitamos el 2o MA pues teien nivel de significación mayor a 0.05
arima2              ## El AIC es 185.08 -> mejora el modelo
coeftest(arima2)
#En este modelo, para PREDECIR nuevos datos hacemos
# Xt = e_t + ma1 * e_{t-1} + c.estacionari.
#Tiene un AIC 185.08, algo mejor que el modelo AR(1)

#por tanto un posible proceso seria
#X_t =0.5294 E_{t-1} + E_t + Componente estacional del mes.





### MIREMOS EL ACF PARA EL RUIDO
random=decompose$random[complete.cases(decompose$random),]

acf(random); pacf(random)

model_random=auto.arima(random)
summary(model_random) #Tiene un BIC=129.2 y AIC=119.84
#Nos da un modelo ARMA(2,2)
#Veamos si los componentes són significativos
coeftest(model_random)
#Notamos que uno de los coeficientes no es significativo, lo que parece indicar que será un modelo
# ARMA(2,1) lo probamos
?arima
model_random_arima21=arima(random, c(2,0,1))
summary(model_random_arima21)   #Tiene un AIC=115.43 que es más bajo por lo que a priori es mejor
coeftest(model_random_arima21)  #QUitamos el Intercept pues no tiene significación estadística,



#####IMPORTANTEEEE ############

#Cuando hacemos un summary() el sigma^2 qque da es el valor de la variancia del ruido
#es decir en este caso serà un modelo
# Xt = ar1 X_{t-1} + ar2 X_{t-2} + e_{t} + ma1 e_{t-1} 
# con e_t, e_{t-1} ~ N(0, sigma^2)
################################


####CUIDADO: NO SABEMOS COMO AÑADIR DESPUÉS LA TENDENCIA PARA PREDECIR NUEVOS VALORES

### PARA ESTACIONALIDAD FACIL
#St= coefs_AR*xt +e_t+ coes_MA*E_{t-i} + component estacional (de la estació on estigui la t)


#Notamos que 

## PRÁCTICA 2.2
#Estima los parámetros del modelo que propone la función auto.arima() realizada directamente 
#sobre el fichero "prac4TS.txt" de acuerdo con las ecuaciones de Yule-Walker
auto.arima(data)
#Me da un modelo AR(2)

# Las ecuaciones de Yule-Walker para el AR(2) son

#Dado el modelo 
#Xt=\phi1 X_{t-1} +\phi2 X_{t-2} + Zt

#Las ecuaciones son
# \p1 = \phi1 \p0 +\phi2 \p(-1)
# \p2 = \phi1 \p1 +\phi2 \p0
 

# Como el proceso es estacionario, \p(-1) = \p(1)
p0=1
p1 <- as.numeric(acf(data, plot=FALSE)[[1]])[2]
p2 <- as.numeric(acf(data, plot=FALSE)[[1]])[3]

a<-rbind(c(p0,p1), c(p1,p0))
a
(b<-c(p1,p2))
solve(a,b)
#Esto nos dice que 
#Xt <- 0.6499070 X{t-1} -0.2718407X{t-2} + Et
# NO ENTIENDO MUY BIEN POR QUÉ ESTOS COEFICIENTES

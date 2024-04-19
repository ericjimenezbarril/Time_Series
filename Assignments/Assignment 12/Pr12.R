 
## PRÁCTICA 12: MODELOS ARCH Y GARCH 2
library(forecast)
library(tseries)
library(TSA)
library(fGarch)
library(lmtest)
setwd("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 12 (281123)/")

# Ejercicio 1
ej1 <- read.table("dst.txt", header=T)
data1 <- ej1$x

plot.ts(data1) #No parece un proceso estacionario.

#Veamos que la media es constante:
t = 1:(length(data1))
data.lm <- lm(data1~t)
data.lm
# t = 9.054e-06, intercept = -1.943e-02
#Además vemoa que
coeftest(data.lm)
#Ambos tienen pvalor mayor que 0.1 por lo que no podemso rechazar la hipótesi alternativa que sean 0.
#Por tanto es un proceso constante en la media, pero está claro que no en la volatilidad.

#Hagamos la prueba de Dickey Fuller
adf.test(data1) #  Rechazamos la hipótesi nula, por tanto el Test ADF nos dice que es estacionario, pero qualitativametne obervamos que no .
#pues no tiene media constante.

#Veamos si tiene estacionalidad.
#Para verlo lo comprobamos basándonos en el likelihood ratio test
fit1<- ets(data1)
fit2 <- ets(data1, model="AZN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#el pvalor es
1-pchisq(deviance, df) #H0: NO estacional
#No podemos rechazar la hipótesi nula de que no tiene estacionalidad.

#Mldelo propuesto en base a EACF, ACF, PACF
eacf(data1) #MA(5)  ARMA(2,5), AMRA(2,2)
par(mfrow=c(1,2))
acf(data1)
pacf(data1)
#Lo único que deja claro es que es un ARMA pero no el orden
#Podíá ser un ARMA(2,2)

# Modelo auto.arima
mod1 <- auto.arima(data1)
mod1 # ARMA(3,2) con media 0
#AIC = 1110.38, BIC=1139.83

coeftest(mod1) #todos son significativos

#Vemos los residuos
checkresiduals(mod1) #No Rechamamos la hipótesi nula de que se distibuyan de forma independiente

shapiro.test(mod1$residuals) #No son normales.
#Es un buen modelo. Estudiemos los residuos al cuadrado.

Box.test((mod1$residuals)^2, type="Ljung-Box")
#No son incorrelacionados. Vemoas de hecho
plot((mod1$residuals)^2) #muestran un gran pico en el pico de vairanza

acf((mod1$residuals)^2)
pacf((mod1$residuals)^2) #No son incorrelacionados.
#Parece que podemos ajustarles un modelo ARCH(1) o ARCH(2)
eacf((mod1$residuals)^2)#Según la EACF, un  GARCH(1,1) se ajustaría bien

# Ajustamos un modelo ARCH/GARCH para los residuos,

mod2 <- garch(data1)
mod_21 <- garchFit(~garch(1, 1), data=data1)
mod_21 # beta1 no es significativo, podemos descartarlo para tener un ARCH(1)
#Propone un GARCH(1,1) con 
#Y(t) = -0.01004 + E(t), E(t) ~ N(0, s(t)^2)
# s(t)^2 = 0.02794 + 0.6778 E(t-1) + 0.00000001 s(t-1)^2

mod_22 <- garch(data1, c(1,1))
mod_22; AIC(mod_22) #-25.5

mod3 <- garch(data1, c(0,1))
mod3; AIC(mod3) #-51

mod4<- garch(data1, c(0,2))
AIC(mod4)#-24

#Por tanto el mejor modelo según garch es ARCH(1)

fGarch(data1)


#Predecimos 
pred <- predict(mod3)
head(pred)
par(mfrow=c(1,1))
ts.plot(data1)
lines(seq(1, length(data1), 1), pred[, 1], col="red")
lines(seq(1, length(data1), 1), pred[, 2], col="red")

### Comptem en quants casos el valor real est??fora de l'interval de confian?a del 95%:
df <- data.frame(real=data1, lower=pred[, 2], upper=pred[, 1])

## Creem una nova variable que indica si l'observaci? est? fora de l'interval 
df$fora <- ifelse(df$real >= df$lower & df$real <= df$upper, 0, 1) 

head(df)

### I comptem
table(df$fora)

### Un 31.8% dels punts se'n van fora de l'interval del 95%.

Box.test(residuals(mod3), type="Ljung-Box")  #No rechazamos la hipótesi nula de independencia
Box.test(residuals(mod3)^2, type="Ljung-Box")  #NO rechazamos la hipótesi nula de independencia

checkresiduals(mod3) # Parecen rfesidusos normales

shapiro.test(residuals(mod3))# Los residuos son normales


par(mfrow=c(2,2))
acf(residuals(mod3)[2:length(residuals(mod2))]); 
pacf(residuals(mod3)[2:length(residuals(mod2))])

acf(residuals(mod3)[2:length(residuals(mod2))]^2); 
pacf(residuals(mod3)[2:length(residuals(mod2))]^2)

#Un buen ajuste de modelo.

# EJ 2
ej2 <- read.table("PRC2_data1.csv", sep=";",header=T)
data2 <- ej2$serie1
head(data2)
as.numeric(data2)

## EJ 3

ej3 <- read.table("PRC2_data2.csv", sep=";", header=T)

data3<- ej3$serie2

plot.ts(data3)

#Creamos la recta de regresión
t=1:(length(data3))
data3.lm <- lm(data3~t)
coeftest(data3.lm) #todos los coeficientes son significativos
# intercept = 36.466169, scope=0.2287253
intercept=data3.lm$coefficients[1]
slope=data3.lm$coefficients[2]

# Hacemos la media movil
MediaMovil1 = ma(data3, order = 12, centre =T)
MediaMovil2 = ma(data3, order=52, centre=T)
MediaMovil3=ma(data3, order=4, centre=T)
ts.plot(data3)
lines(t,intercept+slope*t,col="blue",lwd=2)
lines(MediaMovil1,col="pink",lwd=2)
lines(MediaMovil2, col="red", lwd=2) # Se ajusta muy mal
lines(MediaMovil3, col="pink3", lwd=2) #Con 4 se ajusta bastante bien


#Observamos que tiene una clara tendencia creciente.

#Veamos si tiene estacionalidad
#Podemos hacer los box-plots para 12 y 4, que son con los que obtuvimos buenos MA
length(data3) #200, no podemos hacerlo con 12, pero si si quitamos los ultimos datos
matrix_data=matrix(data=data3[1:192], nrow=12)
matrix_data_t=t(matrix_data)
boxplot(matrix_data_t, notch=TRUE)  #Se observa una clara estacionalidad
#No se observa estacionalidad.

#Probemos con 4
matrix_data=matrix(data=data3, nrow=4)
matrix_data_t=t(matrix_data)
boxplot(matrix_data_t, notch=TRUE)  #Se observa una clara estacionalidad
# No se observa estacionalidad.
ej3
#Probamos una estacionalidad anual.
año2016<- data3[1:53]
año2017 <- data3[54:105]
año2018 <- data3[106:157]
año2019 <- data3[158:200]
años <- list(año2016, año2017, año2018, año2019)

# Crear el boxplot
boxplot(años, names = c("año2016", "año2017", "año2018", "año2019"), col = "lightblue", main = "Boxplot de 4 años")
#Parece que podría haber una estacionalidad anual

# Lo hacemos con el truco de likelihood ratio test
fit1 <- ets(data3)
fit2 <- ets(data3, model="AZN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#P value
1-pchisq(deviance,df)

#No pdemos rechazar la hipótesi nula, por tanto no suponemos estacionalidad.

#Veamos que nos recomienda auto.arima. Está claro que no es estacionaria por lo que cuanto menos, hay que diferenciar

auto_1 = auto.arima(data3)
auto_1 #En efecto, nos da un modelo ARIMA(3,1,1) con drift, y  sin parte estacional.
coeftest(auto_1) # El primer coeficiente MA y AR son significativos. 
#AIC= 569.6, BIC=589.36
diff_data3 = diff(data3)

plot.ts(diff_data3) # Parece más estacionaria. 

#Podem fer un model on el temps sigui covariable, es a dir, agreguem la regresió
mod_reg <- auto.arima(data3, xreg=t) ### Model amb el temps com a covariable.
mod_reg
#Obtenemos una serie ARIMA(1,0,3) con intercept y coeficiente regresor del tiempo
#El coeficiente MA3 no es significativo.
coeftest(mod_reg) #AIC = 585.24, BIC=608.33 #Es peor que el modelo diferenciando

#Usamos el primer modelo
checkresiduals(auto_1) #El ljung test tenemos un pvalor de 0.1035 por lo que no 
#rechazamos la hipótesi nula de que los datos se distribuyen de manera independiente

shapiro.test(auto_1$residuals) #Rechazamos la hipótesi nula de que sean normales.


#Veamos los residuos al cuarado
plot((auto_1$residuals)^2) #No parecen estacionarios ni ruido
par(mfrow=c(1,2))
acf((auto_1$residuals)^2)
pacf((auto_1$residuals)^2)
Box.test((auto_1$residuals)^2,type="Ljung-Box") #No rechazamos la hipótesi nula de que sean independientes

#Muestra las predicciones a 30 semanas vista con el modelo ajustado con interalo de 90 y 95% de coenfianza
#Crees aue se ajusta correctamente?
pred1 <- forecast(data3, h=30, model=auto_1)
pred1
par(mfrow=c(1,1))
plot(pred1);


train= data3[1:170]
mod_autoarima2 = auto.arima(train)
mod_autoarima2
?arima
pred2 <- forecast(train, h=30, model=mod_autoarima2)
pred2
plot(pred2);
lines(data3, col="red");

#No se ajusta del todo bien.
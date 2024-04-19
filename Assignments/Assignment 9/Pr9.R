library(forecast)
library(TSA)
library(tscount)
library(tseries)
library(lmtest)

data<-read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 9 (311023)/prac9TS.txt")
data=data$x
plot.ts(data)
# A priori sembla un procés estacionari.
#Podem fer el test de Dickey-Fuler per veure si ho es
adf.test(data)
#Obtenim un pvalor menor que 0.01 per tant rebutjem la hipótesi nula, es a dir, es estacionari

#Mirem si té tendència, encara que no ho sembla
t<-1:length(data)
data.lm=lm(data~t)
summary(data.lm)
#Obtenim que la pendent no té pvalor 0.9 per tant no podem rebutjar la hipótesi nula que sigui 0, es a dir, no té tendència.

#Mirem si té estacionalitat.
plot.ts(data) #té 12 pics, l'ultim no acaba, per tant podem pensar que, com 70/12 ~ 5.83, mirem si té estacionalitat de periode 6
matrix_data=matrix(data=data[1:66], nrow=6)
matrix_data_t=t(matrix_data)

boxplot(matrix_data_t, notch=TRUE)

#Si que sembla tenir una estacionalitat de periode 6.

#Notem que si ho fem per exemple per 5 o 7
matrix_data5=matrix(data=data, nrow=5)
matrix_data_t5=t(matrix_data5)
boxplot(matrix_data_t5, notch=TRUE) #No s'aprecia res gaire significatiu

matrix_data7=matrix(data=data, nrow=7)
matrix_data_t7=t(matrix_data7)
boxplot(matrix_data_t7, notch=TRUE)
#Excepte el primer, semblen similars.

#Per tant en cas de tenir estacionalitat es de periode 6.
boxplot(matrix_data_t, notch=TRUE)

#Podem observar-ho amb la funció decompose
ts_data=ts(data,frequency=6)
decompose(ts_data)
plot(decompose(ts_data))

#No es gaire clar que hi hagi una estacionalitat.
#La estacionalitat que apareix no es gaire representativa.


#(d) Estacionalidad auto.arima?
acf(data)
pacf(data)
#Maybe AR(2) o ARMA(2,2)
mod1<-auto.arima(data)
mod1
#ens dona un ARMA(3,1) amb mitjana 25.0083 i sense estacionalitat ni tendència.


#(e)Utiliza forecast() para estimar los últimos 20 valores y dibujad la prediccón junto a la serie real
# Qué dirías de la bondad de ajuste del modelo? En cuantos puntos los valores están fuera del IC 95%?
data_50<-data[1:50] #agafem les 50 primeres per estudiar les ultimes 20
pred20<-forecast(data_50, h=20, model=mod1)
plot(pred20, type="l")
lines(51:70, data[51:70], col="red")

#Contem quantes son a fora de l'interval de confianza
df<-data.frame(real=data[51:70], lower=pred20$lower[,2], pred=pred20$mean, upper=pred20$upper[,2])
df
df$fora <- ifelse(df$real>=df$lower & df$real<=df$upper, 0,1)
table(df$fora) #per tant totes les observacions son dins de l'interval 95%.

#(f) Modelo alternativo al auto.arima
mod1
coeftest(mod1)
#Els coeficientes ar1 i ar2 ni son significativament diferent de 0.
#Mirem de fixarlos a 0. Provem també a fer un ARMA(1,1) o un MA(1)
mod2 <- arima(data, c(3, 0, 1), fixed=c(0, 0, NA, NA, NA))
mod2   #AIC=191.41
AIC(mod1) #AIC=197.0343

mod3<-arima(data, c(1, 0, 1))
mod3
# AIC=215.41 (molt pitjor)

mod4<-arima(data, c(0,0,1))
mod4 #AIC=215.11 molt pitjor també.

#Ens quedem amb el model ARMA(3,1) amb phi1=phi2=0.

# (h) comparad las predicciones de ambos modelos. Cual os parece mejor
par(mfrow=c(1,2))
pred20mod2   <- forecast(data_50, h=20, model=mod2) ### Predicci? per a les 20 darreres observacions (basada en el segon model)
plot(pred20mod2, type="l")
lines(seq(51, 70, 1), data[51:70], col="red")
pred20mod1  <- forecast(data_50, h=20, model=mod1) ### Predicci? per a les 20 darreres observacions (basada en el primer model)
plot(pred20mod1, type="l")
lines(seq(51, 70, 1), data[51:70], col="red")
#Parecen bastante similares.

(res_modpred20mod1= sqrt(mean((data[51:70]-pred20mod1$mean)^2))) #rmse=1.335634
(res_modpred20mod2= sqrt(mean((data[51:70]-pred20mod2$mean)^2))) #rmse=1.348238

#Per tant el RMSE del primer model1 es mejor que del 2, per tant el model aproxima millor les dades.

#Mirem fem checkresiduals
checkresiduals(mod1)
checkresiduals(mod2)
ks.test(res_modpred20mod1, "pnorm"); ks.test(res_modpred20mod2, "pnorm")
#En cap dels dos casos podem rebutjar independència, ni normalitat. Per tant ambodos son bons models.

#Ajustem un AR(1) per exemple amb alpha=0.8
par(mfrow=c(1,1))
mod3 <- arima(data, c(1, 0, 0), fixed=c(0.8, NA))
pred20   <- forecast(data_50, h=20, model=mod3) ### Predicció per a les 20 darreres observacions
plot(pred20, type="l")
lines(seq(51, 70, 1), data[51:70], col="red")
df3 <- data.frame(real=data[51:70], lower=pred20$lower[, 2], pred=pred20$mean, upper=pred20$upper[, 2])
df3$fora <- ifelse(df3$real >= df3$lower & df3$real <= df3$upper, 0, 1) 
table(df3$fora)
### Les observacions reals en el peri?ode predit continuen sent dins l'interval de confian?a del 95% 
### (i ara tamb? del 80%!) perqu? s'han eixamplat molt
res3 <- df3$pred-data[51:70] # Residus del tercer model
hist(res3)
ks.test(res3, "pnorm") ### Ara rebutjem la normalitat: No és un bon model!
checkresiduals(mod3)
#Rebutjem que els residuos siguen incorrelacionats, per tant no es un bon model.





## EXERCICI 2
data<-read.table("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 9 (311023)/prac8TS.txt")
data<-as.numeric(data$V3[2:61])
data<-ts(data, start=as.yearmon(as.Date("2013-01-01")), end=as.yearmon(as.Date("2017-12-01")),frequency = 12)
data
plot.ts(data)

# A priori sembla un procés estacionari.
#Podem fer el test de Dickey-Fuler per veure si ho es
adf.test(data)
#Obtenim un pvalor menor que 0.01 per tant rebutjem la hipótesi nula, es a dir, es estacionari
#En aquest cas el ACF es enganyos ja que no ho analitza en el lag 12(la nostra estacionalitat ), per tant es enganyos
#Veiem que tenim una estacionalitat de periode 12


#Mirem si té tendència, encara que no ho sembla
t<-1:length(data)
data.lm=lm(data~t)
summary(data.lm)
#Obtenim que la pendent no té pvalor 0.267 per tant no podem rebutjar la hipótesi nula que sigui 0, es a dir, no té tendència a priori
plot(decompose(data)$trend)
#No té una tendencia definida

#Mirem si té estacionalitat.
plot.ts(data) #té 5 pics, l'ultim no acaba, per tant podem pensar que te periode 12 (anual)
plot(decompose(data)$seasonal)
#Sembla tenir una estacionalitat anual
matrix_data=matrix(data=as.vector(data), nrow=12)
matrix_data_t=t(matrix_data)

boxplot(matrix_data_t, notch=TRUE) #la estacionalitat es molt clara

#Si que sembla tenir una estacionalitat de periode 12


#(d) Estacionalidad auto.arima?
acf(data)
pacf(data)   #No sembla ser un model ARMA

#Veiem que ens diu auto.arima
mod1<-auto.arima(data)
mod1
#ens dona un ARIMA(1,0,0)(1,1,0)[12] 
#Es a dir, un model amb estacionalitat de periode 12.
#Veiem si els coeficients son significatius
coeftest(mod1)
#Tots 2 coeficients son significatius

#(f) Calcula la diferencias propuestas por la funcion auto-arima y comprueba que la serie es estacionaria
data_diff=diff(data, lag=12)
plot(data_diff)

adf.test(data_diff) #Obtenim que es estacionaria (pero abans també i no ho era)
#Notem que 
plot(decompose(data_diff)) #No sembla una tendencia ni estacionalitat clares.

#Notem que 
acf(data_diff)
pacf(data_diff)
mod2=auto.arima(data_diff) #No ens dona un model estacionari. Continua tenint estacionalitat.

#(g)
mod1
mod2
#No hay ninguna diferencia.

#(h) Utiliza forecast() para dar la estimación del año 2018 y dibujalo
pred1<-forecast(data, h=12, model=mod1)
pred2<-forecast(data_diff,h=12,model=mod2)
par(mfrow=c(2,1))
plot(pred1,main = "Estimació per la sèrie original")
plot(pred1,main = "Estimació per la sèrie diferenciada")


#(i) Quedaros con los datos entre 2013 y 16. Predecir los 12 valores restantes. Cuantos caen fuera del intervalo 95%?
par(mfrow=c(1,1))
length(data)
data_1316=data[1:(4*12)]
pred20mod1  <- forecast(data_1316, h=12, model=mod1) ### Predicci? per a les 12 darreres observacions (basada en el primer model)
plot(pred20mod1, type="l")
lines(seq(49, 60, 1), data[49:60], col="red")
#Parecen bastante similares.

(res_modpred20mod1= sqrt(mean((data[49:60]-pred20mod1$mean)^2))) #rmse=2226.055

#Veamos cuantas caen fuera
df3 <- data.frame(real=data[49:60], lower=pred20mod1$lower[, 2], pred=pred20mod1$mean, upper=pred20mod1$upper[, 2])
df3$fora <- ifelse(df3$real >= df3$lower & df3$real <= df3$upper, 0, 1) 
table(df3$fora)
# 2 valors cauen fora de l'interval de confianza 95%
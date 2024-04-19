# entrega 13

#Librerías
library(forecast); library(lmtest); library(TSA)
library(fGarch); library(tseries); 
library(tidyverse);library(dplyr)

## LECTURA DE LES DADES
setwd("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/LLIURAMENT 2")
Palencia <- read.table("Palencia.csv", header=T); head(Palencia)

## PRE-PROCESADO
Palencia <- Palencia %>% dplyr::select(Periodo, Total.1)
cat("El total de valores faltantes de Total.1 es ", sum(is.na(Palencia$Total.1)))

## SEMANAS POR AÑO
semanes_per_any <-c(1, rep(0, 23)) #Ponemos la primera de 2023
a = 1
ano = 2000
for (i in 2:length(Palencia$Periodo)){
  if(substr(Palencia$Periodo[i], 1, 4)==substr(Palencia$Periodo[i-1], 1, 4)){
    semanes_per_any[a]=semanes_per_any[a]+1}
  else{
    a=a+1
    semanes_per_any[a]=semanes_per_any[a]+1}}
for (i in 1:length(semanes_per_any)){
  cat("El año ", ano+(i-1), "tiene ", semanes_per_any[i], "semanas \n")}

## DEFINICIÓN SERIE `TOTAL`
Palencia <- as.data.frame(lapply(Palencia, rev))
files_a_excluir = c(261, 522, 835, 1096, 1201:1244)
Total <- Palencia[-files_a_excluir,]
nrow(Total)

Total <- ts(Total$Total.1[521:(sum(semanes_per_any))], frequency=52, start=c(2010,01), end=c(2022,52))

## PLOT SERIE TOTAL
plot(Total, ylab="Defunciones", xlab="Semanas", main="Defunciones por semana en Palencia entre 2010 y 2022")
abline(h=mean(Total), col="red", lwd=2)
lines(ma(Total, order = 52, centre = T),col="blue", lwd=2)
legend('topleft', legend=c('Serie `Total`','Media', 'Media Móvil'), col=c('black','red', 'blue'), lty=c(1, 1,1), lwd=c(1,1.5, 1.5), cex=1)
mean(Total)

## REGRESIÓN `TOTAL` RESPECTO EL TIEMPO
t <- 1:(length(Total))
total.lm <- lm(Total ~ t)

## TEST COEFICIENTES DE REGRESIÓN
coeftest(total.lm)

## BOX-PLOT SERIE `TOTAL`
matrix_data_t=t(matrix(data=Total, nrow=52))
boxplot(matrix_data_t)

## PRUEBA ESTACIONALIDAD LIKELIHOOD
fit1 <- ets(as.vector(Total))
fit2 <- ets(as.vector(Total), model="AZN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#P value
1-pchisq(deviance,df)  #H0: No es estacional / H1: es estacional

## ESTACIONALIDAD (DECOMPOSE())
plot(decompose(Total)$seasonal, ylab="Estacionalidad en las Defunciones", xlab="Semanas", main="Componente estacional de la serie `Total`")

## DEFINICIÓN SERIE `TOTAL_DIFF1_52`
Total_diff1_52 <- diff(diff(Total), lag=52)

## PLOT SERIE `TOTAL_DIFF1_52`
plot.ts(Total_diff1_52, ylab="Diferencia de Defunciones", xlab="Semanas", main="Diferencias de orden 1 y posteriormente 52 de \n las Defunciones por semana en Palencia entre 2010 y 2022")
abline(h=mean(Total_diff1_52), col="red", lwd=2)
lines(ma(Total_diff1_52, order = 52, centre = T),col="blue", lwd=2)
legend('topleft', legend=c('Serie `Total_diff1_52`','Media', 'Media Móvil'), col=c('black','red', 'blue'), lty=c(1, 1,1), lwd=c(1,1.5, 1.5), cex=1)

## BOX-PLOT SERIE `TOTAL_DIFF1_52`
matrix_data_t=t(matrix(data=Total_diff1_52, nrow=52))
boxplot(matrix_data_t)

## PRUEBA ESTACIONALIDAD LIKELIHOOD
fit1 <- ets(as.vector(Total_diff1_52))
fit2 <- ets(as.vector(Total_diff1_52), model="AZN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#P value
1-pchisq(deviance,df)  

## ESTACIONALIDAD (DECOMPOSE())
plot(decompose(Total_diff1_52)$seasonal, ylab="Estacionalidad en las Diferencias de Defunciones", xlab="Semanas", main="Componente estacional de la serie `Total_diff1_52`")

## TEST AUGMENTED DICKEY FULLER A SERIE `TOTAL_DIFF1_52`
adf.test(Total_diff1_52)

## EACF, ACF, PACF `TOTAL_DIFF1_52`
eacf(Total_diff1_52)
par(mfrow=c(2,1))
acf(Total_diff1_52)
pacf(Total_diff1_52, main="")
par(mfrow=c(1,1))

## MODELO auto.arima() PARA LA SERIE `TOTAL`
mod1 <- auto.arima(Total)

## TEST PARA LOS COEFICIENTES DEL MODELO
coeftest(mod1)

## ESTUDIO DE LOS RESIDUOS PARA EL MODELO
checkresiduals(mod1)

## SHAPIRO WILK TEST PARA LOS RESIDUOS
shapiro.test(mod1$residuals) 

## PREDICCIONES 2023 CON EL MODELO
pred1 <- forecast(mod1, h=52)

## DEFINICIÓN DE LOS DATOS DE DEFUNCIÓN 2023
real <- ts(tail(Palencia,44)$Total.1, frequency=52, start=c(2023,01), end=c(2023,44))

## VISUALIZAMOS LA PREDICCIÓN
plot(pred1, xlab="Semanas", ylab="Defunciones", main="Predicciones del modelo ARIMA(1,1,2)(0,0,1)[52] \n con drift para 2023")

plot(pred1$mean, type='l', lwd=2, col='blue', xlab='Semanas', ylab='Valores Predichos con IC', main='Valores Predichos con Intervalos de Confianza', ylim=c(0, 100))
lines(pred1$lower[,2], col='black', lty=3, lwd=2) 
lines(pred1$upper[,2], col='black', lty=3, lwd=2) 
lines(real, col='red', lwd=2)
legend('topleft', legend=c('Valores Reales','Valores Predichos', 'Intervalo de Confianza'), col=c('red','blue', 'gray'), lty=c(1, 1,2), lwd=c(2,2, 1), cex=0.8)

## ESTUDIO DE LOS RESIDUOS (continuación)

pacf(mod1$residuals)

plot((mod1$residuals)^2, ylab="Residuos al cuadrado", xlab="Semanas")
par(mfrow=c(2,1))
acf((mod1$residuals)^2, main="Serie Residuos al Cuadrado")
pacf((mod1$residuals)^2, main="")

par(mfrow=c(1,1))

## MODELOS GARCH
mod3 <- garch(Total, order=c(0, 1))
mod4 <- garch(Total, order=c(1, 1))
mod5 <- garch(Total, order=c(1, 2))

## AIC de los modelos
c(AIC(mod3), AIC(mod4), AIC(mod5))

## MEJOR MODELO GARCH
mod4 <- garchFit(~garch(1,1), Total)
coef(mod4)

## AJUSTE AÑADIENDO REGRESOR COVID
COVID<-c(rep(0,531), rep(1,93), rep(0,52)) 

## REGRESIÓN `TOTAL` RESPECTO A `COVID`
Total.lm2 <- lm(Total~COVID)

## TEST PARA LOS COEFICIENTES DEL MODELO
coeftest(Total.lm2)

## MODELO auto.arima() AÑADIENDO EL REGRESOR COVID
mod_cov <- auto.arima(Total, xreg=COVID)

## TEST PARA LOS COEFICIENTES DEL MODELO
coeftest(mod_cov)

## FIJAMOS QUE EL COEFICIENTE ar1 SEA 0
mod_cov <- arima(Total, order=c(2,0,2), seasonal=list(order=c(0,0,1), period=52), xreg=COVID, include.mean=TRUE, fixed=c(0, NA, NA, NA, NA, NA, NA))

## TEST PARA LOS COEFICIENTES DEL MODELO
coeftest(mod_cov)

## ESTUDIAMOS LOS RESIDUOS
checkresiduals(mod_cov)

## PREICCIONES 2023
pred2 <- forecast(mod_cov, h = 52, xreg=numeric(52))

## PLOT BONDAD DE AJUSTE
plot(pred2$mean, type='l', lwd=2, col='blue', xlab='Semanas', ylab='Valores Predichos con IC', main='Valores Predichos con Intervalos de Confianza', ylim=c(0, 100))
lines(pred2$lower[,2], col='black', lty=3, lwd=2)  
lines(pred2$upper[,2], col='black', lty=3, lwd=2) 
lines(real, col='red', lwd=2)
legend('topleft', legend=c('Valores Reales','Valores Predichos', 'Intervalo de Confianza'), col=c('red','blue', 'gray'), lty=c(1, 1,2), lwd=c(2,2, 1), cex=0.8)



## RMSE DE LOS DOS MODELOS ARIMA
rmse1 = sqrt(mean(((pred1$mean)[1:44] - real)^2))
rmse2 = sqrt(mean(((pred2$mean)[1:44] - real)^2))
c(rmse1, rmse2)

## CÁLCULO DEL R2 DE LOS DOS MODELOS ARIMA
SSR1 = sum(((pred1$mean)[1:44] - real)^2)
SST1 = sum((mean(real)-real)^2)
SSR2 = sum(((pred2$mean)[1:44] - real)^2)

c(1- SSR1/SST1, 1- SSR2/SST1)



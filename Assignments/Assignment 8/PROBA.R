### PRÁCTICA EVALUABLE SERIES TEMPORALES ###
require(zoo)
library(tseries)
library(lmtest)
library(forecast)
library(TSA)
library(stats)
library(ggplot2)
library(dplyr)

datos<-read.table(file = "C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/LLIURAMENT/data.txt", header = FALSE, dec=",")
datos                           ## me aparecen en orden inverso
datos.nac<-read.table(file = "C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/LLIURAMENT/data_nac.txt", header = FALSE, dec=",")
datos.nac

datos<-datos$V1
datos.nac<-datos.nac$V1

(datos <- rev(as.numeric(datos))) #Los datos venían al revés en orden cronológico
(datos.nac <- rev(as.numeric(datos.nac)))

start=as.yearmon(as.Date("2000-01-01"))
end=as.yearmon(as.Date("2023-04-01"))

datos.ts=ts(datos,start=start, end=end, frequency = 4)
datos.ts
datos.nac.ts=ts(datos.nac,start=start, end=end, frequency = 4)
datos.nac.ts


#Observamos que hay tres tendencias distintas.
#El primer trimestre crece hasta 2009 aunque 2011 es mayor a 2009 y ya baja
#El segundo y cuarto hasta 2011 y el 3o hasta 2009 -> dejamos 2011 pues el 4to aun crece

# Luego se queda estable/baja hasta 2018 y de 2019 a 2023 sube

###############################################################
# Crear el gráfico
par(mfrow=c(1,1))
plot(datos.ts, xlab="Trimestre-Año", ylab="Coste", col="slateblue2", pch=19, col.axis="black", lwd=1.5)
title("Andalucia")
points(datos.ts, col="aquamarine3")
abline(v=2000, col="springgreen", lwd=1.5)
abline(v=2009+0.75, col="springgreen", lwd=1.5)
abline(v=2010, col="orange", lwd=1.5)
abline(v=2020+0.25, col="orange", lwd=1.5)
abline(v=2020+0.5, col="springgreen", lwd=1.5)

# Agregar puntos oscuros en el primer trimestre de cada año
trimestre <- rep(1:4, length.out = length(datos.ts))
puntos_min <- ifelse(trimestre == 1, "blue", "aquamarine2")
puntos_max <- ifelse(trimestre == 4, "springgreen1", "aquamarine2")
points(datos.ts, pch=ifelse(trimestre == 1, 19, 1), col=puntos_min)
points(datos.ts, pch=ifelse(trimestre == 4, 19, 1), col=puntos_max)
legend("bottomright", legend=c("Primer Trimestre","Segundo/Tercer Trimestre","Cuarto Trimestre"), pch=c(19,1,19), col=c("blue", "aquamarine2","springgreen1"), title="Trimestre")
##############################################################

# Hacemos lo mismo para los nacionales
###############################################################
# Crear el gráfico
plot(datos.nac.ts, xlab="Trimestre-Año", ylab="Coste", col="slateblue", pch=19, col.axis="white")
title("Nacional")
points(datos.nac.ts, col="aquamarine3")
abline(v=2000, col="springgreen", lwd=1.5)
abline(v=2009+0.75, col="springgreen", lwd=1.5)
abline(v=2010, col="orange", lwd=1.5)
abline(v=2020+0.25, col="orange", lwd=1.5)
abline(v=2020+0.5, col="springgreen", lwd=1.5)

# Agregar puntos oscuros en el primer trimestre de cada año
trimestre <- rep(1:4, length.out = length(datos.nac.ts))
puntos_min <- ifelse(trimestre == 1, "blue", "aquamarine2")
puntos_max <- ifelse(trimestre == 4, "springgreen1", "aquamarine2")
points(datos.nac.ts, pch=ifelse(trimestre == 1, 19, 1), col=puntos_min)
points(datos.nac.ts, pch=ifelse(trimestre == 4, 19, 1), col=puntos_max)
##############################################################



# Fijamos los datos de cada periodo
tend1 <- datos.ts[1:(4*12)]
tend2 <- datos.ts[(4*12+1):(4*20+2)]
tend3 <- datos.ts[(4*20+3):94] 

# Nos quedamos pues los datos a partir de cuando la tendencia es creciente
start=as.yearmon(as.Date("2020-07-01"))
data.ts=ts(tend3,start=start, end=end, frequency = 4)
# Crear el gráfico

plot(data.ts, xlab="Trimestre-Año", ylab="Coste", main="Andalucía", col="slateblue2", pch=19, col.axis="black", lwd=1.5)
points(data.ts, col="aquamarine3", lwd=1.5)
trimestre <- rep(c(3,4,1,2), length.out = length(data.ts))
trimestre
puntos_min <- ifelse(trimestre == 1, "blue", "aquamarine2")
puntos_max <- ifelse(trimestre == 4, "springgreen1", "aquamarine2")
points(data.ts, pch=ifelse(trimestre == 1, 19, 1), col=puntos_min)
points(data.ts, pch=ifelse(trimestre == 4, 19, 1), col=puntos_max)
nombres_tics <- c("T3-2020", "T4-2020", "T1-2021", "T2-2021", "T3-2021", "T4-2021", "T1-2022", "T2-2022", "T3-2022", "T4-2022", "T1-2023", "T2-2023")
ubicaciones_tics <- seq(1, length(data.ts))
axis(1, at = ubicaciones_tics, labels = nombres_tics)

# 1er MODEL, MODEL DE REGRESIÓ + ESTACIONALITAT

#Observem que obviament les dates tenen component estacional.
(seasonal <- decompose(data.ts)$seasonal)
# C1=-88.39953  , C2= 57.05359 , C3= -41.75578  , C4=73.10172

# Ara, un cop restem la component estacional, estimem una regresió
data_noseasonal <- newdata.ts-seasonal
plot(data_noseasonal) #Observem la tendencia creixent llevat l'any el trimestre de la covid

#Estimem una regresió lineal.
t <- 1:12
(data.lm <- lm(as.numeric(data_noseasonal)~t))
int <- 2285.72                
m <- 28.52      

#Veiem com s'ajusta a les dades i la predicció dels pròxims dos anys
ggdata= data.frame(
  data=as.numeric(data.ts),
  Trim=c("20.Qtr3","20.Qtr4","21.Qtr1","21.Qtr2","21.Qtr3","21.Qtr4","22.Qtr1","22.Qtr2","22.Qtr3","22.Qtr4","23.Qtr1","23.Qtr2"),
  trim=1:12
  )
ggpred= data.frame(
  Date_pred=13:14,
  Date_Pred=c("23.Qtr3","23.Qtr4"),
  Seas_pred=seasonal[1:2]
)
ggplot(data=ggdata, aes(x=Trim, y=data))+
  geom_point(color="red3")+geom_line(color="red", group=1) +  
  theme_minimal()+
  xlab("Data") + ylab("PreuxTraballador") +
  geom_abline(slope=m, intercept=int, col="deepskyblue", lwd=0.5)+
  geom_point(aes(x=Trim, y=int+m*trim + seasonal), col="deepskyblue3")+
  geom_line(aes(x=Trim, y=int+m*trim + seasonal), col="deepskyblue3", group=1)+
  geom_point(aes(x=ggpred$Date_Pred[1], y=int+m*ggpred$Date_pred[1] + ggpred$Seas_pred[1]), col="green")+
  geom_point(aes(x=ggpred$Date_Pred[2], y=int+m*ggpred$Date_pred[2] + ggpred$Seas_pred[2]), col="green")+
  geom_segment(aes(x=ggpred$Date_Pred[1], y=int+m*ggpred$Date_pred[1] + ggpred$Seas_pred[1], xend=ggpred$Date_Pred[2], yend=int+m*ggpred$Date_pred[2] + ggpred$Seas_pred[2]), col="green")+
  geom_segment(aes(x=Trim[12], y=int+m*trim[12] + seasonal[4], xend=ggpred$Date_Pred[1], yend=int+m*ggpred$Date_pred[1] + ggpred$Seas_pred[1]), col="green")

#Calculem els residus
summary(data.lm)
#R2 = 0.9759  -> Model molt bo

## RSE= sqrt(SSE/(n-p-1) (n-p-1=10) = 16.94
## MSE ~ sigma^2 = 286.9636


#### HAGAMOS AHORA UN auto.arima PARA ESTOS DATOS,
plot(data.ts)
#Notamos que no es estacionario, tiene tendencia y estacionalidad. La estacionalidad es de log=4.
data4 <- diff(data.ts, lag=4)
data4
plot(data4)          #Notamos que perdemos la tendencia y además los datos parecen estacionarios.
#Además, teníamos una tendencia creciente por lo que diferenciamos de nuevo los datos
data_f = diff(data4,lag=1)
plot(data_f)

acf(data4) #sembla soroll blanc
pacf(data4) # sembla soroll blanc

#veiem que ens recomana la funció auto.arima
fit1=auto.arima(data.ts)       #Ens recomana un ARIMA(0,0,0)(0,1,0)[4] amb drift= 28.2297 amb AIC=77.96 , BIC=75.72
fit1
#Comparem amb altres models
(fit2<-arima(data.ts, order=c(0,1,0), seasonal=list(order=c(0,1,0), period=4))) #AIC=64.56, BIC=66.5105, sigma^2 = 593.3
BIC(fit2)
(fit3<-arima(data.ts, order=c(0,1,0), seasonal=list(order=c(1,1,0), period=4)))#AIC=65.67, BIC=67.56673, SAR(1)=0.4586
BIC(fit3)
(fit4<-arima(data.ts, order=c(0,1,1), seasonal=list(order=c(0,1,0), period=4)))#AIC=66.28, BIC=68.16836, MA(1)=-0.2590
BIC(fit4)
(fit5<-arima(data.ts, order=c(1,1,0), seasonal=list(order=c(0,1,0), period=4)))#AIC=66.34, BIC=68.23125, AR(1)= -0.1902
BIC(fit5)
(fit6<-arima(data.ts, order=c(0,1,0), seasonal=list(order=c(0,1,1), period=4)))#AIC=65.67, BIC=67.56614 SMA(1)=0.6567
BIC(fit6)
(fit7 <- arima(data.ts, c(0,1,1)))    # AIC=137.58, BIC=140.37, MA(1)=-0.5214
BIC(fit7)
(fit8 <- arima(data.ts, c(1,1,0)))    # AIC=126.26, BIC=129.0558, AR(1)=-0.9216
BIC(fit8)

#Per tant el millor model es un ARIMA(0,1,0)(0,1,0)[4]  ->fit2


#El  model fit2 és el que té millor AIC i millor BIC.
#Checkejem els residus d'aquest
checkresiduals(fit1) #pvalor =  0.6147 #No podem rebutjar la hipótesi nula que els residus siguin independents i es clar a l'ACF que son incorrelacionats
shapiro.test(fit2$residuals) #En aquest test, NO podem rebutjar la hipótesi nula que siguien normals
# pvalor =0.17 
# Predim valors amb aquest
### Hagamos una predicción con nuestro modelo de los últimos datos para ver si se ajustan a los reales
train_serie <- data.ts
pred1 <- forecast(train_serie, h=2, model=fit1)
pred1
plot(pred1)

#Sigue creciendo.




#### SUAVITZAT EXPONENCIAL DE HOLT-WINTERS
#Com el el mètode de Holt_Winters genera un model amb bons pronòstics a curt termini pot ser bon

hw_fit = HoltWinters(data.ts, seasonal = "additive")
pred = predict(hw_fit, n.ahead = 2, prediction.interval = TRUE, level=0.95)
plot(hw_fit, pred)
hw_fit
# a=2654.45630, b=30.74129, s1=-37.71219, s2=80.58781, s3=-98.00469, s4=55.12906
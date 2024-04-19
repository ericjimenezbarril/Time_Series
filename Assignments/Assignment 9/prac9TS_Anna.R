

### Practica 9: Bondat d'ajust i selecció del model
library(forecast)
library(TSA)
library(tscount)
library(tseries)


### Practica
dades <- read.table("/Users/POR740051/Desktop/UOC/SeriesTemporalsUAB/prac9TS.txt", header=T)
head(dades)
dades

#### Apartat a)
### Dibuixeu la serie. És un procés estacionari? Per què?
ts.plot(dades$x) 

### Sembla un procés estacionari.  
### Anem a fer el test Augmentat de Dickey-Fullert

adf.test(dades$x) 

### El test ens diu que sí que és un procés estacionari
### https://www.ugr.es/~montero/matematicas/cointegracion.pdf

#### Apartat b)
### Té tendència? 
### No sembla que tingui tendència
dades1<-dades$x
dades1
t= 1:70
t
dades1.lm=lm(dades1~t)
summary(dades1.lm)
#El coefficient que acompanya a la variable independent t surt no significatiu;  

### No sembla tenir tendència


#### Apartat c)


dades$x
matrix_dades = matrix(data = dades$x[1:68], nrow = 6)
matrix_dades
matrix_dades_t = t(matrix_dades)

matrix_dades_t
 
boxplot(matrix_dades_t, notch = TRUE)

ts_dades = ts(dades$x, frequency = 6)
decompose(ts_dades)
plot(decompose(ts_dades))

#### Apartat d)

# Correlograma
par(mfrow=c(2,1))
acf(dades$x)
# Correlacions parcials
pacf(dades$x)

### Quina és la estacionalitat que proposa auto-arima()?
mod1 <- auto.arima(dades$x) 
mod1
### auto.arima proposa un ARMA(3,1) i per tant, sense cap estacionalitat.

#### Apartat e)
#### Fent servir...
# Funció per borrar la finestra de gràfics
dev.off()
### Predicció per a les 20 darreres observacions
dades_50 <- dades$x[1:50] ### Em quedo amb les primeres 50 observacions, per dibuixar-les desprÃ©s
head(dades_50)
pred20 <- forecast(dades_50, h=20, model=mod1)
pred20
plot(pred20, type="l")

### Pintem a la gráfica els valors correctes
lines(seq(51, 70, 1), dades$x[51:70], col="red")

### Comptem en quants casos el valor real està fora de l'interval de confiança del 95%:
df <- data.frame(real=dades$x[51:70], lower=pred20$lower[, 2], pred=pred20$mean, upper=pred20$upper[, 2])
df 

### Creem una nova variable que indica si l'observació està fora de l'interval 
df$fora <- ifelse(df$real >= df$lower & df$real <= df$upper, 0, 1) 
df$fora
### I comptem
table(df$fora)

### Veiem que totes les observacions cauen dins de l'interval del 95%.

#### Apartat f)
mod1; # Veiem que els coeficients AR1 i AR2 NO son significativament diferents de zero, 
      #de manera que podem plantejar un model alternatiu fixant el valor d'aquests paràmetres a zero:
mod2 <- arima(dades$x, c(3, 0, 1), fixed=c(0, 0, NA, NA, NA))
mod2

#### Apartat g)
AIC(mod1); AIC(mod2) ### El segon model té millor AIC (menor), per tant ens quedar­em amb aquest.

#### Apartat h)
par(mfrow=c(1,2))
pred20mod1   <- forecast(dades_50, h=20, model=mod2) ### Predicció per a les 20 darreres observacions (basada en el segon model)
plot(pred20mod1, type="l")
lines(seq(51, 70, 1), dades$x[51:70], col="red")
pred20mod2  <- forecast(dades_50, h=20, model=mod1) ### Predicció per a les 20 darreres observacions (basada en el primer model)
plot(pred20mod2, type="l")
lines(seq(51, 70, 1), dades$x[51:70], col="red")

### Apartat j)
mod3 <- arima(dades$x, c(4, 0, 2))
mod3

par(mfrow=c(1,2))
pred20mod2 <- forecast(dades_50, h=20, model=mod2) ### Predicció per a les 20 darreres observacions (basada en el segon model)
plot(pred20mod2, type="l")
lines(seq(51, 70, 1), dades$x[51:70], col="red")
pred20mod3 <- forecast(dades_50, h=20, model=mod3) ### Predicció per a les 20 darreres observacions (basada en el primer model)
plot(pred20mod3, type="l")
lines(seq(51, 70, 1), dades$x[51:70], col="red")



### Comptem en quants casos el valor real està  fora de l'interval de confiança del 95%:
df2 <- data.frame(real=dades$x[51:70], lower=pred20mod2$lower[, 2], pred=pred20mod2$mean, upper=pred20mod2$upper[, 2])
df2$fora <- ifelse(df2$real >= df2$lower & df2$real <= df2$upper, 0, 1) ### Nova variable que indica si l'observaciÃ³ estÃ  fora de l'interval 
table(df2$fora)

df3<- data.frame(real=dades$x[51:70], lower=pred20mod3$lower[, 2], pred=pred20mod3$mean, upper=pred20mod3$upper[, 2])
df3fora <- ifelse(df2$real >= df2$lower & df2$real <= df2$upper, 0, 1) ### Nova variable que indica si l'observaciÃ³ estÃ  fora de l'interval 
table(df3fora)


### Les prediccions produïdes pels dos models són molt similars, 
### ens quedar­em amb el segon model perquè té menys paràmetres, millor AIC i 
### proporciona estimacions molt similars.

#### Apartat i)
res1 <- df$pred-dades$x[51:70] # Residus del primer model
res2 <- df2$pred-dades$x[51:70] # Residus del segon model
par(mfrow=c(1,2))
  hist(res1)
  hist(res2)
### Funció per borrar la finestra de gràfics
dev.off()

### Els residus del segon model s'acosten més a la normalitat, 
### però amb un test de K-S no podem rebutjar la normalitat amb cap dels dos models:
ks.test(res1, "pnorm"); ks.test(res2, "pnorm")

### Chequejem els residus del model
checkresiduals(mod1);
# Normalitat dels residus
ks.test(mod1$residuals, "pnorm");
### ### Soroll blanc: https://rpubs.com/Meca/376836 -> H0: És soroll blanc
Box.test(mod1$residuals)


#### Apartat j)
### Ajustem per exemple un AR(1) amb alpha=0.8
mod3 <- arima(dades$x, c(1, 0, 0), fixed=c(0.8, NA))
pred20   <- forecast(dades_50, h=20, model=mod3) ### PredicciÃ³ per a les 20 darreres observacions
plot(pred20, type="l")
lines(seq(51, 70, 1), dades$x[51:70], col="red")
df3 <- data.frame(real=dades$x[51:70], lower=pred20$lower[, 2], pred=pred20$mean, upper=pred20$upper[, 2])
df3$fora <- ifelse(df3$real >= df3$lower & df3$real <= df3$upper, 0, 1) 
table(df3$fora)
### Les observacions reals en el peri­ode predit continuen sent dins l'interval de confiança del 95% 
### (i ara també del 80%!) perquè s'han eixamplat molt
res3 <- df3$pred-dades$x[51:70] # Residus del tercer model
hist(res3)
ks.test(res3, "pnorm") ### Ara rebutjem la normalitat: No Ã©s un bon model!


eacf(dades$x)




### Simulació de les dades. No executar
x <- arima.sim(model=list(ar=c(0.7, -0.6), ma=c(-0.2)), n=70)+25
write.table(x, "/Users/prac9TS.txt")


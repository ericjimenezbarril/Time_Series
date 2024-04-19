library(forecast)
library(tseries)
library(TSA)
library(fGarch)

### Exercici 1



#Carregar les dades del PIB del EEUU per Trimestre, des del 1947 fins al 2001
setwd("C:/Users/ERIC/Desktop/5. SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 11(211123)/")
dades1 <- read.table("prac11TS.txt", sep=";", header=T)
head(dades1)
length(dades1)

### Apartat a)
### La convertim en un vector ts
dades1 <- ts(dades1$x, start=c(1947, 1), end=c(2001, 4), freq=4)
dades1

### Fem el gr?fic
ts.plot(dades1)

## Test de Dickey-Fuller
adf.test(dades1) 
### No ?s estacionari, el test ho confirma.


### Apartat b)
#### No sembla tenir tend?ncia 

### Apartat c)
###  ni estacionalitat clara, 
### malgrat les dades tinguin una freq??ncia trimestral

### Per comprovar-ho, un truc basat en el likelihood ratio test
fit1 <- ets(dades1)
fit2 <- ets(dades1, model="AZN")
deviance <- 2*c(logLik(fit1) - logLik(fit2))
df <- attributes(logLik(fit1))$df - attributes(logLik(fit2))$df 
#P value
1-pchisq(deviance,df) ### H0: No ?s estacional / H1: ?s estacional
La component estacional no ?s significativa.

### Apartat d)
mod1 <- auto.arima(dades1) 
mod1

mod2 <- arima(dades1, order=c(1,0,3), fixed=c(NA, NA, 0, NA, NA) ) # Model proposat per auto.arima
mod2



### Ens d?na un ARMA(1,3)
### auto.arima confirma que no cal diferenciar i que no hi ha estacionalitat: Proposa un ARMA(1, 3)

### En base a l'ACF i PACF
# Correlograma
par(mfrow=c(2,1))
acf(dades1)
# Correlacions parcials
pacf(dades1)


### Sembla un model raonable? Donem una ullada als residus del model
checkresiduals(mod2);

### Soroll blanc: https://rpubs.com/Meca/376836
### H0: ?s soroll blanc
Box.test(residuals(mod2))

tsdiag(mod2)

#################################
#### En la pr?ctica 11 vam arribar fins aqu?.
#### Vam trobar que la nostra s?rie Dades1 seguia un ARMA(1,3)
#### Dades1(t) = Y(t)
#### Y(t) = 0,2433 -0.6311 * Y(t-1) + 0.5106 * E(t-1) - 0.0461 * E(t-2) + 0.1744 * E(t-3) + E(t)
#### On els residus E(t) segueixen una normal amb esperan?a 0 i vari?ncia 3.214

    
#### Posteriorment en l'apartat e) vam veure que els residus eren normals. Independents. 
#### Per? quan vam anar a mirar els residus al quadrat v?rem veure que no eren independents.
#### Si no que seguien una estructura.

### Apartat e)

### Els residus quadrats no son independents!
Box.test(residuals(mod2)^2, type="Ljung-Box") 

# Correlograma
par(mfrow=c(2,1))
acf(residuals(mod2)^2)
# Correlacions parcials
pacf(residuals(mod2)^2)

eacf(residuals(mod2)^2)




# Com la funci? acf va decaient exponencialment a 0. I la pacf nom?s t? un valor diferent de 0,
#### Pel acf i pacf sembla que segueixi un GARCH(1,0)
### Recordem que ?s un GARCH

### Com els residus al quadrat sembla que tinguin una estructura autoregressiva. 
#### De fet, observem una AR(1). Ens fa pensar que el model de la serie no ?s una ARMA(1,3).
#### Si no que sembla un model GARCH

### Apartat f)
###Ajustem GARCH

### Primer que ho ajusti la funci? garch de R directament
mod1_prova <- garch(dades1)

mod1_prova
mod1_1 <- garchFit(~garch(1, 1), data=dades1)
mod1_1


### El model que proposa ?s un GARCH(1,1)
#### Model: Y(t) = 0.06902 + E(t) on E(t) segueix una normal amb esperan?a 0 i vari?ncia sigma^2(t)
#### on sigma^2(t) = 0.58112 + 0.72553 * E(t-1) + 0.02280 * sigma^2(t-1)

### La funci? garch ens est? dient que un bon model seria un GARCH(1,1)

### Nosaltres ajustarem un GARCH(0, 1) pel que hav?em observat en les funcions acf i pacf

mod2 <- garch(dades1, order=c(0, 1))
mod2

mod2_1 <- garchFit(~garch(1, 0), data=dades1)
mod2_1
mod2_3
mod2

mod2_3 <- garchFit(~garch(1, 0), data=dades1, include.mean = TRUE)
mod2_3

#### Model: Y(t) = E(t) on E(t) segueix una normal amb esperan?a 0 i vari?ncia sigma^2(t)
#### on sigma^2(t) = 0.61279 + 0.7295 * E(t-1) 


mod3 <- garch(dades1, order=c(0, 1), include.mean = FALSE)
mod3

mod1
mod2
mod3

### Apartat g)

AIC(mod1) 
AIC(mod2) 
AIC(mod3)

### El segon model ?s millor.
 
### Alternativa fent servir garchFit. La notaci? ?s diferent!
### El model amb garchFit no ?s directament comparable, l'AIC est?? "normalitzat"!
### Per calcular l'AIC del model que proposa garchFit, necessitem treballar una mica m?s:
mod2_1@fit$value 

### Aqu? es guarda la (menys) log-versemblan?a.
length(mod2_1@fit$params$index) 
### Aix?? podem tenir el nombre de par?metres que s'han estimat. Per tant, l'AIC el podem calcular a ma:
AIC_Mod2 <- 2*mod2_1@fit$value + 2*length(mod2_1@fit$params$index) 
AIC_Mod2
### Molt similar al mod1, encara que el mod1 ?s una mica pitjor, per tant a partir d'ara em quedo amb mod2.

### Apartat h)

# Veiem ara com es comporten els residus del model al quadrat i 
### veurem que el patr? que observavem amb el model ARMA ja no hi ?s.
par(mfrow=c(1,2))
acf(residuals(mod2)[2:length(residuals(mod2))]^2); 
pacf(residuals(mod2)[2:length(residuals(mod2))]^2)


Box.test(residuals(mod2)^2, type="Ljung-Box") 
### Els residus quadrats son independents! Definitivament, aquest model ?s millor

### I aix? mateix si ho mrem pels residus
par(mfrow=c(1,2))
acf(residuals(mod2)[2:length(residuals(mod2))]); 
pacf(residuals(mod2)[2:length(residuals(mod2))])


Box.test(residuals(mod2), type="Ljung-Box") 



###
pred <- predict(mod2)
head(pred)
par(mfrow=c(1,1))
ts.plot(dades1)
lines(seq(1947, 2001.75, 0.25), pred[, 1], col="red")
lines(seq(1947, 2001.75, 0.25), pred[, 2], col="red")

### Comptem en quants casos el valor real est??fora de l'interval de confian?a del 95%:
df <- data.frame(real=dades1, lower=pred[, 2], upper=pred[, 1])
head(df) 

## Creem una nova variable que indica si l'observaci? est? fora de l'interval 
df$fora <- ifelse(df$real >= df$lower & df$real <= df$upper, 0, 1) 
head(df$fora)
### I comptem
table(df$fora)


#### Un 36% dels casos ens cauen a fora de l'interval de confian?a

### Exemple amb dades del preu de l'acci? SONY: 
### https://rpubs.com/Bernlc82/555323


### Apartat i)
### Per comprovar que no sempre els residus al quadrat tenen una estructura, provem el següent
prova <- arima.sim(model=list(ar=0.7), n=10000) ### AR(1) simulat
mod.prova <- auto.arima(prova)

Box.test(residuals(mod.prova), type="Ljung-Box")
Box.test(residuals(mod.prova)^2, type="Ljung-Box")

par(mfrow=c(1,2))
  acf(residuals(mod.prova)^2); pacf(residuals(mod.prova)^2)


### Veiem que en aquest cas ni els residus ni els residus al quadrat semblen tenir una estructura, i el test de Ljung-Box confirma que
### els residus son independents.

#### PRÁCTICA 2 ####
## EXTRACCIÓN DE LA TENDENCIA Y LA ESTACIONALIDAD CON R ##


# DURANTE LA PRÁCTICA HAREMOS PASO A PASO Y REPRODUCIREMOS EL PROCEDIMIENTO QUE HAY DENTRO DE LA FUNCIÓN "DESCOMPOSE"
# PERMITE AISLAR LA COMPONENTE "Estacional, la tendencia, y un ruido".



### 1. DESCOMPOSICIÓN ADITIVA
# La "variación estacional" parece constante (No crece cuando crecen los valores de la serie)
# Usaremos la descomposición aditiva
# Xt=mt+st+Yt
#mt: componente estacional; st:tendencia; Yt: ruido.

### EJEMPLO ###
install.packages("fpp")
library("fpp")
ausbeer #conjunto de datos de fpp; producción trimestral en Austria de cerveza en megalitros entre 1956 1er quatri hasta 2008 3er quatri.

#Veamos que la componente estacional No parece cambiar con la magnitud de observaciones
data(ausbeer)
head(ausbeer)
tail(ausbeer)
plot.ts(ausbeer)
ausbeer

#PREGUNTA 1: ¿Qué periodicidad presentan los datos?
# Los datos tienen una periodicidad trimestral. Van del primer trimestre de 1956 al tercero de 2008

# 2: La serie presenta estacionalidad?
# -> Sí. La serie presenta un ciclo trimestral que se repite anualmente. Los picos anuales se dan en el 4to semestre todos los años.

# 3: ¿Dirías que existe una tendencia creciente o decreciente en el tiempo?
# -> La serie presenta una tendencia creciente los primeros años hasta llegar a un nivel a partir del cual se mantiene con cierta estabilidad, ni crece ni decrece.

# 4: ¿De qué año a qué año observas un cambio de tendencia?
# -> En el gráfico de la serie observamos que el cambio se produce entre 1970 y 1980. Podemos realizar zoom para ver el cambio
library(quantmod)
ggseasonplot(head(tail(ausbeer, 40*4+2),15*4))

##Se observa que el cambio de tendencia viene a partir de 1974



### PARA LOS SIGUIENTES PASOS NOS QUEDAMOS CON UN TROZO CORTO DE LA SERIE
# Q3 de 1957 y Q2 de 1973 para evitar el cambio de tendencia
ausbeerrec=tail(head(ausbeer,17*4+2),17*4-4)


### USO DE head(), tail()
x=1:100
head(x,19) #Se queda con los n primeros datos
tail(x, 29) #Se queda con los últimos 29
################################################
#Por ese motivo, queremos hasta Q2 de 1973, comienza en Q1 de 1956, hay 17 años hasta Q4 de 1972 + 2 Q de 1973 (17*4+2)
#Una vez tenemos estos datos, tenemos que quitarnos los 6 primeros, como tenemos 17*4+2, cogemos los 17*4-4 últimos
############


plot.ts(ausbeerrec, col="navy", lwd=2)





### PASO 2: DETECTAMOS LA TENDENCIA (Trend) ####

### AJUSTE DE LA TENDENCIA CON REGRESIÓN LINEAL
#Para detectar la tendencia podremos utilizar un modelo de regresión.
# Xt=B0+B1*t+Yt        (Yt es el ruido)


### PRÁCTICA 1.2

#(i) Pasemos la serie a vector: ausbeerrec_num
ausbeerrec_num=unclass(ausbeerrec)
ausbeerrec_num
ausbeerrec
?unclass


# (ii) Crear un vector de la misma longitud con los valores (1,2,….) que será nuestro vector tiempo.
t=(1:length(ausbeerrec_num)); 
length(t)==17*4-4 #sabiamos la dimensión 

# (iii) Construid un modelo de regresión donde ausbeerrec_num es la variable explicada y t la explicativa.
ausbeerrec.lm=lm(ausbeerrec_num~t)
summary(ausbeerrec.lm) 

#La recta obtenida es Yt=235.7336+2.9769t+Rt 
#Rt e ruido. Sabemos que el ajuste es bueno ya que
#Los pvalores son menores a 0.01 (por tanto la constante y coeficiente que acompañan la t son significativos)
#El coeficiente de determinación de la recta es 66.02% por lo que el tiempo explica un 66% e la variación de la serie ausbeerrec
#(NO ENTIENDO ESTOOOOOOOOOOOOOOOOOOOOO)######################3
##########################################################
##########################################################
##########################################################
##########################################################


# (iv) grafico de la serie + recta de regresión. ¿Qué observas?
beta=ausbeerrec.lm$coefficients
plot(t,ausbeerrec_num)
lines(t, beta[1]+beta[2]*t, col="orchid2", lwd=2)

#La recta de regresión si refleja la tendencia de la serie, pero se observa una pauta en los
#puntos alrededor de ella. Los puntos que están por encima de la recta, la gran mayoría se
#mantienen la distancia de separación a la recta ajustada parece mantenerse constante.

## (v) Construye el gráfico de residuos, que observas?
?lm
plot(t, ausbeerrec.lm$residuals, main="Noise process(Residuals)")
abline(h=0)

#Aunque los puntos estan dispersos, al igual que obserbávamos en la anterior gráfica, los
#residuos de la regresión parecen seguir un patrón que la regresión lineal no detecta: la
#estacionalidad.










#### AJUSTE DE LA TENDENCIA CON MEDIA MOVIL (MA)

#Otra manera de detectar la tendencia subyacente es suavizar la serie usando un promedio movil centrado

#Probar de suavizar la serie ausbeerrec con la función MA utilizando el orden 2, 4 y 10.


##### COMO FUNCIONA EL MA(n) ######

#### SI el orden m ES IMPAR ######, k=(m-1)/2
# T_t=(1/m)*sum_j\in(-k,k) (y_t+k) #
# EX: m=3, t=2
#T_2= (1/3)*(y1+y2+y3)

## SI el orden m ES PAR ####    k=m/2
# T_t=(1/(2m))*(y_t-k) + (1/m)*sum_j\in(-k+1,k+1) (y_t+k) + (1/(2m))*(y_t+k)
#EX: m=4, t=5
# T_5= (1/8)*(y_3) + (1/4)*(y_4+y_5+y_6) + (1/8)*(y_7)
#######################################################



#(i) Dad los resultados de los primeros registros de la serie y de la MA en orden 3 y replica el
# cálculo manualmente del primer registro obtenido en la MA(3).
ausbeerrec
trend_beer_3=ma(ausbeerrec, order=3, centre=T) #Coge el anterior, el valor y el posterior y hace la media
trend_beer_3
#Cálculo manual del 2o y 3er registro.
#(320+272+233)/3=275
#(272+233+237)/3=247.33


# (ii) Agregad todas las medias móviles, MA(2), MA(4) y MA(10) en el mismo gráfico con la serie
# original. ¿Qué orden crees que ajusta mejor?
trend_beer_2=ma(ausbeerrec, order=2, centre=T) #Coge el anterior, el valor y el posterior y hace la media
trend_beer_2
trend_beer_4=ma(ausbeerrec, order=4, centre=T) #Coge el anterior, el valor y el posterior y hace la media
trend_beer_10=ma(ausbeerrec, order=10, centre=T) #Coge el anterior, el valor y el posterior y hace la media
plot(as.ts(ausbeerrec), col="navy", lwd=2)
lines(trend_beer_2, col="gold",lwd=3)
lines(trend_beer_4, col="red",lwd=3)
lines(trend_beer_10, col="blue",lwd=3)


## La media móvil de orden 4 (línea roja) es la que mejor suaviza la serie, coincidiendo el orden que mejor ajusta con el orden de la estacionalidad.
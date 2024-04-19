
#### PRÁCTICA 2 ####
## EXTRACCIÓN DE LA TENDENCIA Y LA ESTACIONALIDAD CON R ##


# DURANTE LA PRÁCTICA HAREMOS PASO A PASO Y REPRODUCIREMOS EL PROCEDIMIENTO QUE HAY DENTRO DE LA FUNCIÓN "DESCOMPOSE"
# PERMITE AISLAR LA COMPONENTE "Estacional, la tendencia, y un ruido".



### 1. DESCOMPOSICIÓN ADITIVA

# SI la "variación estacional" parece constante (No crece cuando crecen los valores de la serie)
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
chartSeries(ausbeer)
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
# Realizar el model de regresión de la serie ausbeerrec
#(i) Pasemos la serie a vector: ausbeerrec_num
ausbeerrec_num=unclass(ausbeerrec)
ausbeerrec_num
ausbeerrec
?unclass

# IMPORTANTE PASAR DE DATA.FRAME (ts) A VECTOR CON unclass

# (ii) Crear un vector de la misma longitud con los valores (1,2,….) que será nuestro vector tiempo.
t=(1:length(ausbeerrec_num)); 
length(t)==17*4-4 #sabiamos la dimensión 

# (iii) Construid un modelo de regresión donde ausbeerrec_num es la variable explicada y t la explicativa.
ausbeerrec.lm=lm(ausbeerrec_num~t)
#Modelo de regresión, lm(var_explicada ~ var explicativa)
summary(ausbeerrec.lm) 
 ##summmary -> resumen
#La recta obtenida es Yt=235.7336+2.9769*t+Rt 
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
head(ausbeerrec)
trend_beer_3=ma(ausbeerrec, order=3, centre=T);trend_beer_3 #Coge el anterior, el valor y el posterior y hace la media
#Cálculo manual del 2o y 3er registro.
#(320+272+233)/3=275
#(272+233+237)/3=247.33


# (ii) Agregad todas las medias móviles, MA(2), MA(4) y MA(10) en el mismo gráfico con la serie
# original. ¿Qué orden crees que ajusta mejor?
trend_beer_2=ma(ausbeerrec, order=2, centre=T) #Coge el anterior, el valor y el posterior y hace la media
head(trend_beer_2)
head(ausbeerrec)
trend_beer_4=ma(ausbeerrec, order=4, centre=T) #Coge el anterior y posterior /4 y suma el valor /2
trend_beer_10=ma(ausbeerrec, order=10, centre=T) #(Arriba explicación)
plot(as.ts(ausbeerrec), col="navy", lwd=2)
lines(trend_beer_2, col="gold",lwd=3)
lines(trend_beer_4, col="red",lwd=3)
lines(trend_beer_10, col="blue",lwd=3)


## La media móvil de orden 4 (línea roja) es la que mejor suaviza la serie, coincidiendo el orden que mejor ajusta con el orden de la estacionalidad.



# Con este ejercicio vemos que es importante usar como "orden" el mismo que la "estacionalidad".
#(En caso de que la serie tenga estacionalidad, si no podemos coger cualquiera)


# A partir de ahora usaremos orden 4 pues los datos son trimestrales.



## PASO 3: QUITAR LA TENDENCIA

## Al quitar la "TENDENCIA" de la serie, en este caso (al ser aditiva) restandola, obtenemos
#una nueva serie que muestra la componente estacional con el residuo

# COMO OBTENER LA SERIE SIN LA TENDENCIA
detrend_beer=ausbeerrec-trend_beer_4
plot.ts(detrend_beer)
plot.ts(trend_beer_4)

#Ahora podemos obtener la serie original como
ausbeerrec = detrend_beer+trend_beer_4
plot.ts(ausbeerrec)



## PASO 4. PROMEDIAR LA ESTACIONALIDAD (SEASONAL)
#A partir de la serie "detrend" es facil calcular la estacionalidad promedio
#1. Pondremos la serie en una matriz de manera que cada columna tenga los elementos de cada periodo
#2. Finalmente promediamos cada columna y el valor obtenido es "componente estacional"


#PRÁCTICA 1.4
#Para la serie detrend_beer, dad para cada trimestre la componente estacional como el valor 
#promedio de cada trimestre. 

#->Creamos la matriz
m_beer=t(matrix(data=detrend_beer, nrow=4)); m_beer 
#Observamos que el orden será Qrt3,Qtr4, Qtr1, Qtr2
seasonal_beer=colMeans(m_beer, na.rm=T);seasonal_beer
par(mfrow = c(1, 2))
plot.ts(rep(seasonal_beer, 16))
plot.ts(detrend_beer)
par(mfrow=c(1,1))   ##Comparamos los gráficos, observamos que hay algo de ruido
#¿En cuál de los cuatro trimestres el componente estacional es más alto? 
# -> El 4to trimetre tiene el componente estacional más alto con una media de 57.3833
#¿Y el más bajo? 
#-> El 2ndo trimestre tiene el componente estacional más bajo con una media de -40.01667

#¿Tiene sentido con lo observado en la gráfica de la serie? Razonad 
#vuestra respuesta. 




# PASO 5: EXAMINAR EL RUIDO RESTANTE
#Al quitarle a la serie la tendencia y estacionalidad, nos queda el ruido.
#Sobre el ruido propondremos modelos para capturar las dependencias temporales



## PRÁCTICA 1.5
#Para la serie ausbeerrec, dibuja el ruido aleatorio que aparece al descomponer la serie en su 
#media móvil de orden 4 (trend_beer_4) y las componentes estacionales (seasonal_beer) 
#obtenidas en los ejercicios prácticos anteriores. 

# Comprobad manualmente que la primera componente del primer trimestre de la serie
#random_beer se obtiene mediante la fórmula 

# random_beer=ausbeerrec-trend_beer_4-seasonal_beer

random_beer=ausbeerrec-trend_beer_4-seasonal_beer; random_beer
plot.ts(random_beer)




## PASO 6
# Reconstrucción de la serie original

# En el 1.5 podemos ver que para recuperar la señal original podemos sumar las componentes extraidas
# Xt=mt+st+Yt

#Pero con esto perdemos algunos puntos al principio y final de la serie, ya que los 
# promedios móviles consumen datos antes de producir promedios.

#Veamoslo

recomposed_beer= trend_beer_4+seasonal_beer+random_beer
plot.ts(recomposed_beer, col="orchid", lwd=2)
points(ausbeerrec, col="orchid4") #observaciones originales



## PASO 7
# Descomposición utilizando la serie decompose o stl Reconstrucción de la serie original
# Utilizad la función descompose para descomponer la serie ausbeerrec.
ts_beer = ts(ausbeerrec, frequency = 4)
decompose_beer = decompose(ts_beer, "additive") 


#Lo ploteamos
plot.ts(decompose_beer$seasonal)
plot.ts(decompose_beer$trend) 
plot.ts(decompose_beer$random) 
plot(decompose_beer) 

head(decompose_beer$seasonal) 
head(seasonal_beer)
#Las componentes estacionales obtenidas son ligeramente parecidas a las obtenidas en el anterior ejercicio. 
head(decompose_beer$trend)  #Nose por que se come los dos primeros datos y pone NA
head(trend_beer_4)



## PRÁCTICA 1.7
#Utilizad la función stl para descomponer la serie ausbeerrec. Buscad la ayuda de la función en 
#help para conocer la descomposición. Dad los valores de los componentes estacionales que 
#calcula la función. ¿Son los mismos que hemos calculado en el anterior ejercicio práctico? 
?stl
ts_beer=ts(ausbeerrec, frequency=4)
stl_beer=stl(ts_beer, "periodic",na.action = na.omit)
names(stl_beer)
seasonal_stl_beer=stl_beer$time.series[,1]
trend_stl_beer=stl_beer$time.series[,2]
random_stl_beer=stl_beer$time.series[,3]


plot(ts_beer)
plot.ts(seasonal_stl_beer) 
plot(trend_stl_beer) 
plot(random_stl_beer) 
plot(stl_beer)


head(seasonal_stl_beer)
head(seasonal_beer)
#Son ligeramente parecidas a las del ejercicio anterior.




# 2. Descomposición multiplicativa

# Práctica 2.1
#Calculad las desviaciones típicas de los cuatro trimestres para cada año de las series ausbeer 
#y AirPassengers (trabajada en la práctica 1) y dibuja las dos series de desviaciones. Si la serie 
#anual de las desviaciones típicas se mantiene estable se deduce una descomposición aditiva. 
#Si la serie anual crece o decrece se deduce una descomposición multiplicativa. ¿Qué 
#corresponde a cada una de nuestras dos series? 

library(ffp)
data(ausbeer)
head(ausbeer)
m_beer_dv=matrix(data=ausbeer, nrow=4)  #Cada fila tiene cada trimestre
m_beer_dv_right=(colSums((m_beer_dv*m_beer_dv), na.rm=TRUE)-(colMeans(m_beer_dv,na.rm=T)^2)*4)/3
m_beer_dv_right

plot(sqrt((m_beer_dv_right)))







# PRÁCTICA 2.2
#Realizad los mismos pasos que los realizados con la serie ausbeerc para la serie 
#AirPassengers teniendo en cuenta que la descomposición es multiplicativa.

# Paso 1: Mirar y obtener los datos
data("AirPassengers")
timeserie_air=AirPassengers
plot.ts(timeserie_air)


# Paso 2. Detectar la tendencia (Trend)
trend_air=ma(timeserie_air, order=12, centre=T) #tiene una estacionalidad anual.
plot.ts(timeserie_air)
lines(trend_air,col="darkcyan",lwd=2) 
plot.ts(trend_air)


# PASO 3. QUITAR EL TRENDS
detrend_air=timeserie_air/trend_air
plot.ts(detrend_air)


# PASO 4. PROMEDIAR LA ESTACIONALIDAD (SEASONAL)
m_air=t(matrix(data=detrend_air,nrow=12))
seasonal_air=colMeans(m_air, na.rm=T)
seasonal_air
plot.ts(rep(seasonal_air, 12))


# PASO 5. EXTRAER EL RUIDO
random_air=timeserie_air/(trend_air*seasonal_air)
plot.ts(random_air)



# EXTRA 
#También podemos quitar solamente la estacionalidad, y en ese caso obtenemos la llamada 
#serie ajustada por estacionalidad, observar que en este caso, en 1960 se observa una 
#anomalía que no era detectable a ojo a partir de los datos originales.
sadjust_air = timeserie_air / seasonal_air 
plot.ts(sadjust_air, col = "orchid", lwd = 2)



#### PRÁCTICA SÈRIES TEMPORALS ####
##MANEJO DE LAS SERIES TEMPORALES CON R##

## 1. LA CLASE "ts"

#El paquete base funciona con objetos de clase "ts", que solo maneja datos "equiespaciados".
# És decir: un dato por dia, cada mes, etc.


#Si NO son equiespaciados, usamos objetos de clase "zoo", que se crean en la libreria "zoo".
#Los objetos de clase "zoo" también pueden ser equiespaciadosm por lo que un "zoo" es más general que un "ts"


#DATOS DE CLASE "ts".

#EJEMPLO 1: DATO "ts"
#Exploremos datos de clase "ts" y creemos el gráfico
data("AirPassengers")
ts.plot(AirPassengers, col="darkblue", lwd=2) #lwd:grosor de la línea
AirPassengers   #Los datos son mensuales

## EJERCICIO 1 ###
# ¿Qué periocidad presentan los datos?
# -> Los datos tienen una periocidad mensual, van de enero de 1949 a diciembre de 1960 
# ¿La serie presenta estacionalidad?
# -> Sí. La serie presenta un ciclo mensual que se repite año tras año. Los picos anuales más altos se dan en julio y agosto.
# ¿Dirías que existe una tendencia creciente o decreciente en el tiempo?
# -> Sí, se observa una tendencia creciente, a medida que aumenta el tiempo aumenta el número de pasageros.
# Además el crecimiento anual NO es constante todo el año, los meses de verano teien un aumento mayor en el 
# número de viajeros que los de invierno, donde el crecimiento es más moderado.








#EJEMPLO 2: CONVERTIR VECTOR A "ts"
#Vector con las ventas de vino Australiano entre enero 1980 y octubre 1991

# Ayuda con la función "ts": 
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/ts

install.packages("zoo") #Tiene una instruccion para usar solo meses
require(zoo)

#Leemos el archivo y definimos la serie, indicando fecha de inicio y final
wine=read.table("https://homepage.univie.ac.at/robert.kunst/WINE.TSM")
head(wine)
wine  #recordemos que head coge los 6 primeros datos
wine=wine$V1
wine   #V1 es el colname de la única columna.

#Definimos la fecha de inicio (R tiene otros formatos de fecha de inicio: https://rpubs.com/jo_irisson/howto_date_time)
start=as.Date("1980-01-01")
?as.Date() #para indicar que es una fecha
start
start=as.yearmon(start)
start
?as.yearmon()   ##yearmon{zoo} es una clase para representar datos mensuales

#Defiminos la fecha de finalización

end=as.yearmon(as.Date("1991-10-01"))
end



winets=ts(wine,start=start, end=end, frequency=12)
winets #ver que aspecto tiene el objeto
winets
ts.plot(winets)
plot(decompose(winets))
## Este gráfico corresponde a la descomposición de la serie en suma de una tendencia, una parte espacional y un ruido puro.
?decompose()     # En modelos aditivos o multiplicativos.
#Primero determina la componente de tendencia y la elimina de la serie temporal
#En segundo lugar hace lo mismo con el primedio y por último determina el error
## La función decompose() 

##################################################
##################################################
##################################################
### PREGUNTAR DESCOMPOSE ---> PRÁCTICA 2
##################################################
##################################################
##################################################


### PRÁCTICA 1.2 ####
install.packages("readxl")
library(readxl)
?readxl
### PREGUNTAR 
# Como no me hace la lectura

Dataset<- data.frame(
  año=c(2015,2015,2015,2015,2016,2016,2016,2016,2017,2017,2017,2017,2018,2018,2018,2018,2019,2019,2019,2019),
  Trimestres=rep(c("T1","T2","T3","T4"),5),
  Viajes=c(707595,1106119,1784994 , 1149798,889362,890404,1587825,797481,919403,1209072,1750883,933993,939097,1250890,1755465,913692,963458,1341253,1759956,779868),
  GastoTotal=c(92308,163232,391754,163188,122458,102926,370557,148394,108802,200821,442712,166040,106572,193800,400189,156846,107653,197390,484501,133185),
  DuracionMedia=c(3.16,3.04,5.58,3.00,3.07,2.48,5.59,3.16,2.49,2.97,5.93,3.05,2.32,2.55,4.72,3.49,2.45,2.57,5.80,3.08),
  GastoXViajero=c(130.45,147.57,219.47,141.93,137.69,115.59,233.37,186.08,118.34,166.10,252.85,177.77,113.48,154.93,227.97,171.66,111.74,147.17,275.29,170.78)
  )

Dataset



## (ii) Transforma la serie de número de viajes de la CCAA a formato ts y pega a ,
#continuación la imagen de cómo viene representada la base de datos y el gráfico resultante.

viajes=ts(Dataset$Viajes, start=c(2015,1), end=c(2019,4), frequency = 4) #Frequency=4 pues son 4 trismestres
plot(viajes)

#ts.plot(Dataset$Viajes)    -> No guarda fechas 


## (iii)
#-> ¿Qué periocidad presentan los datos?
# Los datos presentan una periocidad trimestral, va del 1er trimestre de 2015 al último de 2019.

#->La serie presenta estacionalidad?
# Sí, los datos parecen tener un ciclo anual.
# Se produce un ascenso de datos progresivo del último semestre del año anterio alcanzando los picos anuales más altos en 3er trimestre del año.
# coincidiendo con los meses fuertes veraniegos y posteriormente desciende abruptamente druante el 4to trimestre alcnzando el pico más bajo del año junto al primer trimestre.
# Se repite este ciclo los 5 años, a excepción de alguno donde el 1er, 2ndo o 4rto trimestre tienen tendencia más plana. Pero se repite en todos que el 3ero tiene un pico destacado al resto.




#-> Dirías que existe una tendencia creciente o decreciente?
# No, la media parece bastante plana sin destacar ninguna tendencia creciente o decreciente durante los años 2015-2019







# 2. LA CLASE ZOO
#En el siguiente fichero aparecen datos de la población de USA entre 1790 y 1990 con intervalos de 10 años (esquiespaciados)
#Aun que sean equiespaciados (10 años) utilizaremos zoo para ver como sea hace

#usa=read.table("http://www.stat.colostate.edu/~estep/assets/uspopulation.txt")
#(El enclace no funciona, lo copiamos a mano)
usa=data.frame(
  V1=c(3929214, 5308483, 7239881, 9638453, 12860702, 17063353, 
       23191876, 31443321, 38558371,50189209, 62979766, 76212168,
       72228496, 106021573, 123202624, 132164569, 151325798,
       179323175,203302031,226542203, 248709873)
)
usazoo=zoo(usa, order.by=seq(1790, 1990, 10))
class(usazoo) #"zoo"
usazoo


# Podemos hacer el gráfico
plot(seq(1790, 1990, 10), usazoo) # gráfico de puntos
plot(seq(1790, 1990, 10), usazoo, type = "b") #Une por rallas cuando hay cambio de tendencia
plot(seq(1790, 1990, 10), usazoo, type = "b", main="US population") #Pone el título
plot(seq(1790, 1990, 10), usazoo, type = "b", main="US population",xlab="year", ylab="population")
#Cambia los títulos de los ejes



# 3. OTRAS FUNCIONES PARA MANIPULAR SERIES
#Podemos combinar objetos de clase "ts" y de clase "zoo". 

# -> "ts.union" junta dos series rellenando con NA los periodos en que una es más larga que otra
# -> "intersect.ts" junta ambas series pero sólo con los datos corresponientes al periodo común de ambas

serie1=ts(1:20, freq=12,start=c(1981,3))
serie1
serie2=ts(1:15, freq=12, start=c(1980,9))
serie2
serie3=ts.union(serie1,serie2)
serie3
ts.plot(serie3) #queda muy mal al haber pocos datos y ser lineales
serie4=ts.intersect(serie1,serie2)
serie4



## LA FUNCIÓN "lag.plot": Representa a serie observada frente a una versión suya desfasada un número lags de unidades de tiempo. Permite visualizar la "autodependencia" de la serie.
#lag.plot(x,lags=k)=plot(lag(x,k),x)

lag.plot(winets)
ts.plot(winets)
plot(winets,lag(winets), xlab="winets(t)", ylab="winets(t+1)")
lag.plot(winets,lags=4, layout=c(2,2))

#Otra función útil es "diff" que la veremos en otras prácticas.


# 4. OTRAS FUNCIONES PARA VISUALIZAR GRÁFICOS
# La libreria ggplot2 irá bien para gráficos.
require(ggplot2)
require(forecast)
ggseasonplot(winets) #gráfico anual
ggseasonplot(window(winets,start=1990)) #Elegimos solo algún año
ggseasonplot(window(winets,start=1985, end=1990))
#?window és una función de "st".


## PRÀCTICA 4.1
#Con los datos trabajados en la práctica 1.2 del archivo 
#EncuestaResidentesTrimestral.xls, pega a continuación el código utilizado y la gráfica 
#de la serie de número de Viajes comparando año a año su comportamiento trimestral 
#con la función ggseasonplot. Se pide:
viajes=ts(Dataset$Viajes, start=c(2015,1), end=c(2019,4), frequency = 4) #Frequency=4 pues son 4 trismestres
plot(viajes)
ggseasonplot(viajes)


# PREGUNTAS
#(i) Explica brevemente que comportamiento anual observas en el tercer trimestre. 
#¿Año tras año aumenta el número de viajes, disminuye, se mantiene estable, se alterna? 
ggseasonplot(viajes)
# Observamos como en número de viajes se mantiene estable durante los últimos 3 años (2017-2019)
#El primer año (2015) era superior a todos los años pero el 2016 hubo un descenso de 200.000 viajes aprox un 11%

# (ii) És el mismo comportamiento anual el segundo trimestre que el tercero?
# -> No exactamente. En el segundo trimestre también se da un descenso abrupto de 2016 respecto 2015 en este caso mayor (un 20%)
# -> En cambio a partir de 2017 hay un crecimiento progresivo en los datos del 2o semestre que no es tan apreciado en el 3o
# -> Un 36% en 2017 respecto 2016, 3% en 2018, 7% en 2019.







### SERIES FINANCIERAS ###
#Es útil la libreria "quantmod"
#->Tiene funciones que bajan datos de Yahoo!, Finance, La Reserva Federal de EEUU, 

install.packages("quantmod")
library("quantmod")
getSymbols("AAPL") #Aplle stok yahoo Finance
dim(AAPL)
head(AAPL)
tail(AAPL)
chartSeries(AAPL) # Apple daily closing prices y trading volume
addBBands(n=20,sd=2) #Bollinger bands (media móvil de tamaño n +/- 2 desviaciones
chartSeries(AAPL[,6]) # Columna 6 of the object "AAPL" de R.
chartSeries(AAPL[,6],theme="white")



## PRÁCTICA 4.2
install.packages("TSA")
library(TSA)
library(tseries)
dades=read.csv2("C:/Users/ERIC/Desktop/SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 1 (120923)/ExerciciBorsaASDF.txt", header=TRUE, sep="")
head(dades)

## NO ME FUNCIONA LA LECTURA DE DATOS

dades=data.frame(
  x=c(200,199.9328859,198.92198,200.800216,201.075056,202.4002594,201.161885,197.4692159,198.0000577,201.9628447,203.0919489,200.9096352,198.9028436,198.9940529,198.7569741,198.8995379,199.1845066,200.7141423,199.2476819,198.3632923,197.9029737,200.6520341,199.5161789,200.3951612,200.5964167,199.4019118,197.5685683,199.4370568,199.8123694,196.9250992,199.1061817,198.5278516,199.42816,196.9433097,194.8852729,198.101602,197.6257048,197.3124723,198.8316939,197.1519613,196.0626374,198.8386287,198.1363402,199.1354418,194.8678034,194.6293767,195.1205392,196.9617888,196.7747041,197.9851398,194.4066369,194.7115157,195.3380549,196.4370314,197.424514,199.7395505,196.2509076,194.6810511,196.4346359,196.0663035,198.0099486,194.9118622,196.7697532,194.1138913,198.1480805,196.8041855,195.2277137,194.4564257,196.3787789,196.5985769,195.3072555,195.2105133,194.3254386,193.3713843,194.9218798,196.381346,194.9359129,195.0113844,193.7381381,193.1297039,194.2417025,196.7232349,195.0316473,193.422561,191.6357935,192.0220855,192.1147789,192.8901888,192.6453338,194.6255875,194.2894852,192.1484144,192.0752123,193.7738776,195.0539513,195.2364312,193.793488,196.6954736,193.331223,194.1291837,192.8601137,193.8832233,193.1625768,192.4288297,192.6120046,192.9956421,190.4219908,194.4506492,194.8642993,191.5602327,189.2637064,192.2182838,194.1171722,194.5105809,193.2638002,191.9396705,192.404669,190.9324905,190.7452833,194.3783277,191.8993894,191.3665952,190.0476305,194.0741305,192.2072634,192.9449153,193.2165045,190.2426915,191.1323912,190.4173848,187.4941282,191.8851693,193.1047887,190.3974996,187.560048,189.0378122,192.3800126,192.6780475,191.1422624,187.9263653,191.3864211,192.6812067,192.2147798,189.2827395,190.4677112,189.2398265,189.2010095,188.2190123,190.329575,189.749439)
)
ts.plot(dades, main="Evolucio temporal de la borsa de l'empresa ASDF")
library(zoo)
d<-seq(as.Date("2019-01-01"), as.Date("2019-05-30"), by="1 day")
dades.ts=as.ts(dades$x)
names(dades.ts)<-d
dades.ts
####################################################################
#EJEMPLO DE names
x=1:10
names(x)=c("A", "B", "C", "D", "A", "B", "C", "D", "F", "H"); x
####################################################################

# Veamos la evolución del plot
plot(d, dades.ts, type = "l",xaxt="n",ylab="dades", xlab="temps",main="Evolucio temporal de la borsa de l'empresa ASDF")
axis.Date(1, at=seq(min(d), max(d+4), by="week"), format="%Y-%m-%d")
## by="months", "days", "weeks", "years"


# Altre forma de plot
plot(dades$x, type = "l",xaxt="n",ylab="Valor Accions", xlab="Mesos 2019",main="Evolucio temporal de la borsa de l'empresa ASDF")
axis(1,c(1,32,60,91,121,152),month.name[1:6])


# Altre forma de plot
#serie1<-read.table("C:/Users/ERIC/Desktop/SERIES TEMPORALES/PRÁCTICAS/PRÁCTICA 1 (120923)/ExerciciBorsaASDF.txt", dec=",", header=T)
serie1=dades
dates<-seq(as.Date("2019-01-01"), length=150, by="days")
serie<-xts(x=serie1$x, order.by=dates)
chartSeries(serie)


# Última forma de plot
diff_dades<-diff(dades.ts)
plot(d[-1], diff_dades, type="l", xaxt="n", ylab="diff_dades", xlab="temps", main="Dades diferenciades")
axis.Date(1, at=seq(min(d), max(d+4), by="months"), format="%Y-%m-%d")

## (ii) ¿Cuál es la tendencia de la serie, creciente, decreciente, oscila?
# -> La tendencia global de la serie es decreciente.
#-> El valor de las acciones sufren caída global, con importantes oscilaciones de subidas y bajadas intermitentes.



# (iii) ¿Cuál ha sido la variación que ha sufrido la acción del 1 de enero de 2019 a la 
#fecha más actual del archivo? 

dades$x
#El valor de la acción ha decaido de 200$ a 189.7494$.
(189.7494-200)/200 #sufre una caida del 5% en total


#¿Y la variación intramensual (variación entre el 
#valor de la acción a cierre del mes anterior con la fecha del cierre del mes  actual)? 
dades.ts
#En el cierre de abril el valor de la acción era de 194.3783$ y actualmente de 189.7494$
(194.3783-189.7494)/194.3783
#Esto supone una caida del 2.38% en el valor de la acción
#Comenta brevemente los resultados obtenidos


## (iv)¿Entre qué días hay el “guany2” más elevado en el valor de las acciones? 
#¿Y la “pèrdua2” más elevada?

max=max(diff_dades)
min=min(diff_dades)

#Busquem a quins dies pertanyen aquestes xifres
for (i in 1:length(diff_dades)){
  if(diff_dades[i]==max) print(i)
  if(diff_dades[i]==min) print(i)
} ## 44 i 131
#Verifiquem les dades
dades.ts[45]-dades.ts[44]==min #TRUE
dades.ts[132]-dades.ts[131]==max #TRUE

#Per tant la pèrdua més elevada es troba entre els dies 44 i 45 i els guanys més elevats són entre els 131 i 132
#Mirem la data
sprintf("(%s,%s", names(dades.ts[44]), names(dades.ts[45]))
#Entre el 13 i 14 de febrer les pèrdues més elevades

sprintf("(%s,%s", names(dades.ts[131]), names(dades.ts[132]))
#Entre el 11 i 12 de maig els guanys més elevats


## ¿Cuál es el número màximo de días consecutivos con subidas de valor?
#Hacemos una función
#Pujades de valor
j=0
max=0
for(i in 1:length(diff_dades)){
  if(diff_dades[i]>0){
    j=j+1
    if(j>max) max=j
  }
  else j=0   #para reiniciar la j si un dia baja
}
paste("El nombre maxim de dies consecutius de pujades de valor es ", max)

#Baixades de valor
j=0
max=0
for(i in 1:length(diff_dades)){
  if(diff_dades[i]<0){
    j=j+1
    if(j>max) max=j
  }
  else j=0   #para reiniciar la j si un dia baja
}
paste("El nombre maxim de dies consecutius de baixades de valor es ", max)

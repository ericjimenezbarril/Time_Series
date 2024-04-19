#### PRÁCTICA 3 A ####

# Suavización exponencial de Holt Winters y otros usos de las medias móviles




# 1. EL SUAVIZADO EXPONENCIAL (HOLT-WINTERS)

# Ajuste del modelo
#-> El model de Winters emplea un componente de:
 #  -> media, tendencia y estacional
# en cada periodo para generar un model robusto que devuelve pronósticos a corto plazo fuera de la muestra

# Utiliza 3 parámetros de suavización para actualizar los componentes de cada período

# --> Los valores para los componentes de "nivel" y "tendencia" se obtienen de una regresión lineal
# sobre su tiempo.

# Para cada componente estacional se obtiene una regresion de variables simulada utilizando datos sin tendencia

# PRÁCTICA 1.1
library(fpp)
data(ausbeer)
plot.ts(ausbeer)

#(i) Ejecutad el siguiente código R para obtener el pronóstico de modelo de suavizado 
#exponencial de Holt-Winters para los 5 periodos siguientes fuera de la muestra. 

hw.beer=HoltWinters(ausbeer, alpha = NULL, beta = NULL, gamma = NULL, seasonal = "additive") 
hw.beer
p.beer=predict(hw.beer, n.ahead = 5, prediction.interval = TRUE,level = 0.95) 
p.beer
plot(hw.beer,p.beer)

# (ii) Dad los valores de pronóstico obtenidos para los 5 periodos y su intervalo de confianza al  95%. 
p.beer

# (iii) Dad la ecuación de Holt-Winters con los valores numéricos que te calcula R. 
hw.beer
?HoltWinters
#Lt = 0,2141392 *(Yt – S[t-p]) + (1-0,2141392*(L[t-1]+T[t-1])
#Tt = 0.130769 ( Lt- L[t-1] + (1-0.130769) *T[t-1]
#St = 0,3237649 * (Yt-Lt)+(1-0,3237649)* S[t-p]
#Yt estimat = L[t-1] + T[t-1] + S[t-p]

# Te devuelve a,b,s1,s2,s3,s4
# p=4 es la componente estacionaria (de estacionalidad)
# a=a[0], b=b[0], s1=s[1]
# a[t]==Lt, b[t]==Tt, s[t]=St

### 2. OTROS USOS DE LAS MEDIAS MÓVILES
# (para finanzas) El , el análisis técnico consiste en producir diversas descripciones de las series de precios de los activos financieros
# Pt, t\in {0,1,.....}

#Una estrategia para comprar o vender es:
# 1. Se ajusta a la serie de precios de cierre un promedio móvil de corto plazo MA(n)_t (ventana de tamaño n )
# 2. Se ajusta la serie a largo plazo MA(m)_t con una ventana de tamaño m>n
# 3. Se calcula el proceso Xt=MA(n)_t - MA(m)_t
# 4. Si en un instante t_i cambia de signo, entonces
#   4.1. Si X_{ti-1}<0 y X_{ti}>0, comprar en t_i +1 al precio de apertura del dia siguiente
#   4.2. SI X_{ti-1}>0 y X_{ti}<0, vender

# SE SUELE USAR n=20, m=50




## PRÁCTICA 1.2. 
#Realizad el gráfico de la serie de los precios de Apple desde enero de 2020 a hoy 6 de octubre de 2020 y 
#sus medias móviles de orden 20 y 50. 

library(quantmod)
getSymbols("AAPL")
adj=AAPL$AAPL.Adjusted["2020-01-01::2020-10-06"]
plot.ts(adj,type="l",lwd=2)
lines(ma(adj, order = 50, centre = T),col="orchid")
lines(ma(adj, order = 20, centre = T),col="gold")


#(i)¿El último cruce de las dos medias móviles que observas en el gráfico indicaba comprar o vender 
#acciones?
#Recordemos que Xt=MA(20)t-MA(50)t
#Compramos si X{ti-1}<0 y X{ti}>0
#Vendemos caso contrario

#En el útlimo cruze, X{ti-1}<0 y X{ti}>0 por tanto indica que recomendamos comprar al dia siguiente


#(ii) ¿Creéis que en los próximos días se intuye algún cambio de posición de inversión en la
#compra-venta de acciones? ¿Cuál? Razonad vuestra respuesta

# -> Realizando una estimación de las medidas móviles, recomendaria como opinión personal
# -> vender en la bolita roja y actualmente entramos en una zona propicia para volver a comprar
# (ver solución)

#(iii) iii. Dad las bandas de Bollinger y describid lo que observáis con lo aprendido en el siguiente enlace:
#https://www.youtube.com/watch?v=yyRRgL8zjsw&feature=emb_logo

### Un grafico mas profesional:
chartSeries(AAPL, subset="2017-01-11::2018-09-20", 
            theme=chartTheme("white",up.col="green",dn.col="red"), 
            TA=c(addBBands(n=20,sd=2),addSMA(n=50,col="blue")))
?chartSeries

### OTRO GRÁFICO 
chartSeries(AAPL, subset="2017-01-11::2018-09-20", 
            theme=chartTheme("white",up.col="green",dn.col="red"), 
            TA=c(addBBands(n=20,sd=2),addSMA(n=30,col="darkorchid2"),addSMA(n=50,col="blue"),addVo(log.scale=FALSE)))



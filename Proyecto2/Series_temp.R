#Pasamos a analizar la serie temporal del �ndice nacional total de comercios al por menor. Disponemos datos de cada mes desde 
#enero del 2000 hasta diciembre de 2021.

#Emepezamos borrando las variables, dataframes y funciones del global environment y cargando nuestros datos:

rm(list=ls())
datos <- read_excel("C:/Users/aleja/OneDrive/Escritorio/Tarea/Indice_Temp.xls")
datos <- as.data.frame(datos)
colnames(datos)

summary(datos)

ggplot(as.data.frame(summary(datos))) 

str(datos)   #Todas las variables son numericas

#(An�lisis un poco de los datos bla bla)

#Creamos ahora un objeto timeseries con la funcion ts para analizar nuestra serie temporal:

indice <- ts(datos[,-1], start=c(2000,1), frequency = 12)

#Graficamos:

autoplot(indice)

autoplot(indice) + ggtitle("�ndice nacional de comercios al por menor")
+ xlab("mes") + ylab("�ndice")

#Para que aparezca el mes en el eje podemos hacer uso de la librer�a zoo.
#Creamos la secuencia de fechas observadas ennuestros valores:

dt = seq(from=as.Date("2000-01-01"), by="month", length.out=264)
head(dt)
str(dt)
dt[264]

#Asignamos ahora los valores dt a la serie

indiceZ = zoo(datos[,-1], order.by=dt)
autoplot(indiceZ) + ggtitle("�ndice nacional de comercios al por menor") + xlab("mes") + ylab("�ndice") #----> No me sale el formato
#En plots posteriores, cuando reduzcamos la escala dl eje x,  apareceran los meses y no solo los a�os
 
#Escogemos 2 a�os a partir de 2005 y los observamos con la opcion windows:

indiceZ_2005 <- window(indiceZ, start=as.Date("2005-01-01"), end=as.Date("2007-01-01"))
autoplot(indiceZ_2005) + ggtitle("�ndice a�os 2005-2006") + xlab("mes") + ylab("�ndice")

#Pasamos a calcular la descomposici�n estacional de la serie:
#Lo hacemos seg�n el modelo multiplicativo:

indice_Comp <- decompose(indiceZ, type=c("multiplicative")) #Esta no funciona
indice_Comp <- decompose(indice, type=c("multiplicative"))
autoplot(indice_Comp, ts.colour = "blue")

#En el primer gr�fico vemos que tenemos un patr�n que se repite cada a�o, con dos m�ximos y dos m�nimos
#Tenemos un segundo gr�fico con la tendencia, que var�a dependiendo de los a�os.
#Un tercer gr�fico con el comportamiento estacional, que se repite. Se calcula para cada mes como la media de
#todos los meses observados entre la media de todos los valores observados.
#Por �ltimo tenemos el gr�fico de las irregularidades, que grafica las irregularidades que no se explican con el 
#modelo de descomposicion.

#Podemos acceder a las componentes que se obtienen de la descomposicion estacional en forma de tabla:

print(indice_Comp)

#Los coeficientes de estacionalidad son:

knitr::kable(indice_Comp$figure, digits=2, caption = "Coef Estacionalidad", "simple")

#Vemos por ejemplo que el �ndice de comercios al por menor aumenta un 20% en dciembre con respecto a la media del a�o
#Representamos la serie con la tendencia y la serie ajustada estaciionalmente

autoplot(indice, series="Datos") +
  autolayer(trendcycle(indice_Comp), series="Tendencia") +
  autolayer(seasadj(indice_Comp), series="Estacionalmente ajustada") +
  xlab("Year") + ylab("�ndice CM") +
  ggtitle("�ndice de comercios menores") +
  scale_colour_manual(values=c("gray", "red", "blue"),
                     breaks=c("Datos", "Tendencia", "Estacionalmente ajustada"))

#Vemos que los datos estacionalmente ajustados no se ajustan del todo a los originales, esto es porque estamos empleando
#siempre el mismo coeficiente de estacionalidad (totos los meses de agosto tienen el mismo). Este coeficiente
#ha podido ir cambiando con el tiempo, y esto es lo que conseguiremos con los m�todos de suavizado que veremos despu�s

#Para estudiar la estacionalidad resulta �til representar cada a�o por separado:
#Es interesante grafica con coordenadas polares en funci�n de los meses:

ggseasonplot(indice, polar = TRUE, 
             season.labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                               "Aug","Sep","Oct","Nov","Dec")) +
  ggtitle("�ndice de comercios al por menor en Espa�a") + 
  xlab("A�o") + 
  scale_color_discrete(name= "Year", labels= paste0("Y:",2000:2021)) +
  theme_bw()

#Se ve por ejemplo la anomal�a del �ndice en abril del a�o 2020, o el patr�n de como los meses de diciembre son los que tienen
#el �ndice m�s elevado o las depresiones de noviembre y febrero.

#En coordenadas cartesianas:

ggseasonplot(indice, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("N�mero") +
  ggtitle("Seasonal plot: �ndice de comercios al por menor en Espa�a")

#Quiz�s pueda ser interesante filtrar d�cada a d�cada para que se distingan mejor los datos
#En las funciones te piden como input objetos de tipo ts y al convertir los zoo con as.ts los datos se modifican
#Tendriamos que crear nuevos indices del 2000 al 2010, y del 2010 a la actualidad:

indice_00_10 <- ts(datos[0:120,-1], start=c(2000,1), frequency = 12)
indice_10_21 <- ts(datos[121:264,-1], start=c(2010,1), frequency = 12)

ggseasonplot(indice_00_10, polar = TRUE, 
             season.labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                               "Aug","Sep","Oct","Nov","Dec")) +
  ggtitle("�ndice de comercios al por menor en Espa�a") + 
  xlab("A�o") + 
  scale_color_discrete(name= "Year", labels= paste0("Y:",2000:2009)) +
  theme_bw()

ggseasonplot(indice_10_22, polar = TRUE, 
             season.labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul",
                               "Aug","Sep","Oct","Nov","Dec")) +
  ggtitle("�ndice de comercios al por menor en Espa�a") + 
  xlab("A�o") + 
  scale_color_discrete(name= "Year", labels= paste0("Y:",2010:2021)) +
  theme_bw()

ggseasonplot(indice_00_10, year.labels=TRUE, year.labels.left=TRUE) +
  ylab("N�mero") +
  ggtitle("Seasonal plot: �ndice de comercios al por menor en Espa�a")

ggseasonplot(indice_10_21, year.labels=TRUE, year.labels.left=TRUE) +    #Con este no hemos conseguido mucho
  ylab("N�mero") +
  ggtitle("Seasonal plot: �ndice de comercios al por menor en Espa�a")

#Vamos ahora a reservar separar el u�timo a�o del dataframe para utilizarlo para hacer predicciones:

indice_test <- window(indice, start=c(2021,1))
indice_test2 <- window(indice, start=c(2020,1))
autoplot(indice_test) #Aq�i tenemos el ultimo a�o

#Empezamos haciendo un suavizado exponencial simple con la funcion ses:

indice_s1 <- ses(indice, alpha=NULL, h=24 ) #alpha calcula directamente calcula el alpha para el menor error cuadr�tico
#h=7 es el numero de predicciones que calculamos a partir del ultimo observado


knitr::kable(indice_s1$model$par, digits=4, caption="Parametros del modelo", "simple")

#Nos aparece el alpha=0.4764 y el valor inicial en el isntante1, que es la media de la serie
#Graficamos:

autoplot(indice_s1) + 
  autolayer(fitted(indice_s1), series = "Fitted") +
  ylab("�ndice") + xlab("mes")

#Vemos que no se adapta nada bien a nuestra serie, puesto que nuestra serie presenta cambios en la tendencia.
#Aplicamos el m�todo de Holt para incluir la tendencia:

indice_s2 <- holt(indice, h=24 )

autoplot(indice_s2) + 
  autolayer(fitted(indice_s2), series = "Fitted") +
  ylab("�ndice") + xlab("mes")

#No entiendo muy bien lo que estoy haciendo. Estoy prediciendo sobre la serie del ultimo a�o, y despues no tengo valores.
#No tendria que quitar el ultimo a�o, hacer predicciones sobre el resto y ya despu�s comparar con los valores reales?

#Para calcular los parametros alpha y beta podemos clicar en indice_2 y en model
#El m�todo de tendencia amortiguada es una variaci�n del m�todo Holt y hace que las predicciones no se ajusten a una recta
#sino a una curva

indice_s3 <- holt(indice, damped=TRUE, phi=NULL, h=24 )

autoplot(indice_s3) + 
  autolayer(fitted(indice_s3), series = "Fitted") +
  ylab("�ndice") + xlab("mes")

#Vemos los parametros:

knitr::kable(indice_s3$model$par, digits=4, caption="Parametros del modelo Holt amortiguado")

autoplot(indice) +
  autolayer(indice_s3, series="Dumped Holt's Method", PI=FALSE) +
  autolayer(indice_s2, series="Holt's Method", PI=FALSE) + 
  ggtitle("Forecasts from Holt's method") + ylab("�ndice") + xlab("mes") + guides(colour=guide_legend(title="Ajustes"))

#Sigo pensando que deberia utilizar todos los datos para hacer el fit y guardarme el ultimo a�o para comparar
#Y no hacer predicciones a partir solo del ultimo a�o.

#Aplicamos ahora otro m�todo de suavizado, que tiene en cuenta la estacionalidad de una serie. Si la incidencia de la
#estacionalidad aumenta con el tiempo emplearemos el modelo multiplicativo. En el caso de que no lo haga emplearemos el
#modelo aditivo. Este ser� el modelo de suavizado que m�s se ajuste a nuestros datos probablemente

indice_test2 <- window(indice, start=c(2020,1)) #Cogemos m�s valores porque el algoritmo los necesita
#Esto ya hemos dicho que habr� que cambiarlo para las partes anteriores probablemente

indice_s3 <- hw(indice, h=24, seasonal="additive", level=c(80,95)) #El level son los intervalos de confianza
print(indice_s3) #Aqu� vemos las predicciones y los intervalos de confianza

#Para ver los par�metros:

knitr::kable(indice_s3$model$par, format="pipe", digits=4, caption="Estimadores de los par�metros")

#Graficamos:

autoplot(indice_s3)

#El subset que probablemente queramos usar: train <- subset(indice, end=length(indice) - 12)
#En el hw incluir el parametro damped=TRUE ----> Amortig�a la estacionalidad

#Probamos ahora a ver si es mejor el additive o el multiplicative con el set que deber�amos utilizar en principio

train <- subset(indice, end=length(indice) - 12)
fit1 <- hw(train, h=12, seasonal="multiplicative")
fit2 <- hw(train, h=12, seasonal="additive")

autoplot(train) +
  autolayer(fit1, series="HW multiplicative forecast", PI=FALSE) +
  autolayer(fit2, series="HW additive forecast", PI=FALSE) +
  xlab("Year") +ylab("�ndice") + ggtitle("�ndice peque�os comercios ") +
  guides(colour=guide_legend(title="Forecast"))

#Esto se parece m�s a lo que queremos
#Par�metros:

knitr::kable(fit1$model$par, format="pipe", digits=4, caption="Estimadores de los par�metros")
knitr::kable(fit2$model$par, format="pipe", digits=4, caption="Estimadores de los par�metros")

#Predicciones:

print(fit1)
print(fit2)

#Probamos ahora a mortig�ar el componente estacional:

fit3 <- hw(train, h=12, seasonal="multiplicative", damped=TRUE)
fit4 <- hw(train, h=12, seasonal="additive", damped=TRUE)

#Nos quedamos con las predicciones para compararlas con los valores reales:

data1 <- as.data.frame(fit1)
prediction1 <- data1[,1]
print(prediction1)

data2 <- as.data.frame(fit2)
prediction2 <- data2[,1]
print(prediction2)

data3 <- as.data.frame(fit3)
prediction3 <- data3[,1]
print(prediction3)

data4 <- as.data.frame(fit4)
prediction4 <- data4[,1]
print(prediction4)

#Extraemos ahora los valores conocidos en forma de variable num�rica:

values <- window(indice, start=c(2021,1))
values <- as.numeric(values)                                       #Todo esto se puede resumir mucho
print(values)

#Para saber si quedarnos con un modelo u otro podemos estudiar distintos par�metros
#Definimos la funcion:


getPerformance = function(pred, val) {
  res = pred - val
  MAE = sum(abs(res))/length(val)
  RSS = sum(res^2)
  MSE = RSS/length(val)
  RMSE = sqrt(MSE)
  perf = data.frame(MAE, RSS, MSE, RMSE)
}

#HAY QUE REALIZAR LAS ESTIMACIONES SIN COGER EL PERIDO DE LA PANDEMIA, AS� PODEMOS VER EL EFECTO QUE ESTA HA TENIDO EN LAS PREDICCIONES
#CON IR DIRECTAMENTE AL ANALISIS DE HOLT WINTERS ES SUFICIENTE, EPLICANDO TE�RICAMENTE POR QU� TOMAMOS LA DECISI�N

params1 <- getPerformance(data1, values)
params2 <- getPerformance(data2, values)
params3 <- getPerformance(data3, values)
params4 <- getPerformance(data4, values)
print(params1)
print(params2)
print(params3)
print(params4)

#En este caso nos quedar�amos con el primer modelo al tener un RMSE m�s bajo

autoplot(fit1)

#Podemos coger este modelo y ver c�mo funciona si lo entrenamos s�lo con los datos anteriores al COVID, para ver c�mo ha
#influ�do la pandemia en la informaci�n

#La autocorrelaci�n y la autocorrelaci�n parcial son medidas de asociaci�n entre valores de series actuales y pasadas e indican cu�les son los valores de series pasadas m�s �tiles para predecir valores futuros. 
#Con estos datos podr� determinar el orden de los procesos en un modelo ARIMA. M�s concretamente,

#Funci�n de autocorrelaci�n (FAS). En el retardo k, es la autocorrelaci�n entre los valores de las series que se encuentran a k intervalos de distancia.
#Funci�n de autocorrelaci�n parcial (FAP). En el retardo k, es la autocorrelaci�n entre los valores de las series que se encuentran a k intervalos de distancia, teniendo en cuenta los valores de los intervalos intermedios.

#un correlograma es una imagen de la correlaci�n de estad�sticas. Por ejemplo, en el an�lisis de series temporales, el correlograma, tambi�n conocido como un gr�fico de autocorrelaci�n

#Para calcular las autocorrelaciones simples par aun retardo de 48 meses (4 a�os):

train2 <- subset(indice, end=length(indice) - 24) #Eliminamos la anomalia de la pandemia
autoplot(train2)

ggAcf(train2, 48)

#Si el correlograma decrece lentamente la serie no es estacionaria/Si decrece r�pido s� lo es (la nuestra decrece lentamente) ---> No es estacionaria
#Si el correlograma se corta o decrece r�pidamente podemos considerarla estacionaria  
#Esta funcion tambi�n nos va a mostrar el modelo a usar y el orden

#Vemos que existe una correlaci�n que destaca principalmente en los meses de diciembre y otra que lo hace de forma
#secundaria en el mes de junio. Adem�s podemos argumentar que la serie no presenta demasiada estacionariedad, puesto que
#el correlograma decrece de forma suave. Esto lo ve�amos ya al graficar nuestros datos, donde se observa como la media y la 
#varianza de la serie var�an con el tiempo.

#En las correlaciones parciales eliminamos el efecto de retardo de las acumulaciones anteriores, por lo que se puede
#ver de forma m�s clara las correlaciones realmente sinificativas.
#Para calcular las autocorrelaciones parciales para aun retardo de 48 meses (4 a�os):

ggPacf(train2, 48)

#Con respecto a las autocorrelaciones parciales, a partir de los 13 meses las podemos despreciar, pero antes
#encontramos varias que son significativas. Entre ellas destacamos dt=1, dt=5, dt=10, dt=11, dt=12. Esto quiere decir que
#hay una dependencia mayor entre un mes t y los meses t+1, t+5, t+10, t+11 y t+12.

#Volvamos al plot de nuestra serie, que sabemos que no es estacionaria ni en media, ni en varianza.
#Aunque no lo es en varianza simplemente por la anomal�a de la pandemia


#Jugamos un poco -----------------------------------------------------------------------------------------------------------------------
#Veamos qu� ocurre si medimos las diferencias relativas de la serie en lugar de las absolutas: (la diferenciamos)

autoplot(diff(train2)) + ggtitle("�ndice peque�os comercios") + xlab("Tiempo") + ylab("�ndice")
ggtsdisplay(diff(train2), lag=48) 

#Ahora ya la tenemos algo m�s estacionaria con respecto a media y varianza (aproximadamente). Ha mejorado
#Veamos su correlograma:

ggAcf(diff(train2), 48)
ggPacf(diff(train2), 48)

#Es un modelo al que le hemos hecho diferenciaci�n en la parte estacionaria, pero no as� en la estacional
#Es una serie que ajustamosa un modelo autoregresivo de orden 1 en la parte regular, y de orden 1 en la parte estacional
#Los �ndices de medias m�viles de momento son 0.

#Se observa a�n en los correlogramas la estructura estacional y no estacionaria de la seria
#Estudiando las autocorrelaciones simples vemos que siguen saliendo correlaciones de las bandas de confianza, 
#por lo que tendremos que seguir modelizando


#Volvemos a diferenciar la parte estacionaria para intentar eliminar las grandes autocorrelaciones parciales que aparecen
#Se ve que los retardos m�ltiplos de 3 siguen teniendo una correlacion muy alta, por eso hace una diferenciacion de orden
#3 sobre la de orden 1:


autoplot(diff(diff(train2),3)) + ggtitle("�ndice peque�os comercios") + xlab("Tiempo") + ylab("�ndice")
ggAcf(diff(diff(train2),3), 48)
ggPacf(diff(diff(train2),3), 48)

new <- diff(diff(train2),3)

#Mejoran estas autocorrelaciones, ahora hay menos que sobrepasen de las bandas de confianza
#Vemos que hay falta de estacionariedad si miramos las autocorrelaciones simples
#Diferenciando ahora estacionalmente para mejorar esto:

autoplot((diff(new, 12))) + ggtitle("�ndice peque�os comercios") + xlab("Tiempo") + ylab("�ndice")
ggAcf(diff(diff(new),12),48)

#Podemos considerar que hemos alcanzado la estacionariedad?, pues el correlograma simple cambia de forma abrupta?.
#Sigue habiendo estacionalidad

ggPacf(diff(diff(new),12),48)

#Con la serie triplemente diferenciada vemos, en el PACF, que la autocorrelacion de orden 1 y tambi�n la de orden 4
#Seguimos teniendo muchas correlaciones muy elevadas


#-----------------------------------------------------------------------------------------------------------------------------------

#Vamos a seguir ahora la metodolog�a bob-jenkings para ajustar de la mejor manera el modelo ARIMA:
#Con respecto a donde nos habiamos quedado, tomamos logaritmos a ver si mejoramos el tema de la varianza: ---> Esta es la parte importante

autoplot(diff(diff(log(train2), 12))) + ggtitle("�ndice peque�os comercios") + xlab("Tiempo") + ylab("�ndice")

#Hacemos un cbind para graficar todo lo que hemos hecho hasta el momento y ver si hay mejoras: (Cambiamos el orden)

cbind("�ndice" = train2,
      "log(�ndice)" = log(train2), #Se ve la reducci�n de la varianza en la escala del eje
      "1�Dif log(�ndice)" = diff(log(train2)),  #Conseguimos la serie estacionaria
      "Dif Anual log(�ndice)" = diff(diff(log(train2)),12)) %>%   #Conseguimos perder la serie estacional
    autoplot(facets=TRUE) +
    xlab("A�o") + ylab("�ndice") +
    ggtitle("�ndice Comercios Menores")

#Probamos a calcular la derivada estacional de la derivada parcial?

ggtsdisplay(diff(diff(log(train2)), 12), lag=48)

#Vemos que destacan el primer orden, el2, el 6, el 11 y el 12. Habr� que hacer un modelo ARIMA para solucionar eso.
#Qu� modelo? ---> Ni idea probablemente (2,0,0)(0,1,1)12
#El primer 2 es el orden de la diferenciacion que vamos a hacer para solucionar el tema del orden1
#El primer 1 es un medias m�viles para modelizar la autocorrelaci�n de orden 12
#El orden 11 quedar�a a�n por solucionarlo, pero no s� c�mo---> quiz�s podr�amos diferenciar estacionalmente con orden 11

serie_dif <- diff(diff(log(train2)), 12)

ggtsdisplay(diff((serie_dif), 11), lag=48)

#O quiz�s viendo que hab�a una estacionalidad en el orden 6?

ggtsdisplay(diff((serie_dif), 6), lag=48)

#Si juntamos las dos:

ggtsdisplay(diff((diff((serie_dif), 6)),11), lag=48) #---> No es buena idea, aparece una corr muy grande en el orden 5

#Lo hacemos de forma manual en base al modelo que hab�amos escrito arriba

serie_dif %>% Arima(order=c(2,0,0), seasonal=c(0,0,1)) %>% residuals() %>% ggtsdisplay()

#Vemos que los residuos est�n incorrelados, siendo aleatorios y teniendo media 0
#Las autocorrelaciones adem�s est�n la gran mayor�a dentro de las bandas de confianza, y las que las traspasan lo hacen 
#por una distancia que podemos asumir en la pr�ctica

#Creo que no tenemos que hacer al ARIMA sobre la serie_dif, sino sobre la original!!!!!

train2 %>% Arima(order=c(2,1,0), seasonal=c(0,1,2)) %>% residuals() %>% ggtsdisplay()  #He elegido ese modelo porque hago m�s abajo el autoarima! ----> Justificar por qu� escojo ese modelo

#Vamos a hacerlo ahora de forma autom�tica con la funci�n autoarima:  ---> Buena idea
#-----------------------------------------------------------------------------------

model_auto <- auto.arima(serie_dif, seasonal=TRUE)
model_auto_2 <- auto.arima(train2, seasonal=TRUE) #------> A partir del original
checkresiduals(model_auto) #Vemos que recomienda el (2,0,0)(0,0,2)[12]
checkresiduals(model_auto_2) #Recomienda el (2,1,0)(0,1,2)

#Vemos que sigue habiendo �rdenes que siguen depasando las bandas de confianza, pero ahora por menos
#Los residuos tienen que tener media 0, varianza constante y ausencia de correlacion para cualquier retardo
#Este modelo puede valer porque el p-valor mayor de 0.01 acepta la hip�tesis de que los residuos est�n incorrelados
#Para ver los detalles del modelo:

print(model_auto) #---> El AIC y el BIC s�lo nos sirve como medida comparativa, no es como el resto de errores
#Cuanto mejor AIC y BIC mejor
#La funcion autoarima compara los AIC para ejegir su mejor modelos

#Mostramos la tabla con los errores:

knitr::kable(accuracy(model_auto), digits=4, caption="Medidas del ajuste", "simple")

#Finalmente calculamo las predicciones:

autoplot(forecast(model_auto, h=12))
autoplot(forecast(model_auto, h=24))

#Recordemos que hemos tomado logaritmos, luego para calcular los valores reales hacemos exponentes:

pred <- forecast(model_auto, h=24)
cbind("prediccion" = exp(pred$mean),
      "L80" = exp(pred$lower),
      "U80" = exp(pred$upper)) %>% knitr::kable(caption="Predicciones")

#No entiendo por qu� me salen esos valores en las predicciones haciendo exponentes, si solo hemos hecho logaritmos una vez...
#Quiz�s porque hemos estado haciendo esto sobre serie_dif, cuando en realidad deber�amos hacerlo sobre train_2:

model_auto <- auto.arima(train2, seasonal=TRUE)
print(model_auto)
checkresiduals(model_auto)  #Nos recomiendo el (2,1,0)(0,1,2)
model_auto2 <- auto.arima(log(train2), seasonal=TRUE)
checkresiduals(model_auto2)  #Ha bajado el p-value
print(model_auto)
print(model_auto2)  #Tiene un AIC m�s bajo el primer modelo, nos quedamos con �l
knitr::kable(accuracy(model_auto), digits=4, caption="Medidas del ajuste")
autoplot(forecast(model_auto, h=12))
autoplot(forecast(model_auto, h=24))

pred <- forecast(model_auto, h=24)
pred2 <- forecast(model_auto2, h=24)
cbind("prediccion" = pred$mean,
      "L80" = pred$lower,
      "U80" = pred$upper) %>% knitr::kable(caption="Predicciones", "simple")

#Comparamos con los valores que hab�amos reservado antes:

autoplot(indice) +
  autolayer(forecast(model_auto, h=24), series="autom�tico", PI=FALSE) +
  ggtitle("Predicciones y Valores Reales") + xlab("Mes") + ylab("Indice") +
  guides(colour=guide_legend(title="Forecast"))

#Podemos tambi�n inclu� ualquier modelo manual que hayamos realizado antes.
#Vemos como todos se pierden con la crisis del coronavirus y no la preveen, evidentemente.

#Podemos volver a emplear la funci�n definida anteriormente para acceder a los errores:

a <- pred$mean
a <- as.numeric(pred$mean)
print(a)

values <- window(indice, start=c(2020,1))
values <- as.numeric(values)  

params <- getPerformance(as.numeric(pred$mean), values)
params

#------------------------------------------------------------------------------



ggtsdisplay(diff(diff(train2),12), lag=48)  #Tenemos este modelo analizado, lo guardamos en otra variable

serie_dif <- diff(diff(train2), 12)

autoplot(serie_dif)


model_auto <- auto.arima(serie_dif, seasonal=TRUE)  
checkresiduals(model_auto)

#Empezamos diferenciandolo una vez en la parte no estacional:

serie_dif %>% Arima(order=c(2,0,0), seasonal=c(0,0,1)) %>% residuals() %>% ggtsdisplay()
serie_dif_2 <- diff(diff(diff(serie_dif)), 12)

autoplot(serie_dif_2)
ggtsdisplay(serie_dif_2, lag=48)


#Nosotros nos hemos quedado con un ARIMA (2,0,0)(0,0,1) mejor a partir de serie_dif

checkresiduals(model_auto)
 









rm(list=ls())

library(questionr)
library(ggplot2)
library(gridExtra)
library(DMwR2)

getwd()                                                         #Directorio de trabajo
setwd('C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining')       #Lo fijamos
source("Funciones_R.R")                                         #Cargamos algunas funciones

datos_test <- readRDS("C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining/FugaClientes_test.RDS")
datos_training <- readRDS("C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining/FugaClientes_Training.RDS")

#Vemos que los datos de test contienen todas las variables que los de training, excepto la columa objetivo "Fuga"
#Además, los ID del test no los encontramos en los datos del training (parece)
#Empezaremos a trabajar con el set de training;

str(datos_training)

#Vemos que tenemos solo tres variables numericas (Antiguedad y Facturas) y que nuestra variable onetivo es bin
#Además parece que todos tienen asignado el tipo correcto de variable (factor/num)

names(datos_training)
summary(datos_training)

#Podemos ver cómo se distribuye el reparto en las categorías de una variable cualitativa:
#En el summary hemos visto que no hay nada raro a priori

questionr::freq(datos_training$MetodoPago)  

#Vamos a inspeccionar ahora gráficamente la información, buscando valores extraños:
#La funciones dfplot_box y dfplot_his están diseñadas para devolver una lista de boxlots o histogramas de las
#variables continuas junto con una lista de gráficos de barras para las variables discretas

listaGraf <- dfplot_box(datos_training[,-1]) 
listaHist<- dfplot_his(datos_training[,-1]) 

gridExtra::marrangeGrob(listaGraf, nrow = 3, ncol = 3)
gridExtra::marrangeGrob(listaHist, nrow = 3, ncol = 3)

#No vemos nada excesivamente extraño. Cabe la pena comentar que la media está un poco por debajo de la mediana
#en la factura mensual y con la total ocurre lo contrario. Este último caso tiene sentido, entra gente en 
#la compañía que no ha pagado nada históricamente y que aumenta la densidad de la variable en la parte
#baja del eje.

#Estos valores en la factura total que se encuentran un poco alejados del tercer rango
#intercuartílico,no parece que sea sufuciente como para catalogarlos como outliers

#Los missings de variables no conocidas ya están como NA
#Como no vemos errores graves, pasamos a la gestión de Missings y de Outliers:
#Antes es importante separar la variable objetivo de los predictores:

varObj<-datos_training$Fuga
datos_training<-datos_training[,-21] #Generamos un dataframe excepto las variables objetivo
datos_training$ID<-as.numeric(datos_training$ID) #Modificamos para que no haya correlacion
str(datos_training)

#Empezamos buscando los valores atípicos de las variables continuas. Para ello usamos Atipicosamissing:
#¿criterio rangos intercuartilicos/mediana?

sapply(Filter(is.numeric, datos_training),        
       function(x) atipicosAmissing(x)[[2]])/nrow(datos_training)

#No tenemos valores atipicos en las funciones continuas 
#Probamos aun asi con un metodo multivariante, que no mira solamente la variante sino las instancias.
#Depende mucho de k (numero de vecinos) y no acepta valores NA

outlier.scores <- lofactor(na.omit(Filter(is.numeric, datos_training)), k=20)
plot(density(outlier.scores))

# Extraemos la posicion de los 5 registros más extremos 
outliers <- order(outlier.scores, decreasing=T)[1:5]

# Me guardo los ID de estos registros para luego
(na.omit(Filter(is.numeric, datos_training))[outliers,] ->out5)


# Me guardo los ID de estos registros para luego
out5$ID -> ID_out5

#Lo comparamos con el valor de las medias y las medianas de esas variables para ver qué ocurre

data.frame(t(round(apply(na.omit(Filter(is.numeric, datos_training)),2,mean),3)))
data.frame(t(round(apply(na.omit(Filter(is.numeric, datos_training)),2,median),3)))

#Podríamos pensar que la factura total es baja con respecto a la media, sin embargo,
#Son datos que no destacan especialmente. Comparamos por ejemplo con las menos facturas mensuales y totales:

datos_training %>% slice_min(FacturaMes)
datos_training %>% slice_min(FacturaTotal)

#Tiene sentido ademñas que facturas totales bajas vayam acompañadas de pocos años de antiguëdad.
#Somos conservadores con los outliers y consideramos que no hay ninguno

#Pasamos ahora a los valores perdidos:

#Busco si existe algÃºn patrÃ³n en los missings, que me pueda ayudar a entenderlos

corrplot(cor(is.na(datos_training[colnames(
  datos_training)[colSums(is.na(datos_training))>0]])),method = "ellipse",type = "upper") 

#No parece que haya ninguna relacion entre los missings
#Estudiamos la proporción de missings que existe en cada variable:

prop_missingsVars<-apply(is.na(datos_training),2,mean)
t<-data.frame(sort(prop_missingsVars*100, decreasing = T))
names(t)<-"% Missing por Variable"
t

#La variabe con un porcentaje de missings más elevado es el método de pago, pero este sólo está en torno
#al 7%, que nos parece un buen porcentaje.

#Estudiamos ahora el porcentaje de missings por observación:

datos_training$prop_missings<-apply(is.na(datos_training),1,mean) 
summary(datos_training$prop_missings)
datos_training %>% arrange(desc(prop_missings)) %>% slice_head(n=5)

#A las instancias a las que le faltan más información solo les falta un 14% (3 variables como NA)
#Es un porcentaje más que asumible y no necesitaremos eliminar ninguna observación

#Imputamos ahora valores a estos missings para las variables cuantitativas:
#Lo haremos con la función ImputacionCuant y lo haremos por aletoriedad, aunque es importante destacar que esta
#aleatoriedad está atada a una distibución de probabilidad determinada, por la variable,
#sin ser completamente azarosa

datos_training_al <- copy(datos_training)
datos_training_avg <- copy(datos_training)   #Guardamos otras copias del dataframe para aplciar 
datos_training_med <- copy(datos_training)   #distintas funciones a la imputación de NA'S

datos_training_al[,as.vector(which(sapply(datos_training, class)=="numeric"))]<-sapply(
  Filter(is.numeric, datos_training),function(x) ImputacionCuant(x,"aleatorio"))

datos_training_avg[,as.vector(which(sapply(datos_training, class)=="numeric"))]<-sapply(
  Filter(is.numeric, datos_training),function(x) ImputacionCuant(x,"media"))

datos_training_med[,as.vector(which(sapply(datos_training, class)=="numeric"))]<-sapply(
  Filter(is.numeric, datos_training),function(x) ImputacionCuant(x,"mediana"))
#Con las vaiables categóricas podemos tomar la moda o la variable aleatoria (lo haremos con aleatoria)

datos_training_al[,as.vector(which(sapply(datos_training, class)=="factor"))]<-sapply(
  Filter(is.factor, datos_training),function(x) ImputacionCuali(x,"aleatorio"))

datos_training_avg[,as.vector(which(sapply(datos_training, class)=="factor"))]<-sapply(
  Filter(is.factor, datos_training),function(x) ImputacionCuali(x,"aleatorio"))

datos_training_med[,as.vector(which(sapply(datos_training, class)=="factor"))]<-sapply(
  Filter(is.factor, datos_training),function(x) ImputacionCuali(x,"aleatorio"))

# A veces se cambia el tipo de factor a character al imputar, 
# indicamos que es factor
datos_training_al[,as.vector(which(sapply(datos_training, class)=="character"))] <- lapply(
  datos_training[,as.vector(which(sapply(datos_training, class)=="character"))] , factor)

datos_training_avg[,as.vector(which(sapply(datos_training, class)=="character"))] <- lapply(
  datos_training[,as.vector(which(sapply(datos_training, class)=="character"))] , factor)

datos_training_med[,as.vector(which(sapply(datos_training, class)=="character"))] <- lapply(
  datos_training[,as.vector(which(sapply(datos_training, class)=="character"))] , factor)

#puede que quede algún missing en las variables cuantitativas:

indx <- apply(datos_training_al, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(datos_training_avg)[indx]

#Efectivamente, nos damos cuenta de que quedan NA's
#Pasamos otra vez la función:

datos_training_al[,as.vector(which(sapply(datos_training, class)=="numeric"))]<-sapply(
  Filter(is.numeric, datos_training_al),function(x) ImputacionCuant(x,"aleatorio"))

apply(datos_training_al, 2, function(x) any(is.na(x)))

#Hacemos lo mismo con los otros dos datasets

indx <- apply(datos_training_avg, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(datos_training_avg)[indx]

#No hay ninguno para el avg

indx <- apply(datos_training_med, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(datos_training_med)[indx]

#Tampoco para el indx. Ya no nos quedan 
#Consideramos que los datos están depurados y los guardamos:

saveRDS(datos_training_al,"datos_training_al.RDS")    #Podriamos quedarnos con el mejor y aplicarle
saveRDS(datos_training_avg,"datos_training_avg.RDS")  #la inputacion por moda en lugar de por aleat.
saveRDS(datos_training_med,"datos_training_med.RDS")  #a las variables categóricas

#A partir de aquí seguimos con datos_training_avg para ver si mejoramos los resultados obtenidos anteriormente

datos_training_avg<-readRDS('C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining/datos_training_avg.RDS')

#El contrato está un poco desbalanceado. Vemos que los contratos mes a mes están bastante más representados
#que los de un año o los de dos. Podemos unir estas dos categorías en una nueva columna para ver si es
#representativa para el modelo


datos_training_avg$ContratoBI <- as.character(datos_training_avg$Contrato)
datos_training_avg$ContratoBI[datos_training_avg$ContratoBI == "One year"] <- "+Month"
datos_training_avg$ContratoBI[datos_training_avg$ContratoBI == "Two year"] <- "+Month"
datos_training_avg$ContratoBI <- as.factor(datos_training_avg$ContratoBI)

questionr::freq(datos_training_avg$Contrato)
questionr::freq(datos_training_avg$ContratoBI)

#Creamos un par de variables de control:

datos_training_avg$aleatorio<-runif(nrow(datos_training_avg))

datos_training_avg$aleatorio2<-runif(nrow(datos_training_avg))

str(datos_training_avg)

#Buscamos ya la relaciones entre las variables del input y la objetivo con el test de cramer:

graficoVcramer(datos_training_avg,varObj) 

unique(datos_training_avg$prop_missings) #Al no haber 6 valores diferentes no aparece prop_missings en la relación
questionr::freq(datos_training_avg$prop_missings) 

#Vamos a categorizar la variable entre si no tiene missings o si tiene alguno:

datos_training_avg$prop_missings_cat <- as.character(datos_training_avg$prop_missings)

datos_training_avg$prop_missings_cat[datos_training_avg$prop_missings_cat != "0"]  <- "Con Missings"
datos_training_avg$prop_missings_cat[datos_training_avg$prop_missings_cat == "0"] <- "Sin Missings"
datos_training_avg$prop_missings_cat <- as.factor(datos_training_avg$prop_missings_cat)


#Quizás esa variable sea de más ayuda que la de la proporcion missings
#Que además la vamos a pasar a factor:

datos_training_avg$prop_missings <- as.factor(datos_training_avg$prop_missings)

#vamos a ver el efecto de algunas variables gráficamente:

m1<-mosaico_targetbinaria(datos_training_avg$Contrato,varObj,"Contrato")  #Tablas de contingencias en gráfico 
m2<-mosaico_targetbinaria(datos_training_avg$MetodoPago,varObj,"Pago") 
m3<-mosaico_targetbinaria(datos_training_avg$Int_serv,varObj,"Servicio") 

bx1<-boxplot_targetbinaria(datos_training_avg$Antig.fc.edad,varObj,"Antig.")  
bx2<-boxplot_targetbinaria(datos_training_avg$FacturaMes,varObj,"Factura_mes")
bx3<-boxplot_targetbinaria(datos_training_avg$FacturaTotal,varObj,"Factura_tot")

h1<-barras_targetbinaria(datos_training_avg$Contrato,varObj,"Contrato") 
h2<-barras_targetbinaria(datos_training_avg$MetodoPago,varObj,"Pago") 
h3<-barras_targetbinaria(datos_training_avg$Int_serv,varObj,"Servicio") 

marrangeGrob(list(h1,h2,h3,bx1,bx2,bx3),nrow = 3, ncol = 2)

#En base a estos gráficos, quizás también podríamos probar reduciendo el numero de categorías en
#pago y servicios, estableciendo electronic_check/otros y fibra/otros como categorías por
#mostrar esas dos un mayor número de fugados (explorar con el plot todas las categorías)

#Con respecto a las variables continuas observamos que se van más las personas que llevan menos tiempo,
#con una factura mensual mayor y lo que tiene una factura total menor (va de la mano de la edad)

#¿No se están pisando un poco la antiguedad y la factura total?

#El cambio de la frecuencia relativa de las categorías de cada variable dependiendo de si el cliente se ha fugado
#o no, indica que estas variables serán importantes de cara al modelo y que tendrán una capacidad predictora

#Busco las mejores transformaciones para las variables numÃ©ricas con respesto a la variable binaria
#La relación se mide por el coeficiente V de Cramer
#Quitamos la columna de propmissings porque provoca errores al ser continua pero tener solo 4 valores distintos
#Tenemos que categorizarla más adelante

data_tavg<-cbind(datos_training_avg,Transf_Auto(Filter(is.numeric, datos_training_avg),varObj))

#Volvemos a hacer Cramer:

graficoVcramer(data_tavg,varObj)  #-----> Parece que los resultados son mejores en el avg?¿ 









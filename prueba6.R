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
ggplot(out5)


# Me guardo los ID de estos registros para luego
out5$ID -> ID_out5

#Lo comparamos con el valor de las medias y las medianas de esas variables para ver qué ocurre

data.frame(t(round(apply(na.omit(Filter(is.numeric, datos_training)),2,mean),3)))
data.frame(t(round(apply(na.omit(Filter(is.numeric, datos_training)),2,median),3)))

#Podríamos pensar que la factura total es baja con respecto a la media, sin embargo,
#Son datos que no destacan especialmente. Comparamos por ejemplo con las menos facturas mensuales y totales:

datos_training %>% slice_max(FacturaTotal)
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
datos_training_al <- copy(datos_training)   #distintas funciones a la imputación de NA'S

datos_training_al[,as.vector(which(sapply(datos_training, class)=="numeric"))]<-sapply(
  Filter(is.numeric, datos_training),function(x) ImputacionCuant(x,"aleatorio"))

datos_training_avg[,as.vector(which(sapply(datos_training, class)=="numeric"))]<-sapply(
  Filter(is.numeric, datos_training),function(x) ImputacionCuant(x,"media"))

datos_training_al[,as.vector(which(sapply(datos_training, class)=="numeric"))]<-sapply(
  Filter(is.numeric, datos_training),function(x) ImputacionCuant(x,"mediana"))
#Con las vaiables categóricas podemos tomar la moda o la variable aleatoria (lo haremos con aleatoria)

datos_training_al[,as.vector(which(sapply(datos_training, class)=="factor"))]<-sapply(
  Filter(is.factor, datos_training),function(x) ImputacionCuali(x,"aleatorio"))

datos_training_avg[,as.vector(which(sapply(datos_training, class)=="factor"))]<-sapply(
  Filter(is.factor, datos_training),function(x) ImputacionCuali(x,"aleatorio"))

datos_training_al[,as.vector(which(sapply(datos_training, class)=="factor"))]<-sapply(
  Filter(is.factor, datos_training),function(x) ImputacionCuali(x,"aleatorio"))

# A veces se cambia el tipo de factor a character al imputar, 
# indicamos que es factor
datos_training_al[,as.vector(which(sapply(datos_training, class)=="character"))] <- lapply(
  datos_training[,as.vector(which(sapply(datos_training, class)=="character"))] , factor)

datos_training_avg[,as.vector(which(sapply(datos_training, class)=="character"))] <- lapply(
  datos_training[,as.vector(which(sapply(datos_training, class)=="character"))] , factor)

datos_training_al[,as.vector(which(sapply(datos_training, class)=="character"))] <- lapply(
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

indx <- apply(datos_training_al, 2, function(x) any(is.na(x) | is.infinite(x)))
colnames(datos_training_al)[indx]

#Tampoco para el indx. Ya no nos quedan 
#Consideramos que los datos están depurados y los guardamos:

saveRDS(datos_training_al,"datos_training_al.RDS")    #Podriamos quedarnos con el mejor y aplicarle
saveRDS(datos_training_avg,"datos_training_avg.RDS")  #la inputacion por moda en lugar de por aleat.
saveRDS(datos_training_al,"datos_training_al.RDS")  #a las variables categóricas

#A partir de aquí seguimos con datos_training_al para ver si mejoramos los resultados obtenidos anteriormente

datos_training_al<-readRDS('C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining/datos_training_al.RDS')

#El contrato está un poco desbalanceado. Vemos que los contratos mes a mes están bastante más representados
#que los de un año o los de dos. Podemos unir estas dos categorías en una nueva columna para ver si es
#representativa para el modelo


datos_training_al$ContratoBI <- as.character(datos_training_al$Contrato)
datos_training_al$ContratoBI[datos_training_al$ContratoBI == "One year"] <- "+Month"
datos_training_al$ContratoBI[datos_training_al$ContratoBI == "Two year"] <- "+Month"
datos_training_al$ContratoBI <- as.factor(datos_training_al$ContratoBI)

questionr::freq(datos_training_al$Contrato)
questionr::freq(datos_training_al$ContratoBI)

#Creamos un par de variables de control:

datos_training_al$aleatorio<-runif(nrow(datos_training_al))

datos_training_al$aleatorio2<-runif(nrow(datos_training_al))

str(datos_training_al)

#Buscamos ya la relaciones entre las variables del input y la objetivo con el test de cramer:

graficoVcramer(datos_training_al,varObj) 

unique(datos_training_al$prop_missings) #Al no haber 6 valores diferentes no aparece prop_missings en la relación
questionr::freq(datos_training_al$prop_missings) 

#Vamos a categorizar la variable entre si no tiene missings o si tiene alguno:

datos_training_al$prop_missings_cat <- as.character(datos_training_al$prop_missings)

datos_training_al$prop_missings_cat[datos_training_al$prop_missings_cat != "0"]  <- "Con Missings"
datos_training_al$prop_missings_cat[datos_training_al$prop_missings_cat == "0"] <- "Sin Missings"
datos_training_al$prop_missings_cat <- as.factor(datos_training_al$prop_missings_cat)


#Quizás esa variable sea de más ayuda que la de la proporcion missings
#Que además la vamos a pasar a factor:

datos_training_al$prop_missings <- as.factor(datos_training_al$prop_missings)

#vamos a ver el efecto de algunas variables gráficamente:

m1<-mosaico_targetbinaria(datos_training_al$Contrato,varObj,"Contrato")  #Tablas de contingencias en gráfico 
m2<-mosaico_targetbinaria(datos_training_al$MetodoPago,varObj,"Pago") 
m3<-mosaico_targetbinaria(datos_training_al$Int_serv,varObj,"Servicio") 



bx1<-boxplot_targetbinaria(datos_training_al$Antig.fc.edad,varObj,"Antig.")  
bx2<-boxplot_targetbinaria(datos_training_al$FacturaMes,varObj,"Factura_mes")
bx3<-boxplot_targetbinaria(datos_training_al$FacturaTotal,varObj,"Factura_tot")

h1<-barras_targetbinaria(datos_training_al$Contrato,varObj,"Contrato") 
h2<-barras_targetbinaria(datos_training_al$MetodoPago,varObj,"Pago") 
h3<-barras_targetbinaria(datos_training_al$Int_serv,varObj,"Servicio") 

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

data_tavg<-cbind(datos_training_al,Transf_Auto(Filter(is.numeric, datos_training_al),varObj))
str(datos_training_al)
#Volvemos a hacer Cramer:

graficoVcramer(data_tavg,varObj)  #-----> Parece que los resultados son mejores en el avg?¿ 

#Vamos ahora a tramificar las variable contínua raíz de la antigüedad para convertirla en categóricas.
#Escogemos esta porque funciona mejor que la antigüedad según el test de Cramer.
#No lo hacemos con la factura mensual o total porque se generan categorías muy poco representadas

tree_Antiguedad <-rpart::rpart(varObj~sqrtxAntig.fc.edad, data = data_tavg, cp=0.01)
tree_Antiguedad
table(tree_Antiguedad$where)
data_tavg$tree_Antiguedad<-factor(tree_Antiguedad$where)
levels(data_tavg$tree_Antiguedad) = c('Mucho','Medio','Poco')
table(data_tavg$tree_Antiguedad)
str(data_tavg$tree_Antiguedad) #Tipo factor


h4<-barras_targetbinaria(varObj,data_tavg$tree_Antiguedad,"Antigüedad")
h4 #No parece tampoco que influya en exceso #Categorías muy subrepresentadas

#----------------------------------------------------------------------------------------------------

#Empezamos a modelizar

data <- data.frame(data_tavg,varObj) #Renombramos
freq(data$varObj) #Importante tenerlo en cuenta a la hora de valorar el modelo 

#Es muy importante mirar la sensibilidad y especificidad

# Le pedimos las posiciones de las variables para tener cuidado y saber filtrar
names(data)

#Hago la partición, empezando un modelo de referencia con las variables originales
set.seed(123456)
trainIndexx <- createDataPartition(data$varObj, p=0.8, list=FALSE)
#data_trainn <- todoo[trainIndex,c(3,4,5,6,7,8,10,11,16,17,18,19,20,25,26,27,30)] #V de Cramer más importante
#data_testt <- todoo[-trainIndex,c(3,4,5,6,7,8,10,11,16,17,18,19,20,25,26,27,30)]

data_train <- data[trainIndexx,]
data_test <- data[-trainIndexx,]

names(data_train)

#Hacemos un modelo con las variables originales:

modelo_0<-glm(varObj~.,data=data_train[,-c(1,21:31)],family=binomial)
summary(modelo_0)

modelo_1<-glm(varObj~., data=data_train,family=binomial)
summary(modelo_1)

pseudoR2(modelo_0,data_train,"varObj") 
pseudoR2(modelo_0,data_test,"varObj")
modelo_0$rank 

pseudoR2(modelo_1,data_train,"varObj")            
pseudoR2(modelo_1,data_test,"varObj")
modelo_1$rank 

impVariablesLog(modelo_0,"varObj")
impVariablesLog(modelo_1,"varObj")



#Parece que las facturas por sus raíces, que en el test de Cramer funcionaban mejor
#Además cambiamos también la antiguedad por su raíz por el mismo motivo.


modelo_2<-glm(varObj~.,data=data_train[,-c(1,6,19,20,21:26,29:31)],family=binomial)
summary(modelo_2)
names(data_train)

#El step de seleccion de variables (funcion que elegira el mejor modelo) los compara con el AIC
#Cuanto menor sea, mejor

#Miramos la pseudoR, que es el equivalente a R2 solo que con una escala diferente
#Con una R2 de 0.4 estamos ya muy contentos (equivale a una 0.8-0.9 = R2)

pseudoR2(modelo_2,data_train,"varObj") #NA muy bajo, hay que mejorar
pseudoR2(modelo_2,data_test,"varObj")
modelo_2$rank #numero de parametros

#Miramos con respectos a V de Cramer cuales son las variables más importantes:

impVariablesLog(modeloIniciall,"varObj") 

#La antiguedad y la factura mensual no están funcionando en absoluto
#Probamos un modelo con la antigëdad tramificada y eliminamos la factura mensual:


names(data_train)

modelo_3<-glm(varObj~.,data=data_train[,-c(1,6,19,20,21:28,29:31)],family=binomial)
summary(modelo_3)

#Parece que sigue sin funcionar la antigëdad, eso nos molesta Quizás más adelante encontremos una interacción:

pseudoR2(modelo_3,data_train,"varObj") #NA muy bajo, hay que mejorar
pseudoR2(modelo_3,data_test,"varObj")
modelo_3$rank #numero de parametros  

impVariablesLog(modelo_3,"varObj")

#Probamos ahora un modelo más sencillo, en el que quitamos las variables que no parecen muy representativas:

modelo_4<-glm(varObj~.,data=data_train[,-c(1:6,11,12,18:28,29:31)],family=binomial)
summary(modelo_4)

#Hemos subido un poco el AIC pero ganamos en simpleza


pseudoR2(modelo_4,data_train,"varObj") #NA muy bajo, hay que mejorar
pseudoR2(modelo_4,data_test,"varObj")
modelo_4$rank #numero de parametros

#Empeora un poco el modelo, aunque baja el numero de parámetros
#Podemos intentar incluir una interaccion. El método de pago, por ejemplo, nos gustaría que fuese más 
#relevante de lo que es. Quizás nos diga algo más al cruzarla con el int_serv:
#Cogemos el modelo 22 como base que no funcionaba mal

modelo_5<-glm(varObj~Contrato+TV_streaming+Mayor65+Soporte_tecnico+Peliculas
              +Int_serv+VariasLineas+Seguridad+Fact_sinPapel+Telf_serv+raiz4FacturaTotal+TV_streaming
                +MetodoPago*Int_serv, data=data_train,family=binomial)

summary(modelo_5)

#Encontramos una interaccion entre el no tener servicio y el haber pagado con tarjeta

pseudoR2(modelo_5,data_train,"varObj") #NA muy bajo, hay que mejorar
pseudoR2(modelo_5,data_test,"varObj")
modelo_5$rank #numero de parametros

freq(data_trainn$MetodoPago)
impVariablesLog(modelo_5,"varObj")



#Validacion cruzada repetida para elegir entre todos:

#copia de la variable original
auxVarObj<- data$varObj

#formateo la variable objetivo para que funcione el codigo
data$varObj<-make.names(data$varObj)

totall<-c()
modeloss<-sapply(list(modelo_0,modelo_1,modelo_2,modelo_3,modelo_4,modelo_5),formula)



#ClassProbs nos devuelve la robabilidad que tiene las instancias de ser 0 o 1.
for (i in 1:length(modeloss)){
  set.seed(1712)
  vcr<-train(as.formula(modeloss[[i]]), data = data,
             method = "glm", family="binomial",metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  totall<-rbind(totall,data.frame(roc=vcr$resample[,1],modeloo=rep(paste("Modelo",i),
                                                                   nrow(vcr$resample))))
}
boxplot(roc~modeloo,data=totall,main="Area bajo la curva ROC") 

#Vemos los valores mediosy la desviacion de todos los modelos

aggregate(roc~modeloo, data = totall, mean) 
aggregate(roc~modeloo, data = totall, sd) 

#recupero la variable objetivo en su formato
data$varObj<-auxxVarObj

modelo_0$rank
modelo_1$rank 
modelo_2$rank
modelo_3$rank
modelo_4$rank
modelo_5$rank

freq(datoss$Fuga) #Cambiamos el punto de corte de la probabilidad estimada
#El modelo considera que una instancia es 0 o 1 si la probabilidad esta por encima o debajo de 0.5
#Pero esto es correcto si nuestros datos tienen una distribucion simetrica, que no es el caso

hist_targetbinaria(predict(modelo33, newdata=data_test,type="response"),data_test$varObj,"probabilidad")

#Los que son 0 los reconoce, pero los que son 1 para nada. En muchas ocasiones le da una probabilidad de 
#entre 0 y 0.25 a instancias que son 1... Es verdad que hay menos 1's.
#Si corto por el 0.5 cometería muchos falsos negativos, tendría que echar el punto de corte, un poco más
#a la izquerda

#probamos dos
sensEspCorte(modelo33,data_test,"varObj",0.5,"1")
sensEspCorte(modelo33,data_test,"varObj",0.265,"1")

#Especificidad: Capacidad de reconocer a los 0's
#Sensibilidad: Capacidad de reconocer a los 1's

#Hacemos ahora una prueba algo más compleja, generando una rejilla con diferentes puntos de corte:

posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo33,data_test,"varObj",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden)
plot(rejilla$posiblesCortes,rejilla$Accuracy)
rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]

#Los comparamos:

sensEspCorte(modelo44,data_test,"varObj",0.36,"1")  
sensEspCorte(modelo44,data_test,"varObj",0.46,"1")

#Es mejor el punto de corte que hicimos probamos a ojo, balancea más sensibilidad y especificidad y
#encima tiene mas accuracy...

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
pseudoR2(modelo33,data_train,"varObj")
pseudoR2(modelo33,data_test,"varObj")
roc(data_train$varObj, predict(modelo33,data_train,type = "response"), direction="<")
roc(data_test$varObj, predict(modelo33,data_test,type = "response"), direction="<")
sensEspCorte(modelo33,data_train,"varObj",0.265,"1")
sensEspCorte(modelo33,data_test,"varObj",0.265,"1")

#Obtenemos la matriz de correlacion:

# Generar el factos con las clases estimadas en test
pred_test<-factor(ifelse(predict(modelo33,data_test,type = "response")>0.265,1,0))

# Tablas marginales
table(pred_test)
table(data_test$varObj)

# Matriz de confusiÃ³n
confusionMatrix(pred_test,data_test$varObj, positive = '1')

#El modelo es muy malo tiene un P-Value de 0.365, debería ser mucho menor para segurarnos de que el modelo esta
#aportando algo. Los 1's no los distingue practicamente.

#Tenemos un kappa muy bajo también, que tomaria valor = 1 si no hubiese ni falsos positivos ni falsos negativos

#Probamos ahora con el método de selección de variables: Primero solo con las variabes originales
#Empezamos definiendo el modelo vacío y el modelo full:

full <- glm(varObj~.,data=data_train[,-c(1,21:31)],family=binomial)
null <- glm(varObj~1,data=data_train[,-c(1,21:31)],family=binomial)

modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both", trace = F)
summary(modeloStepAIC)
pseudoR2(modeloStepAIC,data_train,"varObj") 
pseudoR2(modeloStepAIC,data_test,"varObj")

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward", trace = F)
summary(modeloBackAIC)
pseudoR2(modeloBackAIC,data_train,"varObj") 
pseudoR2(modeloBackAIC,data_test,"varObj")

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)), trace = F)
summary(modeloStepBIC)
pseudoR2(modeloStepBIC,data_train,"varObj") 
pseudoR2(modeloStepBIC,data_test,"varObj")

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)), trace = F)
summary(modeloBackBIC)
pseudoR2(modeloBackBIC,data_train,"varObj") 
pseudoR2(modeloBackBIC,data_test,"varObj")

#No hemos mejorado lo que teníamos previamente, probamos añadir ahora las interacciones:

#Trans e interacciones

formIntT<-formulaInteracciones(data_train,32)
fullIntT <- glm(formIntT,data=data_train,family=binomial)
null2 <- glm(varObj~1,data=data_train,family=binomial)

summary(fullIntT)


modeloStepAIC_transInt<-step(null2, scope=list(lower=null2, upper=fullIntT), direction="both", trace = F)
summary(modeloStepAIC_transInt)
pseudoR2(modeloStepAIC_transInt,data_train,"varObj") 
pseudoR2(modeloStepAIC_transInt,data_test,"varObj")

modeloStepAIC_transInt$rank

modeloStepBIC_transInt<-step(null2, scope=list(lower=null2, upper=fullIntT), direction="both",k=log(nrow(data_train)), trace = F)
summary(modeloStepBIC_transInt)
pseudoR2(modeloStepBIC_transInt,data_train,"varObj") 
pseudoR2(modeloStepBIC_transInt,data_test,"varObj")

modeloStepAIC_transInt$rank 
modeloStepBIC_transInt$rank

#Vemos cual es el mejor por validacion cruzada

auxVarObj<- data$varObj

#formateo la variable objetivo para que funcione el codigo
data$varObj<-make.names(data$varObj)

totall<-c()
modeloss<-sapply(list(modeloStepAIC,modeloBackAIC,modeloStepBIC,modeloBackBIC
                      ,modeloStepAIC_transInt,modeloStepBIC_transInt),formula)



#ClassProbs nos devuelve la robabilidad que tiene las instancias de ser 0 o 1.
for (i in 1:length(modeloss)){
  set.seed(1712)
  vcr<-train(as.formula(modeloss[[i]]), data = data,
             method = "glm", family="binomial",metric = "ROC",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      summaryFunction=twoClassSummary,
                                      classProbs=TRUE,returnResamp="all")
  )
  totall<-rbind(totall,data.frame(roc=vcr$resample[,1],modeloo=rep(paste("Modelo",i),
                                                                   nrow(vcr$resample))))
}
boxplot(roc~modeloo,data=totall,main="Area bajo la curva ROC") 

#Interpretamos ahora nuesto mejor modelo:

modelo_fin<-glm(formula(modeloStepAIC_transInt),
             data=data,family=binomial)

summary(modelo_fin)
pseudoR2(modelo_fin,data,"varObj") 

epiDisplay::logistic.display(modelo_fin, simplified = TRUE)

str(modelo_fin)

#Buscamos un nuevo punto de corte para el modelo final:


freq(data$varObj) #Cambiamos el punto de corte de la probabilidad estimada
#El modelo considera que una instancia es 0 o 1 si la probabilidad esta por encima o debajo de 0.5
#Pero esto es correcto si nuestros datos tienen una distribucion simetrica, que no es el caso

hist_targetbinaria(predict(modelo_fin, newdata=data_test,type="response"),data_test$varObj,"probabilidad")

#Los que son 0 los reconoce, pero los que son 1 para nada. En muchas ocasiones le da una probabilidad de 
#entre 0 y 0.25 a instancias que son 1... Es verdad que hay menos 1's.
#Si corto por el 0.5 cometería muchos falsos negativos, tendría que echar el punto de corte, un poco más
#a la izquerda

#probamos dos
sensEspCorte(modelo_fin,data_test,"varObj",0.5,"1")
sensEspCorte(modelo_fin,data_test,"varObj",0.265,"1")

#Especificidad: Capacidad de reconocer a los 0's
#Sensibilidad: Capacidad de reconocer a los 1's

#Hacemos ahora una prueba algo más compleja, generando una rejilla con diferentes puntos de corte:

posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo_fin,data_test,"varObj",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden)
plot(rejilla$posiblesCortes,rejilla$Accuracy)
rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]

#Los comparamos:

sensEspCorte(modelo_fin,data_test,"varObj",0.29,"1")  
sensEspCorte(modelo_fin,data_test,"varObj",0.49,"1")

#Es mejor el punto de corte que hicimos probamos a ojo, balancea más sensibilidad y especificidad y
#encima tiene mas accuracy...

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
pseudoR2(modelo33,data_train,"varObj")
pseudoR2(modelo33,data_test,"varObj")
roc(data_train$varObj, predict(modelo33,data_train,type = "response"), direction="<")
roc(data_test$varObj, predict(modelo33,data_test,type = "response"), direction="<")
sensEspCorte(modelo33,data_train,"varObj",0.265,"1")
sensEspCorte(modelo33,data_test,"varObj",0.265,"1")

#Obtenemos la matriz de correlacion:

# Generar el factos con las clases estimadas en test
pred_test<-factor(ifelse(predict(modelo_fin,data_test,type = "response")>0.29,1,0))

# Tablas marginales
table(pred_test)
table(data_test$varObj)

# Matriz de confusiÃ³n
confusionMatrix(pred_test,data_test$varObj, positive = '1')

datos_test <- readRDS("C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining/FugaClientes_test.RDS")

#Tenemos que generar en el test la misma estructura que teniamos antes:


datos_test$Mayor65<-as.character(datos_test$Mayor65)



FacturaMes<-scale(datos_test$FacturaMes)
FacturaMes<-FacturaMes+abs(min(FacturaMes,na.rm=T))*1.0001
raiz4FacturaMes <- (FacturaMes)^(1/4)
raiz4FacturaMes <- as.numeric(raiz4FacturaMes)
datos_test$raiz4FacturaMes <- raiz4FacturaMes

FacturaTotal<-scale(datos_test$FacturaTotal)
FacturaTotal<-FacturaTotal+abs(min(FacturaTotal,na.rm=T))*1.0001
raiz4FacturaTotal <- (FacturaTotal)^(1/4)
raiz4FacturaTotal <- as.numeric(raiz4FacturaTotal)
datos_test$raiz4FacturaTotal <- raiz4FacturaTotal

pred_test_fin<-factor(ifelse(predict(modelo_fin,datos_test,type = "response")>0.29,1,0))

prediccion <- data.frame(datos_test$ID, pred_test_fin)

saveRDS(prediccion,"FugaPredict_AlejandroSierraFernandez.RDS")


str(datos_test)
str(data_train)






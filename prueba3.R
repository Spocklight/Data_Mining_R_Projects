#Estudiamos ahora la creación de un modelo por regresión lineal:

rm(list=ls())

library(questionr)
library(ggplot2)
library(gridExtra)
library(DMwR2)
library(corrplot)
library(caret)
library(gridExtra)
library(rpart)

getwd()                                                         #Directorio de trabajo
setwd('C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining')       #Lo fijamos
source("Funciones_R.R")                                         #Cargamos algunas funciones

datoss<-readRDS('C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining/datos_training_p2.RDS')

#Separamos la variable objetivo del resto de datos
varObj <- datoss$Fuga
str(varObj)
inputt <- datoss[,-21]

#Creamos un par de variables de control:

inputt$aleatorio<-runif(nrow(inputt))

inputt$aleatorio2<-runif(nrow(inputt))

#Pasamos el ID a valor numérico para que no haya correlaciones con la variable objetivo

inputt$ID<-as.numeric(inputt$ID)

#Buscamos ya la relaciones entre las variables del input y la objetivo con el test de cramer:


graficoVcramer(inputt,varObj) #Me lanca un warning: Chi-squared approximation may be incorrect

unique(inputt$prop_missings) #Al no haber 6 valores diferentes no aparece prop_missings en la relación

#quizás podríamos pasarla a variable cualitativa para meterla en el test

#Vemos que puede haber una relacion significativa con respecto a la antigüedad, el contrato, el servicio
#o el metodo de pago.

#vamos a ver el efecto de algunas variables gráficamente:

m1<-mosaico_targetbinaria(inputt$Contrato,varObj,"Contrato")  #Tablas de contingencias en gráfico 
m2<-mosaico_targetbinaria(inputt$MetodoPago,varObj,"Pago") 
m3<-mosaico_targetbinaria(inputt$Int_serv,varObj,"Servicio") 

bx1<-boxplot_targetbinaria(inputt$Antig.fc.edad,varObj,"Contrato")  #???Solo tiene sentido para las continuas
bx2<-boxplot_targetbinaria(inputt$FacturaMes,varObj,"Pago")
bx3<-boxplot_targetbinaria(inputt$FacturaTotal,varObj,"Servicio")

h1<-barras_targetbinaria(inputt$Contrato,varObj,"Contrato") 
h2<-barras_targetbinaria(inputt$MetodoPago,varObj,"Pago") 
h2h3<-barras_targetbinaria(inputt$Int_serv,varObj,"Servicio") 

marrangeGrob(list(h1,h2,h3,bx1,bx2,bx3),nrow = 3, ncol = 2)

#El cambio de la frecuencia relativa de las categorías de cada variable dependiendo de si el cliente se ha fugado
#o no, indica que estas variables serán importantes de cara al modelo y que tendrán una capacidad predictora

#Vamos ahora a tramificar las variables contínuas para convertirlas en categóricas:

tree_Factura_Total <-rpart::rpart(varObj~FacturaTotal, data = inputt, cp=0.005)
tree_Factura_Total
table(tree_Factura_Total$where)
inputt$tree_Factura_Total<-factor(tree_Factura_Total$where)
levels(inputt$tree_Factura_Total) = c('>198','96.6-198','<68.5', "68.5-96.6")
table(inputt$tree_Factura_Total)
str(inputt$tree_Factura_Total) #Tipo factor

h4<-barras_targetbinaria(varObj,inputt$tree_Factura_Total,"Factura_Total")
h4 #No parece tampoco que influya en exceso #Categorías muy subrepresentadas

#No funciona bien la ramificacion para la variable factura_mes

tree_Factura_Mes <-rpart::rpart(varObj~FacturaMes, data = inputt, cp=0.0015) 
tree_Factura_Mes
table(tree_Factura_Mes$where)

tree_Ant <-rpart::rpart(varObj~Antig.fc.edad, data = inputt, cp=0.005) 
tree_Ant
table(tree_Ant$where) #Categorías muy subrepresentadas, no lo incluímos

#Busco las mejores transformaciones para las variables numÃ©ricas con respesto a la variable binaria
#La relación se mide por el coeficiente V de Cramer
#Quitamos la columna de propmissings porque provoca errores al ser continua pero tener solo 4 valores distintos
#Tenemos que categorizarla más adelante

input_binn<-cbind(inputt[,-21],Transf_Auto(Filter(is.numeric, inputt[,-21]),varObj))
todo_binn<-data.frame(input_binn,varObj)
saveRDS(todo_binn,"todo_binn.RDS")

#Volvemos a hacer Cramer:

graficoVcramer(input_binn,varObj)

#Empezamos a modelizar

todoo <- todo_binn #Renombramos
freq(todoo$varObj) #Importante tenerlo en cuenta a la hora de valorar el modelo 

#Es muy importante mirar la sensibilidad y especificidad

# Le pedimos las posiciones de las variables para tener cuidado y saber filtrar
names(todoo)

#Hago la partición, empezando un modelo de referencia con las variables originales
set.seed(123456)
trainIndexx <- createDataPartition(todoo$varObj, p=0.8, list=FALSE)
#data_trainn <- todoo[trainIndex,c(3,4,5,6,7,8,10,11,16,17,18,19,20,25,26,27,30)] #V de Cramer más importante
#data_testt <- todoo[-trainIndex,c(3,4,5,6,7,8,10,11,16,17,18,19,20,25,26,27,30)]

data_trainn <- todoo[trainIndexx,]
data_testt <- todoo[-trainIndexx,]

names(data_trainn)

#Quitamos el ID, que no influye nada, y empezamos haciendo un modelo con todo 

modeloIniciall<-glm(varObj~.,data=data_trainn[,-c(1)],family=binomial)
summary(modeloIniciall)

#El step de seleccion de variables (funcion que elegira el mejor modelo) los compara con el AIC
#Cuanto menor sea, mejor

#Miramos la pseudoR, que es el equivalente a R2 solo que con una escala diferente
#Con una R2 de 0.4 estamos ya muy contentos (equivale a una 0.8-0.9 = R2)

pseudoR2(modeloIniciall,data_trainn,"varObj") #NA muy bajo, hay que mejorar
pseudoR2(modeloIniciall,data_testt,"varObj")
modeloIniciall$rank #numero de parametros

#Miramos con respectos a V de Cramer cuales son las variables más importantes:

impVariablesLog(modeloIniciall,"varObj") 

#Probamos un modelo sencillo con menos variables: Las que están por encima del raiz4aleatorio:

names(data_trainn)

modelo22<-glm(varObj~.,data=data_trainn[,-c(1,2,4,5,12,15,21,22,24,25,26,28,29)],family=binomial)
summary(modelo22)

pseudoR2(modelo22,data_trainn,"varObj") #NA muy bajo, hay que mejorar
pseudoR2(modelo22,data_testt,"varObj")
modelo22$rank #numero de parametros   

#Hemos bajado el número de parámetros pero tampoco parece que el modelo haya mejorado en exceso

#Y probamos otro incluso más sencillo, con las 10 más improtantes según Cramer

modelo33<-glm(varObj~Contrato+TV_streaming+Mayor65+MetodoPago
              +Int_serv+VariasLineas+Seguridad+Fact_sinPapel+Telf_serv+raiz4FacturaTotal,
             data=data_trainn,family=binomial)
summary(modelo33)


pseudoR2(modelo33,data_trainn,"varObj") #NA muy bajo, hay que mejorar
pseudoR2(modelo33,data_testt,"varObj")
modelo33$rank #numero de parametros

#Empeora un poco el modelo, aunque baja el numero de parámetros
#Podemos intentar incluir una interaccion. El método de pago, por ejemplo, nos gustaría que fuese más 
#relevante de lo que es. Quizás nos diga algo más al cruzarla con el int_serv:
#Cogemos el modelo 22 como base que no funcionaba mal

modelo44<-glm(varObj~Contrato+TV_streaming+Mayor65+Antig.fc.edad+CopiaSeguridad+Soporte_tecnico+Peliculas
              +Int_serv+VariasLineas+Seguridad+Fact_sinPapel+Telf_serv+raiz4FacturaTotal+TV_streaming+
                FacturaMes+CopiaSeguridad+PersCargo+
                raiz4FacturaTotal+sqrtxAntig.fc.edad+MetodoPago*Int_serv, data=data_trainn,family=binomial)
              
summary(modelo44)

pseudoR2(modelo44,data_trainn,"varObj") #NA muy bajo, hay que mejorar
pseudoR2(modelo44,data_testt,"varObj")
modelo44$rank #numero de parametros

freq(data_trainn$MetodoPago)

#Parece que alguna relacion hay, podríamos seguir buscando más.
#Nos molesta que el método de pago no esté mucho más estrellado, que según Cramer tendría que estarlo

#Validacion cruzada repetida para elegir entre todos:

#copia de la variable original
auxxVarObj<-todoo$varObj

#formateo la variable objetivo para que funcione el codigo
todoo$varObj<-make.names(todoo$varObj)

totall<-c()
modeloss<-sapply(list(modeloIniciall,modelo22,modelo33,modelo44),formula)

#ClassProbs nos devuelve la robabilidad que tiene las instancias de ser 0 o 1.
for (i in 1:length(modeloss)){
  set.seed(1712)
  vcr<-train(as.formula(modeloss[[i]]), data = todoo,
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
todoo$varObj<-auxxVarObj

modeloIniciall$rank
modelo22$rank 
modelo33$rank
modelo44$rank

freq(datoss$Fuga) #Cambiamos el punto de corte de la probabilidad estimada
#El modelo considera que una instancia es 0 o 1 si la probabilidad esta por encima o debajo de 0.5
#Pero esto es correcto si nuestros datos tienen una distribucion simetrica, que no es el caso

hist_targetbinaria(predict(modelo44, newdata=data_testt,type="response"),data_testt$varObj,"probabilidad")

#Los que son 0 los reconoce, pero los que son 1 para nada. En muchas ocasiones le da una probabilidad de 
#entre 0 y 0.25 a instancias que son 1... Es verdad que hay menos 1's.
#Si corto por el 0.5 cometería muchos falsos negativos, tendría que echar el punto de corte, un poco más
#a la izquerda

#probamos dos
sensEspCorte(modelo44,data_testt,"varObj",0.5,"1")
sensEspCorte(modelo44,data_testt,"varObj",0.265,"1")

#Especificidad: Capacidad de reconocer a los 0's
#Sensibilidad: Capacidad de reconocer a los 1's

#Hacemos ahora una prueba algo más compleja, generando una rejilla con diferentes puntos de corte:

posiblesCortes<-seq(0,1,0.01)
rejilla<-data.frame(t(rbind(posiblesCortes,sapply(posiblesCortes,function(x) sensEspCorte(modelo44,data_testt,"varObj",x,"1")))))
rejilla$Youden<-rejilla$Sensitivity+rejilla$Specificity-1
plot(rejilla$posiblesCortes,rejilla$Youden)
plot(rejilla$posiblesCortes,rejilla$Accuracy)
rejilla$posiblesCortes[which.max(rejilla$Youden)]
rejilla$posiblesCortes[which.max(rejilla$Accuracy)]

#Los comparamos:

sensEspCorte(modelo44,data_testt,"varObj",0.22,"1")  
sensEspCorte(modelo44,data_testt,"varObj",0.47,"1")

#Es mejor el punto de corte que hicimos probamos a ojo, balancea más sensibilidad y especificidad y
#encima tiene mas accuracy...

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
pseudoR2(modelo44,data_trainn,"varObj")
pseudoR2(modelo44,data_testt,"varObj")
roc(data_trainn$varObj, predict(modelo44,data_trainn,type = "response"), direction="<")
roc(data_testt$varObj, predict(modelo44,data_testt,type = "response"), direction="<")
sensEspCorte(modelo44,data_trainn,"varObj",0.265,"1")
sensEspCorte(modelo44,data_testt,"varObj",0.265,"1")

#Obtenemos la matriz de correlacion:

# Generar el factos con las clases estimadas en test
pred_testt<-factor(ifelse(predict(modelo44,data_testt,type = "response")>0.265,1,0))

# Tablas marginales
table(pred_testt)
table(data_testt$varObj)

# Matriz de confusiÃ³n
confusionMatrix(pred_testt,data_testt$varObj, positive = '1')

#El modelo es muy malo tiene un P-Value de 0.365, debería ser mucho menor para segurarnos de que el modelo esta
#aportando algo. Los 1's no los distingue practicamente.

#Tenemos un kappa muy bajo también, que tomaria valor = 1 si no hubiese ni falsos positivos ni falsos negativos

#Probar con normalizar las variables cuantitativasd



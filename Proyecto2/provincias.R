#Empezamos cargando los datos y buscando la matriz de correlación de las variables:

rm(list=ls())
datos <- read_excel("C:/Users/aleja/OneDrive/Escritorio/Tarea/Provincias.xlsx")
datos <- as.data.frame(datos)
colnames(datos)
summary(datos)
str(datos)   #Todas las variables son numericas

dep_datos <- datos[,-1]   #Eliminamos la variable independiente con el nombre de las provincias
rownames <- datos[,1]

tabla1 <- stat.desc(dep_datos, basic = FALSE) #Genera un dataframe con las estadísitcas principales
knitr::kable(tabla1, digits=2, caption="Estadísticos descriptivos")  #No me gustan las tablas de knitr::kable

correlacion <- cor(dep_datos, method="pearson")
knitr::kable(correlacion, digits=2, caption="Relación de correlacion") #Aquí tenemos la matriz de correlaciones
corrplot(correlacion, type="upper", tl.col="black", tl.srt=45)  #Y aquí su representacion gráfica

#Vamos a intentar printear la matriz de correlacion de una forma un poco más bonita:
#Usamos la libreria formattable:

correlaciondf <- as.data.frame(correlacion)
formattable(correlaciondf)                    #Lo vemos en el viewer

#Podemos también colorearlo:

customGreen0 = "#DeF7E9" #Definimos algunos colores
customGreen = "#71CA97"
customRed = "#ff7f7f"

#Definimos una función para que nos coloree las cifras de rojo o verde dependiendo de si son positivas o negativas

improvement_formatter <- 
  formatter("span", 
            style = x ~ style(
              font.weight = "bold", 
              color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))))

formattable(correlaciondf, list(~ improvement_formatter))

#Podemos crear una funcion más compleja, definiendo diferentes colores para diferentes intervalos y así hacernos
#una idea visual de la intensidad de la correlación y de si esta es positia o negativa



colour_corr1 <- formatter("span",
      style = x ~ style(display = "block", font.weight = "bold",color = "black","border-radius" = "4px",
      "padding-right" = "4px",
      "background-color" = ifelse(x <= -0.8, "#A50026",
                           ifelse(x > -0.8 & x <= -0.6, "#D73027",
                           ifelse(x > -0.6 & x <= -0.4, "#F46D43",
                           ifelse(x > -0.4 & x <= -0.2, "#FDAE61",
                           ifelse(x > -0.2 & x <= 0, "#FEE08B",
                           ifelse(x > 0 & x <= 0.2, "#D9EF8B",
                           ifelse(x > 0.2 & x <= 0.4, "#A6D96A",
                           ifelse(x > 0.4 & x <= 0.6, "#66BD63",
                           ifelse(x > 0.6 & x <= 0.8, "#1A9850",
                           ifelse(x > 0.8, "#006837",NA))))))))))))

formattable(correlaciondf, list(~ colour_corr2))

#Estos viewers despues podemos convertirlos a jpeg
#A continuacion realizamos un análisis de componentes, calculando 7 principales:

formattable(tabla1)

#library("FactoMineR")

fit<-PCA(dep_datos ,scale.unit=TRUE,ncp=7,graph=TRUE)

#scale.unit: TRUE,indica que los datos serán estandarizados, pasan a tener
#media 0 y desviación típica 1. Es decir la matriz a diagonalizar es la matriz de correlaciones
#entre las variables. Es conveniente utilizarlo siempre. (calculo los autovalores de la matriz de correlaciones)

#Con ncp indicamos el numero de componentes principales que queremos calcular

#Tableamos los autovalores:

fit$eig
knitr::kable(fit$eig, digits =2,caption = "Autovalores", "simple")
options(scipen=999)
formattable(as.data.frame(fit$eig))

#El primer autovalor explica el 63.7% de la varianza de los datos
#Si nos quedamos con los 7 primeros autovalores estaríamos explicando el 98.74%
#Si un autovalor es menor a uno, el porcentaje de la varianza que explica sería menor que la de una sola variable
#En nuestro caso con cuatro autovalores estaríamos explicando ya más del 90% de la variabilidad de la informacion (92.19)
#Sin embargo este cuarto autovalor es menor que la unidad, por lo que lo mejor seria quedarnos con 3 componentes (87.01%)


#Pasamos a contruir los componentes principales:
#Cada autovalor tiene asociado un autovector
#Los autovectores asociados a los autovalores nos dan los coeficientes de los componentes principales

fit$svd$V
knitr::kable(fit$svd$V, col.names =c("autov1","autov2", "autov3","autov4", "autov5","autov6", "autov7"),
             digits =3,caption = "Autovectores")

#Para ver de forma grafica el porcentaje de variabilidad que explica cada autovalor:
#libr("factoextra)

fviz_eig(fit, geom="line")+ theme_grey()
fviz_eig(fit,addlabels=TRUE)

#A continuacion analizamos las variables en ese nuevo espacio formado por las componentes principales:
#Guardamos los estadísticos asociados a las variables en el objeto var

fit<-PCA(dep_datos,scale.unit=TRUE, ncp=3, graph=TRUE) #Nos quedamos con 3 componentes
var<-get_pca_var(fit)

#Mostramos los coeficientes para obtener los 3 componentes principales: (Apartado 3.a)

knitr::kable(fit$svd$V, col.names =c("autov1","autov2", "autov3"),
             digits =3,caption = "Autovectores", "simple")

formattable(as.data.frame(fit$svd$V), caption="Autovectores")

formattable(as.data.frame(fit$svd$V), 
  align = "c",
  list(~ formatter("span",
      style = x ~ formattable::style(display = "block",
                                    "border-radius" = "2px",
                                    "padding" = "5px",
                                    "text-align" = "center"))))




#Las componentes son la combinacion lineal de las variables estandarizadas multiplicadas por sus coeficientes correspondientes
#Vemos la relacion de correlacion entre las variables originales y las nuevas: (Apartado 3.b)

knitr::kable(var$cor, digits =3,caption = "Correlaciones de la CP con lasvariables", "simple")

options(digist=3)
formattable(as.data.frame(var$cor), caption="Correlaciones de la CP con lasvariabless")

formattable(as.data.frame(var$cor), align = "c", caption = "Correlaciones de la CP con lasvariables", list(~ colour_corr))

#La dim1 está mñas relacionada con los ocupados, la 2 con la natalidad y la 3 con CANE
#Gráficamente:

fviz_pca_var(fit, col.var="cos2",gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),repel = TRUE) #(para dos dimensiones)

#Los cosenos al cuadrado son las correlaciones al cuadrado que expresan la proporción de la
#varianza de cada variable que es explicada por cada componente:  (Apartado 3.d)

knitr::kable(var$cos2, digits =2,caption = "Cosenos al cuadrado", "simple") 
formattable(as.data.frame(var$cos2), caption="Cosenos al cuadrado", align="c")

#Vemos, por ejemplo, que la variación en la Población queda explicada en un 99% por la variable 1.
#La variable cuya varianza  está peor explicada por los nuevos componentes es VS, con solo un 44%
#Gráficamente:

corrplot(var$cos2,is.corr=FALSE)

#Porcentaje de variabilidad explicada por las nuevas componentes:

fviz_cos2(fit,choice="var",axes=1:2)

#La varianza de las componentes proviene de la varianza de las variables originales. Podemos ver la contribución que tiene cada
#variable para cada componente. Es decir, el porcentaje de la varianza de una componente que proviene de cada variable
#Apartado 3.e

knitr::kable(var$contrib, digits =2,caption = "Contribuciones de las variables", "simple")
formattable(as.data.frame(var$contrib), caption = "Contribuciones de las variables", align="c")

#El 8.62% de la varianza de la variable1 proviene de la poblacion, el 1.13% de la mortalidad...
#Las variables que más aportan a cada componente son: 1-Ocupados/ 2-Mortalidad/ 3-CANE
#Gráficamente:

corrplot(var$contrib,is.corr=FALSE)

#Podemos ver ahora, por ejemplo, el porcentaje de las 20 variables originales que más aportan a la variable1

fviz_contrib(fit, choice = "var", axes = 1, top = 20)

#Veamos ahora qué valores toman las instancias en las nuevas componentes:

ind<-get_pca_ind(fit)
knitr::kable(ind$coord, digits =3,caption = "Valores de los individuos en las Cp", "simple")
new_data <- as.data.frame(ind$coord) #Guardamos los datos convertidos en un nuevo df

#Puesto que las CP están construidas sobre las variables estandarizadas, los valores
#que toman las observaciones en las nuevas variables (CP) están alrededor de cero.
#Valores negativos indican observaciones que están por debajo de la media.
#Representamos ahora las instancias en los planos formados por las componentes:

fviz_pca_ind(fit, axes = c(1, 2), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)


fviz_pca_ind(fit, axes = c(1, 3), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)

fviz_pca_ind(fit, axes = c(2, 3), col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)



#Colores azules indican instancias que tienen valores cercanos a la media (cercanos a 0)
#podemos ahora representar simultaneamente las variables originales y las instancias en el nuevo plano:

fviz_pca_biplot(fit, repel = TRUE, col.var = "#2E9FDF", 
                col.ind = "#696969")                       

fviz_pca_biplot(fit, axes = c(1,3), repel = TRUE, col.var = "#2E9FDF", 
                col.ind = "#696969")


fviz_pca_biplot(fit, axes = c(2,3), repel = TRUE, col.var = "#2E9FDF", 
                col.ind = "#696969")

#Se ve como destacan por ejemplo Ceuta y Melilla (31 y 13), que son las ciudades donde la natalidad
#es mayor y con una gran diferencia (dimension(2))
#Madrid o Barcelona por ejemplo toman valores muy elevados para la dimensión 1, que sería entonces la que marque
#un poco el nivel de desarrollo economico por el que nos preguntan en el apartado anterior.
#Vemos como la dim1 tiene mucha correlacion con variables como PIB, OCUPADOS, POBLACION, NUMEMPRESAS, INDUSTRIA, CONSTRUCCION...


#-------------------------------
#Incluimos un índice que valore el desarrollo economico conjunto de las provincias:
#Tenemos que hacerlo siguiendo una combinacion lineal con elr esto de variables
#Cual sería este valor para Madrid? Y para Melilla?ç
#Las variables que más cercanas estén a la dimensión 1 que pesen más, coef cercano a 1
#la mortalidad que tenga un coeficiente negativo (que reste)
#Podemos coger los cosenos al cuadrado de la dim 1 y que esos sean los coeficientes, y el de la mortalidad lo restamos.
#-------------------------------

#Representamos un mapa de calor, estandarizado y sin estandarizar, para ver si detectamos grupos de provincias.
#Lo suyo es estandarizar los datos, para que las distancias entre las variables no influyan a la hora de determinar los grupos

#install.packages("heatmaply")
#install.packages("Cluster")
#install.packages("NbClust")

heatmaply(dep_datos, seriate = "mean", row_dend_left = TRUE, plot_method = "plotly")

#Serieate = "mean" ordena las variables y las instancias y las coloca de tal forma que las más parecidas aparezcan juntas
#Estandarizamos ahora la base de datos y volvemos a mapear:

dep_datos_st <- as.data.frame(scale(dep_datos))
heatmaply(dep_datos_st, seriate = "mean", row_dend_left = TRUE, plot_method = "plotly")

#Vemos como en el premier plot no apreciamos nada debido a que los datos no estaban estandarizados. La variable PIB toma valores
#mucho mayores que el resto, luego el resto de variables son representadas con el mismo color para todos los paises, que representa
#un valor mucho menor. Esto no nos permite observar diferencias entre paises

#Tras estandarizar podemos ver algo más. Observamos cómo madrid y barcelona toman valores parecdos para muchas variables, 
#más grandes en comparacón con el resto de provincias.
#Tambiñen observamos características diferenciadoras en provincias como Ceuta y Melilla o Murcia, Málaga y Sevilla. Esto tiene sentido
#, pues son provincias que comparten unas peculiaridades entre sí, y que las hacen diferentes al resto en muchos sentidos.

#Podemos destacar otras muchas carácteristicas, como la alta natalidad de Melilla en comparacion con el resto de provincias
#o la alta mortalidad de las provincias con índices 29,50,27,35,52,16,36,25,42,40,46,44

#Pasamos ahora a realizar un analisis jerarcico de clusters.

#Calculamos las distancias con los valores sin estandarizar

d <- dist(dep_datos_st, method = "euclidean") # distance matrix
#Mostramos las primeras seis filas dela matriz de distancias (las distancias de las 6 pimeras provincias)

d6<-as.matrix(d)[1:6, 1:6]
knitr::kable(d6, digits =2,caption = "Distancias", "simple")  

#Representamos gráficamente la matriz de distancias
fviz_dist(d, show_labels = TRUE)  ##Podríamos intentar que saliesen los nombres de las porvincias en lugar de los indices
#Reordenamos para agrupar las observaciones que están más próximas y visualizar los posibles clusters

ggheatmap(as.matrix(d), seriate="mean") #Obtenemos ademas en este plot el dendograma con las agrupacionesde paises

#Quizás recomendaríamos 3 grupos diferentes. Dentro de ese tercer grupo, otros dos diferentes. y dentro de ese segundo grupo
#otros dos. En total serían 5 grupos diferentes. (¿Justificacion? ----> visualmente son faciles de distinguir y 
#está bien con respecto al número de provincias que tenemos)

#Podemos visualizar los dendrogramas con las siguientes lineas de código
#Dependera del criterio que eliamos para definir la distancia entre los clusters
#Agrupamos las observaciones según el criterio de ward

res.hc_st <- hclust(d, method="ward.D2") 
fviz_dend(res.hc_st, cex = 0.5)  


#Continuamos con nuestra intuicion de hacer 5 grupos diferentes:

grp <- cutree(res.hc_st, k = 5)
head(grp, n = 4) 
knitr::kable(table(grp), caption = "Número de individuos por cluster", "simple") 

# Get the names for the members of cluster 1
rownames(dep_datos_st)[grp == 1]   #Deberiamos nuevamente intentar mostrar los nombres y nos los indices

#Mostramos el dendrograma con los clusters elegidos y el "nombre" de las provincias

fviz_dend(res.hc_st, k = 5, # Cut in five groups  (Apartado 5.b)
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#D73027"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE) # Add rectangle around groups

#Visualizamos los clusters en un plano formado por los dos primeros componentes principales

fviz_cluster(list(data = dep_datos_st, cluster = grp),
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#D73027"),
             ellipse.type = "convex", # Concentration ellipse
             repel = TRUE, # Avoid label overplotting (slow)
             show.clust.cent = FALSE, ggtheme = theme_minimal())  

#Introduciendo la opcion axes podemos cambiar los ejes

fviz_cluster(list(data = dep_datos_st, cluster = grp), axes=c(1,3), palette
             = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#D73027"), ellipse.type =
               "convex", repel = TRUE, show.clust.cent = FALSE, ggtheme =
               theme_minimal())

fviz_cluster(list(data = dep_datos_st, cluster = grp), axes=c(2,3), palette
             = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#D73027"), ellipse.type =
               "convex", repel = TRUE, show.clust.cent = FALSE, ggtheme =
               theme_minimal())


fviz_cluster(list(data = dep_datos_st, cluster = grp), axes=c(2,15), palette
             = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#D73027"), ellipse.type =
               "convex", repel = TRUE, show.clust.cent = FALSE, ggtheme =
               theme_minimal())   #No son componentes principales? ---->Tenemos que meter otra database?

#Podríamos probar qué pasa metiendo por ejemplo los datos generados a partir de la reducción de variables a la componentes principales

#Interesante son el primero y el tercero, son los que menos se solapan.
#A continuacion implementamos un algoritmo de clasificacion no jerárquico, en el que tenemos que definir previamente
#el numero de clusteres deseados. El numero optimo de clusteres podemos elegirlo en base a distintos criterios como los de Elbow
#o Silouhette

#Fiamos la misma semilla:

RNGkind(sample.kind ="Rejection")
set.seed(1234)

# Compute k-means
km.res <- kmeans(dep_datos_st, 5)      #Decidimos agrupar en 5 clusteers diferentes
head(km.res$cluster, 20)   #Vemos a qué claster pertenecen los primeros 20 elementos

# Visualize clusters using factoextra
fviz_cluster(km.res, dep_datos_st)

#Empleamos ahora el criterio Elbow para elegir el número de Clusters:

fviz_nbclust(dep_datos_st, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")

#El numero de clusters deseado sería aquel en el que la variabilidad total intra-clusters ya no se reduce mucho al
#aumentar uno mas. Parece que 5 clusters es un número razonable.

#Probamos ahora con el criterio de Silouhette:

fviz_nbclust(dep_datos_st, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

#Dos es un numero muy pequeño de clusters, es más razonable el máximo local que aparece en 5, que es lo que habíamos predicho.
#Visualizamos los dos componentes principales que nos faltaban:

fviz_cluster(km.res, dep_datos_st, axes = c(2,3))
fviz_cluster(km.res, dep_datos_st, axes = c(1,3))
fviz_cluster(km.res, dep_datos_st, axes = c(1,18)) #No son componentes principales? ---->Tenemos que meter otra database?

#Calculamos ahora en indice de Silhouette para medir la calidad de los clusters. Este indice mide como de separados y compactos son
#los clusters. r. Si la silueta media es superior a 0.5 se produce una partición de los datos razonable mientras que un valor por
#debajo de 0.2 indica que los datos no exhiben ninguna estructura de cluster. 

#Evaluación de la calidad de los clusters
sil <- silhouette(km.res$cluster, dist(dep_datos_st))   #Podemos intentar hacer el analisis para otro num de clusters
rownames(sil) <- rownames(dep_datos_st)                 #0.25 no es ua gran cifra

fviz_silhouette(sil)

#Si la silueta esta próxima a 1 eso querría
#indicar que la observación se encuentra bien agrupada, mientras que si vale 0 indica
#que la observación podría pertenecer a su cluster actual o a otro cercano a él. Si la
#silueta es negativa indicaría una mala agrupación para la observación. 

#En este momento los cluesters están en el mismo espacio de los datos originales
#No se ha reducido la dimension a componentes principales puesto que utilizamos dep_datos_st
#Habria que impementar directamente la base de datos con la dimension reducida?

#Caracterizamos los clusters:

ordenado<-sort(km.res$cluster)
knitr::kable(ordenado, digits =2, caption = "Provincia y cluster", "simple") #No sale el nombre de las provincias

knitr::kable(km.res$centers, digits =2,caption = "Estadísticos de los clusters, datos STD", "simple") 

#Recordemos que tenemos los datos estandarizados
#Para poder caracterizar mejor las diferencias entre clusters es más útil pobtener los
#estadísticos de resumen de las variables originales por cluster.

EsT_Clus<-aggregate(dep_datos, by=list(km.res$cluster),mean)
knitr::kable(EsT_Clus, digits =2,caption = "Estadísticos de los clusters", "simple") 

#Quizás mostrar tablas mas bonitas, como ya habiamos hecho antes

#Las representaciones gráficas de las variables para cada cluster en boxplot facilitan la
#comparación entre clusters.

dep_datos$grupo<- as.factor(km.res$cluster)
g1<- ggplot(dep_datos, aes(x=grupo, y=Poblacion, fill=grupo)) + geom_boxplot()
g2<- ggplot(dep_datos, aes(x=grupo, y=Mortalidad, fill=grupo)) + geom_boxplot()
g3<- ggplot(dep_datos, aes(x=grupo, y=Natalidad, fill=grupo)) + geom_boxplot()
g4<- ggplot(dep_datos, aes(x=grupo, y=IPC, fill=grupo)) + geom_boxplot()
g5<- ggplot(dep_datos, aes(x=grupo, y=NumEmpresas, fill=grupo)) + geom_boxplot()
g6<- ggplot(dep_datos, aes(x=grupo, y=Industria, fill=grupo)) + geom_boxplot()
g7<- ggplot(dep_datos, aes(x=grupo, y=Construccion, fill=grupo)) + geom_boxplot()
g8<- ggplot(dep_datos, aes(x=grupo, y=CTH, fill=grupo)) + geom_boxplot()
gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, ncol=2, nrow=4)

#Podemos plotear todas las variables t empezar a realizar el analisis socieconomico de nuestros clusters
#Hacemos ahora el análisis para los datos reducidos a las tres componentes principales:


d2 <- dist(new_data, method = "euclidean")
res.hc_st2 <- hclust(d2, method="ward.D2") 
fviz_dend(res.hc_st2, cex = 0.5)  

#A ver si el criterio de Elbow nos sigue recomendando 5 clusteres:

fviz_nbclust(new_data, kmeans, method = "wss") +
  geom_vline(xintercept = 5, linetype = 2)+
  labs(subtitle = "Elbow method")

#Sí, nos sigue recomendando 5

grp2 <- cutree(res.hc_st2, k = 5)

fviz_cluster(list(data = new_data, cluster = grp2), axes=c(1,2), palette
             = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#D73027"), ellipse.type =
               "convex", repel = TRUE, show.clust.cent = FALSE, ggtheme =
               theme_minimal())

fviz_cluster(list(data = new_data, cluster = grp2), axes=c(1,3), palette
             = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#D73027"), ellipse.type =
               "convex", repel = TRUE, show.clust.cent = FALSE, ggtheme =
               theme_minimal())


fviz_cluster(list(data = new_data, cluster = grp2), axes=c(2,3), palette
             = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#D73027"), ellipse.type =
               "convex", repel = TRUE, show.clust.cent = FALSE, ggtheme =
               theme_minimal())

km.res <- kmeans(new_data, 5)

sil <- silhouette(km.res$cluster, dist(new_data))   #Podemos intentar hacer el analisis para otro num de clusters
rownames(sil) <- rownames(new_data)                 #0.25 no es ua gran cifra

fviz_silhouette(sil)
print(sil)
print(mean(sil[,"sil_width"]))

new_data$grupo<- as.factor(km.res$cluster)
g1<- ggplot(new_data, aes(x=grupo, y=Dim.1, fill=grupo)) + geom_boxplot()
g2<- ggplot(new_data, aes(x=grupo, y=Dim.2, fill=grupo)) + geom_boxplot()
g3<- ggplot(new_data, aes(x=grupo, y=Dim.3, fill=grupo)) + geom_boxplot()
gridExtra::grid.arrange(g1, g2, g3, ncol=3, nrow=1)

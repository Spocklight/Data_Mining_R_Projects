rm(list=ls())

library(kableExtra)
library(dplyr)
library(data.table)
library(DT)
library(rmdformats)
library(dtplyr)         #Traduce el cód.de dplyr a datatable (mantenemos la legilibilidad y mejoramos velocidad)
library(microbenchmark)
library(janitor)
library(plotly)

### VisualizaciÃ³n de los datos en bruto
#Leemos nuestros datos del set de training con readRDS
# (Leemos los datos desde formato binario comprimido RDS)


datos <- datosMedia <- readRDS('C:/Users/aleja/OneDrive/Escritorio/R_Data_Mining/FugaClientes_Training.RDS')

#tres formas de leer los datos por consola:

head(data.frame(datos)) 

tibble(datos)

data.table(datos)

datatable(head(datos,1000), options = list(autoWidth = TRUE, scrollX = TRUE),
          caption = 'Datos Uso de Medios',
          class = 'cell-border stripe nowrap')

#No hay referencia al tiempo en nuestros datos (información estática/caracterización)

# Podemos pedir un cortecito por el valor más alto de una columna:

datos %>% slice_max(Antig.fc.edad)

# Summary para ver un poco la distribuciÃ³n de las variables
summary(datosMedia$FacturaMes)

#Utilizamos dtplyr para gacer el slice_min (que tarda mucho en dplyr)

lazy_dt(datos) %>% slice_min(Antig.fc.edad) %>% show_query()

#Aprovecho el output anterior para escribir mi codigo en datatable '_DT1' es mi lazi_dt(datos)

data.table(datos)[, .SD[order(Antig.fc.edad)][frankv(Antig.fc.edad, ties.method = "min", 
                                                 na.last = "keep") <= 1L]]

# Podemos comparar los tiempos con un microbenchmark

prueba <- microbenchmark::microbenchmark(
  
  dplyr = datos %>% slice_min(Antig.fc.edad),
  data.table = data.table(datos)[, .SD[order(Antig.fc.edad)][frankv(Antig.fc.edad, ties.method = "min",
                                                                na.last = "keep") <= 1L]],
  times = 3
)

ggplot2::autoplot(prueba) #Pues tarda menos dplyr XD

#Preguntas típicas que hay que hacerse: ¿Hay duplicados?

datos_distintos <- datos %>% distinct() #deja solo las filas que son distintas

#En este caso todas nuestras filas son diferentes

#Vamos ahora a agrupar los datos según el método de pago, el numero de cada método y lo guardamos en una 
#columna "total", lo ponemos en orden descendente

datos %>% group_by(MetodoPago) %>% summarise(total=n()) %>% arrange(desc(total)) %>% 
ungroup()->datosMedia_distinct

#De la gente que se ha fugado, agrúpalos por el metodo que han usado para pagar y cuéntalos
#Los que mas han dejado de pagar son los que pagan con cheque electrónico

datos %>% filter(Fuga == 1) %>% group_by(MetodoPago) %>% count()

#Si quiero estudiar una variable continua como potencial respuesta a predecir:

datos %>% plot_ly(x= ~FacturaMes) %>% add_histogram()
datos %>% plot_ly(x= ~FacturaMes) %>% add_boxplot()

datos %>% filter(FacturaMes <80) %>% plot_ly(x= ~FacturaMes) %>% add_histogram() #Filtrando para un valor
datos %>% filter(FacturaMes <580) %>% plot_ly(x= ~FacturaMes) %>% add_boxplot()



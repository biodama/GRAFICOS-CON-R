##############################################################################
##############################################################################
# Bloque 1 - Día 1 (06/10/2026)
##############################################################################
##############################################################################
#install.packages("ggplot2")

library(ggplot2)

head(mpg)
str(mpg)

ggplot(data=mpg, aes(x = displ, y = hwy, colour = class)) +
  geom_point()

ggplot(data=mpg)

ggplot(data=mpg,aes(x = displ, y = hwy, colour = class))+
  geom_point()


#### Introducción a R ####
datos <- data.frame(
  id = 1:6,
  sexo = c("M","F","F","M","F","M"),
  edad = c(34, 52, 47, 61, 39, 70),
  grupo = c("A","A","B","B","A","B")
)

str(datos)
head(datos)
summary(datos)

#install.packages("data.table")
library(data.table)
dt<-data.table(datos)

class(dt)
dt

mpg2<-data.table(mpg)

mpg
mpg2

dim(datos)
names(datos)
str(datos)
head(datos)
tail(datos)
summary(datos)
table(datos$sexo)
table(datos$grupo)

rm(list=ls())

load("/Users/celiatalavan/Desktop/GRAFICOS-CON-R-main/data/datos.curso1.RData")


names(datos)
str(datos)
head(datos)
tail(datos)
summary(datos)

datos[datos$edad >= 50, ]
datos[datos$sexo == "Mujer", ]
datos[datos$sexo == "Mujer" & datos$edad >= 50, ]

datos[datos$estado.civil%in% c("Casado", "Divorciado") & datos$edad < 60, ]


indice.registros<- c(1,15,28)
datos[indice.registros,]
datos$"peso"[indice.registros]<- 44.33

table(datos$"estado.civil")
datos$"estado.civil.new" <- datos$"estado.civil"
datos$"estado.civil.new"[datos$"estado.civil.new"%in%"Casado"]<-"cas"
table(datos$"estado.civil.new")

range(datos$"edad")
summary(datos)

gr<-cut(x=datos$"edad",breaks=seq(0,100,20),right=F,include.lowest=T)
datos$"gr.edad"<-gr
class(datos$"gr.edad")
levels(datos$"gr.edad")

table(datos$"gr.edad",exclude=NULL)

datos$"nivel.estudios2"<- factor(datos$nivel.estudios, levels = c("Alto", "Bajo", "Medio"),
                                 labels = c("Doctorado", "ESO", "Grado"))
table(datos$nivel.estudios2)
levels(datos$nivel.estudios2)

table(datos$"estado.civil")
estado.civil.factor <- as.factor(datos$estado.civil)
table(estado.civil.factor)

estado.civil.numerico<-as.numeric(estado.civil.factor)
table(estado.civil.numerico)


estado.civil.factor <- as.factor(datos$estado.civil)
levels(estado.civil.factor)

nuevo.orden<- c("Divorciado", "Soltero", "Casado")
estado.civil.factor.new<- factor(estado.civil.factor,levels=nuevo.orden)
estado.civil.factor.new<- factor(estado.civil.factor,levels=c("Divorciado", "Soltero", "Casado"))

levels(estado.civil.factor.new)


datos$"nivel.estudios2"<- factor(datos$nivel.estudios, levels = c("Alto", "Medio","Bajo"),
                                 labels = c("Doctorado", "Grado", "Eso"))

levels(datos$"nivel.estudios2")



datos$gr.edad2<- factor(datos$gr.edad,
                        levels = c("[0,20)", "[20,40)", "[40,60)", "[60,80)", "[80,100]"),
                        labels = c("1a etapa", "2a etapa", "3a etapa", "4a etapa", "5a etapa"))

datos$gr.edad.new<-factor(datos$gr.edad, levels=c("[0,20)"  ,"[20,40)","[40,60)","[60,80)","[80,100]"),
                          labels = c("1º Etapa","2º Etapa","3º Etapa","4º Etapa","5º Etapa"))



cancer <- data.frame( id = 1:8,
                      sexo = c("F","M","F","F","M","M","F","M"),
                      edad = c(52,67,61,45,73,58,69,64),
                      tipo_cancer = c("Mama","Pulmón","Colon","Mama","Próstata","Pulmón","Colon","Próstata"),
                      casos_2019 = c(1,0,1,1,1,0,1,1),
                      casos_2020 = c(1,1,1,1,1,1,1,1),
                      casos_2021 = c(1,1,1,1,1,1,1,1), casos_2022 = c(1,1,1,1,1,1,1,1))

cancer


library(reshape2)

cancer_long <- melt(
  cancer,
  id.vars = c("id","sexo","edad","tipo_cancer")
  ,variable.name = "año",value.name = "casos")

cancer_long$año<-as.numeric(gsub("casos_","",cancer_long$año))
#cancer_long$año<-as.numeric(cancer_long$año)

cancer_wide <- dcast(cancer_long, id + sexo + edad + tipo_cancer ~ año,
                      value.var = "casos" )
cancer_wide

###################################################################
###################################################################
# Bloque 2 
###################################################################
###################################################################


# Borrado del entorno de trabajo

rm(list=ls())
gc()

require("scales")       # formateo de ejes (%, €, fechas...)
require("RColorBrewer") # paletas de color
require("viridis")      # paletas perceptualmente uniformes
require("ggthemes")     # temas extra (Economist, FiveThirtyEight, Tufte...)
require("patchwork")    # combinar varios gráficos en uno


?ggplot

setwd("/Users/celiatalavan/Desktop/graficos R/")
load("data/datos.cancer.RData")
load("data/datos.tabla.des.RData")
head(cancer)
head(datos)

g <- ggplot(cancer)
g

ggplot(cancer, aes(x = periodo, y = tasa)) +
  geom_point()


ggplot(cancer, aes(x = tasa, y = periodo)) +
  geom_point()



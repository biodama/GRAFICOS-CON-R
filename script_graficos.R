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

library("ggplot2")
g <- ggplot(cancer)
g

ggplot(cancer, aes(x = periodo, y = tasa)) +
  geom_point()


ggplot(cancer, aes(x = tasa, y = periodo)) +
  geom_point()

table(cancer$tumor)

cancer2<-cancer[cancer$tumor%in%c("MAMA","PROSTATA","COLORRECTAL"),]

ggplot(data=cancer2, aes(x = periodo, y = tasa, colour = tumor)) +
  geom_point()

ggplot(cancer2, aes(x = periodo, y = tasa, colour = tumor,shape=sexo)) +
  geom_point()

ggplot(cancer2, aes(x = periodo, y = tasa, colour = tasa, shape=sexo)) +
  geom_point()


head(datos)

###### Ejercicio 2.2 ########

ggplot(data=datos,aes(x = altura, y = peso, colour = sexo)) +
  geom_point()

ggplot(data=datos,aes(x = peso, y = altura, colour = sexo)) +
  geom_point()

ggplot(cancer2, aes(x = tumor, y = tasa, colour = sexo)) +
  geom_boxplot()

ggplot(cancer2, aes(x = tumor, y = tasa)) +
  geom_boxplot(colour = "steelblue", fill = "white")

##### Ejercicio 2.3 ########

ggplot(cancer2, aes(x = tumor, y = tasa, fill = tumor)) +
  geom_boxplot()

ggplot(cancer2, aes(x = tumor, y = tasa, colour =  tumor)) +
  geom_boxplot()

###### Ejercicio 2.4 #######
cancer4<-cancer[cancer$tumor%in%c("HIGADO","ESOFAGO","ESTOMAGO") & cancer$sexo%in%("Mujeres"),]
table(cancer4$sexo)

ggplot(cancer4, aes(x = tumor, y = tasa, fill = tumor)) +
  geom_boxplot()

ggplot(cancer4, aes(x = tumor, y = tasa, colour = tumor)) +
  geom_boxplot()

cancer3 <- cancer[cancer$sexo == "Mujeres" & cancer$tumor %in% c("HUESOS", "PULMON", "LARINGE"),]

ggplot(cancer3, aes(x = tumor, y = tasa, fill = tumor)) +
  geom_boxplot()


mirar <- cancer[cancer$tumor %in% c("MAMA","PROSTATA"),]
table(mirar$tumor)

ggplot(mirar, aes(x = periodo, y = tasa, linetype = sexo, colour = tumor)) +
  geom_line(linewidth = 1)


mirar <- cancer[(cancer$tumor == "MAMA" & cancer$sexo == "Mujeres") |
                  (cancer$tumor == "PROSTATA" & cancer$sexo == "Hombres"),]

ggplot(mirar, aes(x = periodo, y = tasa, linetype = sexo, colour = tumor)) +geom_line(linewidth = 1)


######## Ejercicio 2.5 ######

ggplot(mirar,aes(x=periodo, y=tasa, shape=sexo,colour=sexo))+geom_point()


ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor)) +
  geom_point(alpha = 1, size = 2,shape=17)


########## Ejercicio 2.6 #########

ggplot(mirar, aes(x=tumor, y = tasa))+
  geom_point(alpha = 0.2)

ggplot(mirar, aes(x=tumor, y = tasa))+
  geom_point(alpha = 1)

ggplot(cancer2, aes(x = tumor, y = tasa, colour = tumor)) +
  geom_point(alpha = 0.2, size = 3, shape="circle")

ggplot(cancer2, aes(x = tumor, y = tasa, colour = tumor)) +
  geom_point(alpha = 1, size = 1, shape="circle")


ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor, shape = sexo)) +
  geom_point(size = 3,alpha=0.75)

########## Ejercicio 2.7 #########
ggplot(cancer2, aes(x = periodo, y = tasa, colour = tumor, shape = sexo, alpha = tumor, size=tumor)) +
  geom_point()

ggplot(cancer2, aes(x = periodo, y = tasa, colour = tumor, shape = sexo, size = tasa,alpha=sexo)) +
  geom_point()


ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor)) +
  geom_line()


ggplot(mirar) +
  geom_line(aes(x = periodo, y = tasa, colour = tumor))+
  geom_point(data=mirar[mirar$tasa>=22,],aes(x = periodo, y = tasa))


ggplot(mirar,aes(x = periodo, y = tasa,colour = tumor)) +
  geom_line()+
  geom_point()

ggplot(cancer2, aes(x = periodo, y = tasa)) +
  geom_line(aes(colour = tumor)) +     # el color solo se aplica a la línea
  geom_point(colour = "black", size = 1)

ggplot(cancer2, aes(x = periodo, y = tasa, group =interaction(tumor,sexo),colour = tumor)) +
  geom_point(aes(shape=sexo),color="black") + geom_line()


ggplot(mirar, aes(x = periodo, y = tasa, group = tumor)) +
  geom_line() +      # el color solo se aplica a la línea
  geom_point(colour = "black", size = 1) 


###### Ejercicio 3.1######
pulmon <- cancer[(cancer$tumor == "PULMON" & cancer$sexo == "Hombres"),]

ggplot(pulmon, aes(x = periodo, y = tasa)) +
  geom_line(linewidth=1.5, color= "lightblue") + geom_point(color="navy", size=2)


ggplot(pulmon, aes(x = periodo, y = tasa, group =interaction(tumor,sexo), shape=sexo)) +
  geom_line(aes(colour = tumor)) +      
  geom_point(colour = "black", size = 1)

library(data.table)
ggplot(cancer[tumor%in%"PULMON" & sexo%in%"Hombres"], aes(x = periodo, y = tasa)) +
  geom_point(size = 2,color="orange")+geom_line(color="steelblue")




ggplot(mirar, aes(x = tasa)) +
  geom_histogram(bins = 20, fill = "steelblue", colour = "white")

ggplot(mirar, aes(x = tasa, fill = sexo)) +
  geom_density(alpha = 0.4)

##### Ejercicio 3.3 ############

ggplot(mirar, aes(x = tasa)) +
  geom_histogram(bins = 5, fill = "steelblue", colour = "white")

ggplot(mirar, aes(x = tasa)) +
  geom_histogram(bins = 50, fill = "steelblue", colour = "white")


##### Ejercicio 3.4 ######
ggplot(mirar, aes(x = tasa, fill = sexo)) +
  geom_density(alpha = 0.4)

ggplot(mirar, aes(x = tasa, fill = sexo)) +
  geom_histogram(position="identity", alpha = 0.4)

ggplot(mirar, aes(x = tumor, y = tasa, fill = sexo)) +
  geom_boxplot()

ggplot(mirar, aes(x = tumor, y = tasa, fill = sexo)) +
  geom_violin()

##### Ejercicio 3.5 ######

ggplot(mirar, aes(x = tumor, y = tasa, fill = sexo)) +
 geom_boxplot(width = 0.1, fill = "white")+ geom_violin()


ggplot(cancer2, aes(x = tumor, y = tasa, colour = sexo)) +
  geom_point(width = 0.2, alpha = 0.6)

ggplot(cancer2, aes(x = tumor, y = tasa, colour = sexo)) +
  geom_jitter(width = 0.2, alpha = 0.6)



ggplot(mirar, aes(x = periodo, y = tasa, color = tumor, linetype = sexo)) +
  geom_line() 


ggplot(mirar, aes(x = periodo, y = tasa, color = tumor, linetype = sexo)) +
  geom_smooth(se = T, method = "loess") 

ggplot(mirar, aes(x = periodo, y = tasa, color = tumor, linetype = sexo)) +
  geom_smooth(se = TRUE, method = "lm")

##### Ejercicio 3.6 #####


cancer_pulmon<-cancer[cancer$tumor%in%c("PULMON"),] #data.frame
cancer_pulmon<-cancer[tumor%in%c("PULMON")] #data.table

ggplot(cancer_pulmon, aes(x = periodo, y = tasa, color = sexo)) +
  geom_point() +
  geom_smooth(se = T, method = "lm")   +
  geom_smooth(method="loess", colour="red")


ggplot(cancer[tumor%in%"PULMON" & sexo%in%"Hombres"], aes(x = periodo, y = tasa)) +
  geom_smooth(color="steelblue", method = "lm") +geom_point()+geom_smooth(method="loess", colour="red")


ggplot(cancer2, aes(x = periodo, y = tasa, fill = tumor)) +
  geom_col(position = "dodge")

p1 <- ggplot(cancer2[cancer2$periodo%in%c(2010:2015)], aes(x = periodo, y = tasa, fill = tumor)) +
  geom_col(position = "stack") 

p2 <- ggplot(cancer2[cancer2$periodo%in%c(2010:2015)], aes(x = periodo, y = tasa, fill = tumor)) +
  geom_col(position = "dodge")

p1 | p2



ggplot(cancer2, aes(x = periodo, y = tasa,colour=sexo)) +
  geom_line() +
  facet_wrap(~tumor)

ggplot(mirar, aes(x = periodo, y = tasa)) +
  geom_line() +
  facet_wrap(~tumor, ncol = 1) 


ggplot(cancer2, aes(x = periodo, y = tasa)) +
  geom_smooth(se = FALSE) +
  facet_grid(tumor ~ sexo)


##### Ejercicio 5.1####

ggplot(cancer[cancer$tumor=="COLORRECTAL" & cancer$periodo%in%c(1990:2012)] ,
       aes(x = periodo, y = tasa, color = sexo))+ geom_smooth()  

ggplot(cancer[cancer$tumor=="COLORRECTAL" & cancer$periodo%in%c(1990:2012)] ,
       aes(x = periodo, y = tasa))+ geom_smooth() + 
       facet_wrap(~sexo)

ggplot(cancer[cancer$tumor=="COLORRECTAL" & cancer$periodo%in%c(1990:2012)] ,
       aes(x = periodo, y = tasa))+ geom_smooth() + 
    facet_wrap(~sexo,scales = "free_y")+labs(x = "Periodo", y = "Tasa de mortalidad (defunciones/100000 hab)",
                                             ,title = "Tasa de mortalidad de Colorrectal",
                                             caption = "CNE")


ggplot(mirar[mirar$sexo == "Hombres",], aes(x = periodo, y = tasa, colour = tumor)) +
  geom_line() + annotate("text", x = 1982, y = 22.5, label = "Pico observado", colour = "black") +
  annotate("segment", x = 1984, xend = 1982,y = 21.5, yend = 22, 
           colour = "red", arrow = arrow(length = unit(0.2,"cm")))

ggplot(mirar[mirar$sexo == "Hombres",], aes(x = periodo, y = tasa,colour = tumor)) +
  geom_line() + geom_point(colour = "black", size = 1)+
  geom_text(aes(label = round(tasa,1)),colour = "black",vjust = -0.7,size = 2)

library("ggrepel")

ggplot(mirar[mirar$sexo == "Hombres",], aes(x = periodo, y = tasa,colour = tumor)) +
  geom_line() + geom_point(colour = "black", size = 1) +
  geom_label_repel(aes(label = round(tasa,1)),colour = "black",size = 2.5)

ggplot(mirar[mirar$sexo == "Hombres",], aes(x = periodo, y = tasa,colour = tumor)) +
  geom_line() + geom_point(colour = "black", size = 1) +
  geom_label_repel(aes(label = round(tasa,1)),colour = "black",size = 2.5)



ggplot(mirar, aes(x = periodo, y = tasa, color = tumor, group = tumor)) +
  geom_line() + scale_y_log10() +
  scale_color_manual(values = c("#ABCADF","#7D0112"))

ggplot(mirar, aes(x = periodo, y = tasa, color = tumor, group = tumor)) +
  geom_line() +  scale_color_manual(values = c("#ABCADF","#7D0112"))+
  scale_y_continuous(breaks = seq(10,30,5), limits = c(0,40))

ggplot(cancer2, aes(x = tumor, y = tasa, fill = tumor)) +
  geom_boxplot() + scale_fill_brewer(palette = "Greens",direction = 1)


ggplot(cancer2, aes(x = periodo, y = tasa, colour = tasa)) +
  geom_point(size = 2) + scale_colour_viridis_c(option = "inferno")

ggplot(cancer2, aes(x = periodo, y = tasa/100, fill = tumor)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent)

### Ejercicio 7.1 ###

p3<-ggplot(cancer2, aes(x = periodo, y = tasa, colour = tasa)) +
  geom_point(size = 2) + scale_colour_viridis_c(option = "inferno")



ggplot(cancer, aes(x = periodo, y = tasa, colour = tumor)) +
  geom_line()+
  geom_point() + scale_colour_viridis_d(option = "inferno")


ggplot(cancer2, aes(x = tumor, y = tasa, fill = tumor)) +
  geom_boxplot() +
  coord_flip()

ggplot(cancer2[cancer2$periodo == max(cancer$periodo),],
       aes(x = "", y = tasa, fill = tumor)) +
  geom_col(position = "fill") +
  coord_polar(theta = "y")

g <- ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor)) + geom_line()
g
g + theme_bw()


g+labs(title="Evolución temporal")+ theme_minimal() +theme(legend.position = "bottom",
                           axis.title = element_text(face = "bold"),
                           plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                           panel.grid.minor = element_blank())

g+labs(title="Evolución temporal")+theme(legend.position = "bottom",
                                      axis.title = element_text(face = "bold"),
                                      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
                                     panel.grid.minor = element_blank())

#### Ejercicio 9.1 ####

g <- ggplot(cancer2, aes(x = periodo, y = tasa, colour = tumor, linetype = sexo)) + geom_line()
g<-g +labs(title="Periodo-Tasa de cada tipo de Cancer por Sexo", subtitle="Hecho en 2026")
 
  mi_tema<-theme_minimal() +theme(legend.position = "right",
        plot.title = element_text(color = "red"),
        axis.title = element_text(color = "#7D0112", face = "italic"),
        axis.text.x = element_text(hjust = 1, face = "bold", angle = 90),
        axis.text = element_text(angle = 45, face = "bold"))
  

g+mi_tema



p1 <- ggplot(cancer2, aes(x = tumor, y = tasa)) + geom_boxplot() + coord_flip()+mi_tema

p2 <- ggplot(mirar, aes(x = tasa)) + geom_histogram(bins = 20)+mi_tema

(p1 | p2) +
  plot_annotation(title = "Distribución de la tasa de mortalidad",tag_levels = "A")


p3 / (p2 | p1)


cancer2$tumor <- factor(cancer2$tumor,levels = c("MAMA", "PROSTATA","COLORRECTAL"))

p<-ggplot(cancer2,aes(x = periodo,y = tasa,shape=sexo,colour = tumor)) +
  geom_line() +geom_point(size = 1)

p


### Ejercicio Final ####

ggplot(cancer, aes(x = periodo, y = tasa, colour = tumor, linetype = sexo)) + 
  geom_line() + 
  geom_point() + 
  facet_wrap(~sexo) + 
  scale_colour_viridis_d(option = "viridis") +
  labs(title="Periodo-Tasa de cada tipo de Cancer por Sexo", subtitle="Hecho en 2026")+
  theme(legend.position = "bottom",
        plot.title = element_text(color = "black"),
        axis.title = element_text(color = "black", face = "italic"),
        axis.text.x = element_text(hjust = 1, face = "bold", angle = 90),
        axis.text = element_text(angle = 45, face = "bold"))


  ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor)) +
  geom_point() + scale_color_viridis_d(option = "mako") + facet_wrap(~sexo)+
  labs(title = "Último ejercicio", subtitle = "Hecho en 2026") + theme_minimal() +
  theme(legend.position = "bottom")

  ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor)) +
    geom_line() + geom_point(colour = "black", size = 1) +
    facet_wrap(~sexo)  +
    scale_color_viridis_d(option = "A")+
    labs(title="Grafico Mama y Prostata", subtitle="Hecho en 2026", x = "Periodo",  y = "Tasa" ) +
    theme_minimal() + theme(legend.position = "bottom")

  # p1: Boxplot de tasa por tumor
  p1<-ggplot(cancer2, aes(x = tumor, y = tasa,color=sexo))+
    geom_boxplot()+
    coord_flip()+
    labs(
      title = "Distribuciónnpor tumor",
      x = "Tumor",
      y = "Tasa",color="Sexo"
    ) +
    theme_minimal()
  
  # p2: Histograma de tasa
  p2 <- ggplot(cancer2, aes(x = periodo, y = tasa,color=sexo)) +
    geom_smooth(color="steelblue", method = "lm") +geom_point()+geom_smooth(method="loess", colour="red")+
    labs(
      title = "Tendencia de la tasa por sexo",
      x = "Tasa",
      y = "Densidad",
      color = "Sexo"
    ) +
    theme_minimal()
  
  # p3: Densidad de tasa por sexo
  p3 <- ggplot(cancer2, aes(x = tasa, fill = sexo, color = sexo)) +
    geom_density(alpha = 0.3) +
    labs(
      title = "Densidad de la tasa por sexo",
      x = "Tasa",
      y = "Densidad",
      fill = "Sexo",
      color = "Sexo"
    ) +
    theme_minimal()
  
  p2 / (p1 | p3)
  

##################################################################
##################################################################  
##################################################################  

rm(list=ls())
gc()

setwd("/Users/pfernandezn/Desktop/CURSO_GRAFICOS_R_FI_2026/") 

####### MAPAS #############
  
load("data/peninsula.RData")  
  
ls()

class(peninsula)

library(sf)

library(ggplot2)

ggplot(peninsula) +
  geom_sf(aes(fill = NAME_1), alpha = 0.1, col = "grey80", show.legend = FALSE)

ggplot(peninsula) +
  geom_sf(aes(fill = NAME_1), show.legend = TRUE)

ggplot(peninsula) +
  geom_sf(aes(fill = NAME_1), alpha = 0.1, col = "grey80", show.legend = FALSE)+
  geom_sf_text(aes(label = NAME_2), size = 2) 

ggplot(peninsula) +
  geom_sf(aes(fill = NAME_1), alpha = 0.1, col = "grey80", show.legend = FALSE)+
  geom_sf_text(aes(label = NAME_2), size = 2) +
  theme_bw()

head(st_drop_geometry(peninsula))

library(data.table)

paro <- fread("data/paro.csv", encoding = "UTF-8")
paro[, `:=`(id, sub(" ", "0", format(Prov.id, width = 2)))]
Paro <- subset(paro, Año == 2012 & Trimestre == "I")

peninsula$"id" <- peninsula$"CC_2"

peninsula.paro <- merge(peninsula, Paro, by = "id")

head(st_drop_geometry(peninsula.paro))

table(peninsula.paro$"Sexo")

mapa <- ggplot(peninsula.paro) +
  geom_sf(aes(fill = Tasa.paro), colour = "grey80", size = 0.1)+
  facet_grid(~Sexo)+
  scale_fill_gradient("Tasa de paro", low = "aliceblue", high = "steelblue4") +
  theme_bw()
mapa


mapa <- ggplot(peninsula.paro) +
  geom_sf(aes(fill = Tasa.paro), colour = "grey80", size = 0.1)+
  geom_sf_text(aes(label = NAME_2), size = 1)+
  facet_grid(~Sexo)+
  scale_fill_gradient("Tasa de paro", low = "aliceblue", high = "steelblue4") +
  theme_bw()
mapa


# Cambio de paleta

Paro2 <- paro[Año%in%2014 & Trimestre%in%"III",]
peninsula.paro2 <- merge(peninsula, Paro2, by.x = "id", by.y = "id")

ggplot(peninsula.paro2) +
  geom_sf(aes(fill = Tasa.paro), colour = "grey80", size = 0.1) +
  facet_grid(~Sexo) +
  scale_fill_viridis_c("Tasa de paro", option = "plasma") +
  labs(title = "Tasa de paro por provincia — 2014 T3") +
  theme_minimal()

ggplot(peninsula.paro2) +
  geom_sf(aes(fill = Tasa.paro), colour = "grey80", size = 0.1) +
  geom_sf_text(aes(label = NAME_2), size = 1)+
  facet_grid(~Sexo) +
  scale_fill_viridis_c("Tasa de paro", option = "plasma") +
  labs(title = "Tasa de paro por provincia — 2014 T3") +
  theme_minimal()

# Cosas adicionales

library(sf)

canarias <- st_read("data/municipios_canarias/ll_municipales_inspire_canarias_wgs84.shp")
class(canarias)
head(st_drop_geometry(canarias))
ggplot(canarias) +
  geom_sf(aes(fill = NAME_BOUND), alpha = 0.6, col = "black", show.legend = FALSE)

ggplot(peninsula.paro2) +
  geom_sf(aes(fill = Tasa.paro), colour = "grey80", size = 0.1) +
  geom_sf_text(aes(label = NAME_2), size = 1)+
  facet_grid(~Sexo) +
  scale_fill_viridis_c("Tasa de paro", option = "plasma",direction=-1) +
  labs(title = "Tasa de paro por provincia — 2014 T3") +
  theme_minimal()


# MAPAS INTERACTIVOS

mapa <- ggplot(peninsula.paro) +
  geom_sf(aes(fill = Tasa.paro), colour = "grey80", size = 0.1)+
  facet_grid(~Sexo)+
  scale_fill_gradient("Tasa de paro", low = "aliceblue", high = "steelblue4") +
  theme_bw()
mapa


library(plotly)

ggplotly(mapa)

mapa2 <- ggplot(peninsula.paro) +
  geom_sf(aes(fill = Tasa.paro,
              text = paste0(NAME_2, "<br>Tasa de paro: ", round(Tasa.paro))),
              colour = "grey80", size = 0.1)+
  facet_grid(~Sexo)+
  scale_fill_gradient("Tasa de paro", low = "aliceblue", high = "steelblue4") +
  theme_bw()
ggplotly(mapa2,tooltip = "text")


# EXPORTAR GRAFICOS

ggsave("res/grafico_basico.png", plot = mapa)

# Raster para web
ggsave("res/grafico_web.png", plot = mapa, width = 16, height = 10, units = "cm", dpi = 96)

# Raster de alta resolución para imprimir
ggsave("res/grafico_print.png", plot = mapa, width = 16, height = 10, units = "cm", dpi = 300)

# Formato exigido por muchas revistas
ggsave("res/grafico_revista.tiff", plot = mapa, width = 16, height = 10, units = "cm", dpi = 600)


# Vectorial, tamaño "infinito" en calidad
ggsave("res/grafico_vector.pdf", plot = mapa, width = 16, height = 10, units = "cm")


# Metodo clasico

png("res/grafico_manual.png", width = 16, height = 10, units = "cm", res = 300)
mapa
dev.off()

canarias <- st_read("data/municipios_canarias/ll_municipales_inspire_canarias_wgs84.shp")
class(canarias)
head(st_drop_geometry(canarias))
mapa_canarias<-ggplot(canarias) +
  geom_sf(aes(fill = NAME_BOUND), alpha = 0.6, col = "black", show.legend = FALSE)

pdf("res/grafico_manual_dos_graficos.pdf", width = 16 / 2.54, height = 10 / 2.54)  # pdf() usa pulgadas
mapa
mapa_canarias
dev.off()


library(gridExtra)

pdf_graficos <- marrangeGrob(
  list(mapa, mapa_canarias),
  nrow = 1,
  ncol = 1
)
ggsave(
  "res/grafico_manual_dos_graficos_ggsave.pdf",
  pdf_graficos,
  width = 16,
  height = 10,
  units = "cm"
)


pdf_graficos2 <- marrangeGrob(
  list(mapa, mapa_canarias),
  nrow = 1,
  ncol = 2
)
ggsave(
  "res/grafico_manual_dos_graficos_ggsave2.pdf",
  pdf_graficos2,
  width = 16,
  height = 10,
  units = "cm"
)



# Temas predefinidos

load("data/datos.cancer.RData")

cancer # Base de datos en formato data.table y esta en long

cancer2<-cancer[cancer$tumor%in%c("MAMA","PROSTATA","COLORRECTAL"),]

p<-ggplot(cancer2,aes(x = periodo,y = tasa,shape=sexo,colour = tumor)) +
  geom_line() +
  geom_point(size = 1)

p


library(cowplot)
p + theme_cowplot() 


p + cowplot::theme_cowplot() 




# Paletas de colores

p_bueno <- p + theme_cowplot()

p_bueno + ggsci::scale_color_npg() 

p_bueno + ggsci::scale_color_aaas() 

p_bueno + scale_color_brewer(palette = "Set2") 




temazo <- theme_cowplot()+ 
  theme(
  plot.title = element_text(face = "bold", size = 10),
  axis.title = element_text(size = 15),
  axis.text = element_text(size = 10, color = "black"),
  legend.position = "top",
  legend.title = element_text(size = 15),
  panel.grid = element_blank(),
  axis.line = element_line(linewidth = 0.4),
  axis.ticks = element_line(linewidth = 0.4)
)

p + temazo


source("temazos.R")

p + temazo_verano

p + temazo_otono


# Combinar graficos en una misma figura

library(patchwork)

p1 <- ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() + 
  temazo_otono
p1

p2 <- ggplot(mtcars, aes(factor(cyl), mpg)) + 
  geom_boxplot() + 
  temazo_otono
p2

p3 <- ggplot(mtcars, aes(factor(gear), mpg)) + 
  geom_boxplot() + 
  temazo_otono
p3


(p1 | p2 | p3) + plot_annotation(tag_levels = "A") 



cowplot::plot_grid(p1, p2, labels = c("(A)", "(B)"), ncol = 2)

cowplot::plot_grid(p1, p2,p3, labels = c("(A)", "(B)","(C)"), ncol = 2)


# IA

“Con el data frame mtcars, haz un scatterplot de mpg frente a wt, 
coloreado por cyl como factor, tema theme_pubr(), 
paleta ggsci::scale_color_npg(), 
y prepara el ggsave() para exportarlo como PDF vectorial de 16x10 cm.
No te inventes datos, y ajustate a colores para daltonicos”

library(ggplot2)
library(ggpubr)
library(ggsci)

p <- ggplot(mtcars,
  aes(x = wt, y = mpg, colour = factor(cyl))) +
  geom_point(size = 3, alpha = 0.9) +
  labs(
    x = "Peso (1000 lb)",
    y = "Consumo (mpg)",
    colour = "Cilindros"
  ) +
  scale_color_npg() +
  theme_pubr()

p

# Exportación PDF vectorial (16 x 10 cm)
ggsave(
  filename = "res/mtcars_mpg_vs_wt.pdf",
  plot = p,
  device = cairo_pdf,  # PDF vectorial con buena gestión de fuentes
  width = 16,
  height = 10,
  units = "cm"
)


library(ggpubr)
library(ggsci)

ggplot(mtcars, aes(x = wt, y = mpg, color = factor(cyl))) +
  geom_point(size = 3) +
  labs(title = "Consumo vs. peso", x = "Peso (1000 lbs)", y = "MPG", color = "Cilindros") +
  theme_pubr() +
  scale_color_npg()


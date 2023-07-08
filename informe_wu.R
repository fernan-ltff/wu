##Instalación de paquetes

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)
install.packages("skimr")
library(skimr)
install.packages("ggplot2")
library(ggplot2)
install.packages("patchwork")
library(patchwork)
install.packages("gganimate")
library(gganimate)
install.packages("hrbrthemes")
library(hrbrthemes)
install.packages(c('gapminder','ggplot2','gganimate','gifski'))
library(gapminder)
library(gganimate)
library(gifski)
install.packages("ddply")
library(ddply)

library(readxl)
WU_DATA <- read_excel("WU_DATA.xlsx", sheet = "Hoja2", 
                      col_types = c("numeric", "date", "numeric", 
                                    "text", "text", "text", "text", "text", 
                                    "text", "text", "text", "text", "text"))
View(WU_DATA)
WU <-
  WU_DATA


##Gráficos de calificaciones
####Analizamos con un gráfico de barras la diferencia de calificaciones entre las sucursales:

gb_cal<-ggplot(data=WU)+
  geom_bar(mapping=aes(x=Calificacion,fill=Sucursal),position="dodge")+
  scale_fill_manual(values=c("deepskyblue4","palegreen4"))
gb_cal

###Podemos observar que las sucursales de Buenos Aires obtienen mayor cantidad de calificaciones bajas, mientras que en Bariloche obtienen calificaciones mayormente altas.

####Agregamos al gráfico la variante de fechas para evaluar qué sucede en diferentes épocas del año.



gj_cal<-
  ggplot(data=WU_DATA)+
  geom_jitter(mapping=aes(x=Fecha,y=Calificacion,color=Calificacion))+
  scale_color_gradient(low="red3",high="steelblue4")+
  facet_wrap(~Sucursal)
gj_cal

###Podemos notar que las opiniones negativas incrementan a partir de enero, aunque en Bariloche también aumenta la cantidad de calificaciones altas y en Buenos Aires se mantiene (aproximadamente) la misma cantidad.

gl_cal<-ggplot(data=WU_DATA,aes(Fecha,Calificacion))+
  geom_smooth(aes(weight = Calificacion, color=Sucursal),
              method = "loess",
              se = FALSE) +
  scale_color_manual(values=c("blue","red"))
gl_cal

###El aumento de opiniones negativas, a pesar de la cantidad de opiniones positivas, genera una decreción de calificación en ambas sucursales.

wu_cal <-
  WU%>%
  group_by(Fecha, Sucursal) %>%
  summarize(Cal_med=mean(Calificacion), .groups="drop") %>%
  as.data.frame()


###Calculando un promedio de calificaciones
grafico <- ggplot(data=wu_cal, aes(x=Fecha, y=Cal_med, color=Sucursal))+
  geom_line()+
  geom_point()+
  scale_color_manual(values=c("blue", "red"))+
  labs(title="Calificaciones Buenos Aires vs Bariloche", x="Fecha", y="Calificación")+
  transition_reveal(Fecha)

animate(plot=grafico, render=gifski_renderer(), height=400, width=600, duration=5, fps=20)
anim_save("grafico.gif")

###Podemos observar que las calificaciones decaen notablemente, no sólo en enero, sino también en julio.


###Filtrando las opiniones

totales_suc<-
  WU_DATA %>% 
  group_by(Sucursal) %>% 
  count(Sucursal)

names(totales_suc)[2]<-"Total"

mala_atencion<-
  WU_DATA %>% 
  group_by(Sucursal) %>%
  filter(B_Atencion=="No")%>% 
  count(B_Atencion=="No")%>%
  select(Sucursal,n)
names(mala_atencion)[2]<-"Total_MA"

totales_suc<-
  merge(totales_suc,mala_atencion)


buena_atencion<-
  WU_DATA %>% 
  group_by(Sucursal) %>%
  filter(B_Atencion!="No")%>% 
  count(B_Atencion!="No")%>%
  select(Sucursal,n)
names(buena_atencion)[2]<-"Total_BA"

totales_suc<-
  merge(totales_suc,buena_atencion)
rm(buena_atencion)
rm(mala_atencion)

###Calculando
####Como la cantidad de opiniones sobre las sucursales de Buenos Aires es superior a las de Bariloche, calculamos por porcentajes, ya que hacer un gráfico con los datos como están no mostraría una diferencia real
#####Cálculos
totales_suc$Pje_MA<-
  totales_suc$Total_MA*100 / totales_suc$Total
totales_suc$Pje_BA<-
  totales_suc$Total_BA*100 / totales_suc$Total

####Gráfico de porcentajes de mala atención
gma<-ggplot(data=totales_suc)+
  geom_col(mapping=aes(x=Sucursal,y=Pje_MA,fill=Sucursal),width=0.3)+
  scale_fill_manual(values=c("steelblue3","orangered2"))+
  coord_fixed(0.1)+
  labs(y="Porcentaje de Mala Atención")
gma


###Este último gráfico nos indica que, además de la diferencia de calificaciones, hay una notable diferencia en cuanto a la atención recibida en cada sucursal.

###Analizando las principales quejas

quejas_p<-
  WU_DATA %>% 
  filter(B_Atencion=="No") %>%
  group_by(Sucursal)%>%
  count(Motivo1)

quejas_p<-
  merge(quejas_p,totales_suc)

names(quejas_p)[2] <- "Motivo_Principal"
names(quejas_p)[3] <- "CQuejas"

quejas_p<-
  quejas_p%>%
  select(Sucursal,Motivo_Principal,CQuejas,Total_MA)


quejas_p$Pje<-
  quejas_p$CQuejas*100 / quejas_p$Total_MA

quejas_s <-
  WU_DATA %>%
  filter(B_Atencion=="No") %>%
  group_by(Sucursal)%>%
  count(Motivo2)

quejas_s <-
  merge(quejas_s,totales_suc)
names(quejas_s)[2] <- "Motivo_Secundario"
names(quejas_s)[3] <- "NQuejas"

quejas_s <-
  quejas_s%>%
  select(Sucursal,Motivo_Secundario,NQuejas,Total_MA)

quejas_s$Pje <-
  quejas_s$NQuejas*100/quejas_s$Total_MA

graf_qp <- ggplot(data=quejas_p)+
  geom_col(mapping=aes(x=Motivo_Principal,y=Pje,fill=Sucursal),position="dodge")+
  scale_fill_manual(values=c("steelblue3","orangered2"))+
  labs(title="Quejas Principales", subtitle="Buenos Aires vs Bariloche",y="Porcentaje",x="Motivos")+
  theme(plot.title=element_text(hjust=0.5))+
  theme(axis.text = element_text(angle = 45))

graf_qs <- ggplot(data=quejas_s)+
  geom_col(mapping=aes(x=Motivo_Secundario,y=Pje,fill=Sucursal),position="dodge")+
  scale_fill_manual(values=c("steelblue3","orangered2"))+
  labs(title="Quejas Secundarias", subtitile="Buenos Aires vs Bariloche", y="Porcentaje",x="Motivos")+
  theme(axis.text = element_text(angle = 45))

graf_qp+graf_qs

###Analizando los gráficos
####Al observar y comparar los gráficos de quejas principales y quejas secundarias, descubrimos que en Buenos Aires la mayoría de las opiniones no poseen un segundo motivo al calificar negativamente, siendo la mala atención el principal y único motivo de queja con un amplio porcentaje; en las opiniones con dos motivos, la mala atención sigue destacando.

####Entonces, ¿cuál es el motivo principal en Buenos Aires que tiene como motivo secundario la mala atención?

bsas_q<-
  WU_DATA%>%
  filter(B_Atencion=="No")%>%
  filter(Sucursal=="BSAS")%>%
  filter(Motivo2=="Atención")%>%
  select(Sucursal,Motivo1,Motivo2)
bsas_q

###Con estos datos podemos concluir que el problema en Buenos Aires radica específicamente en la atención recibida y la demora.

###¿Qué sucede en Bariloche?
####Volviendo a los gráficos, podemos ver que entre los motivos principales no hay una diferencia tan marcada, siendo la atención, el cierre y la cantidad de cajas los motivos principales; pero al agregar los motivos secundarios sí se observa una diferencia marcada, destacando la demora como el principal motivo secundario.

graf_qs+graf_qp

###¿Existe relación entre la demora y la falta de cajas?
rapidez<-
  WU_DATA%>%
  filter(Rapidez=="No")%>%
  group_by(Sucursal)%>%
  count(Sucursal)
names(rapidez)[2]<-"Total"

brc_rapidez<-
  WU_DATA%>%
  filter(Sucursal=="BRC")%>%
  filter(Rapidez=="No")%>%
  filter(Motivo1!="Demora")%>%
  group_by(Motivo1)%>%
  count(Motivo1)
names(brc_rapidez)[2]<-"Cantidad"

brc_rapidez<-
  merge(rapidez,brc_rapidez)
brc_rapidez<-
  brc_rapidez%>%
  filter(Sucursal=="BRC")

brc_rapidez$Pje<-
  brc_rapidez$Cantidad*100/brc_rapidez$Total


gr_rapidez<-ggplot(data=brc_rapidez)+
  geom_col(mapping=aes(x=Motivo1,y=Pje,fill=Motivo1))+
  labs(title="Quejas en base a demora", x="Motivo", y="Porcentaje", fill="Motivos")
  gr_rapidez

  
  ------
    
    rapidez2<-
    WU_DATA%>%
    filter(Rapidez=="No")%>%
    filter(Motivo1!="Demora") %>% 
    group_by(Sucursal)%>%
    count(Sucursal)
  names(rapidez2)[2]<-"Total"
  
  brc_rapidez2<-
    WU_DATA%>%
    filter(Sucursal=="BRC")%>%
    filter(Rapidez=="No")%>%
    filter(Motivo1!="Demora")%>%
    group_by(Motivo1)%>%
    count(Motivo1)
  names(brc_rapidez2)[2]<-"Cantidad"
  
  brc_rapidez2<-
    merge(rapidez2,brc_rapidez2)
  brc_rapidez2<-
    brc_rapidez2%>%
    filter(Sucursal=="BRC")
  
  brc_rapidez2$Pje<-
    brc_rapidez2$Cantidad*100/brc_rapidez2$Total
  
  
  gr_rapidez2<-ggplot(data=brc_rapidez2)+
    geom_col(mapping=aes(x=Motivo1,y=Pje,fill=Motivo1))+
    labs(title="Quejas en base a demora", x="Motivo", y="Porcentaje", fill="Motivos")
  gr_rapidez2
  
  
  
  
  
  
######Para el gráfico se analizaron todos los comentarios (independientemente de la atención recibida) referidos a la lentitud de la atención, discriminando los motivos de queja que agregaron.

##Conclusión  
##En base a lo observado y expuesto, respecto a las sucursales de Buenos Aires, es primordial enfocarse en la atención de los empleados hacia los clientes.
##En cuanto a Bariloche, la falta de cajas es una problemática a solucionar para mejorar el servicio. Por otro lado, podemos ver que el horario de cierre es otro punto a tener en cuenta de manera urgente, dado que, no sólo es el segundo motivo de quejas en base a las demoras, sino que obtiene un porcentaje notable.


#### Inmigracion y diversidad en España Crisis Económica y Gestión Municipal

#### **Editorial:** [***ICARIA***](http://www.icariaeditorial.com/)    
#### **Editor:** [***Andreu Domingo***](http://ced.uab.es/directori/andreu-domingo-valls/)    
#### **Data visualization:** [***Juan Galeano***](http://ced.uab.es/directori/juan-galeano/)  

REPOSITORY UNDER CONSTRUCTION
#### **Editorial:** [***ICARIA***](http://www.icariaeditorial.com/libros.php?id=1613)    
#### **Editor:** [***Andreu Domingo***](http://ced.uab.es/directori/andreu-domingo-valls/)    
#### **Data visualization:** [***Juan Galeano***](http://ced.uab.es/directori/juan-galeano/)    

***

Este documento contiene el código R utilizado para la confección de cada uno de los gráficos que se presentan en los distintos capítulos del libro. El unico cambio que se ha realizado a la hora de elaborar este documento han sido unos ligeros ajustes sobre el tamaño de las fuentes.

En el caso de los figuras que contienen mapas gran parte del análisis estadístico también fue realizado con R. Sin embargo, a la hora de visualizar esos resultados sobre el territorio se ha utilizado el programa QGIS. Aqui sólo aparece el mapa final, no el código de análsisi del cual emanan los resultados.  


### **<span style="color:red">Capítulo 1: IMIGRACIÓN INTERNACIONAL Y CAMBIO DEMOGRÁFICO EN EL NUEVO MILENIO [Autores: Juan Galeano & Albert Sabater]</span>**  

<br>

#### **Figura 1.1: Población nacida en el extranjero según región de nacimiento por Comunidad Autónoma, 2000-2014** 

```{r fig.width=16.5, fig.height=9.8}
library (dplyr)
library(ggplot2)
load(url('http://gedemced.uab.cat/DATOS_ICARIA/DATOS_ICARIA_1.Rdata'))
dffinal<-dffinal[!dffinal$COM=="CEUTA Y MELILLA", ]
dffinal$COM<-with(dffinal, ifelse(COM=="CATALUNYA", "CATALUÑA", COM))
POPEXTCOM <- dffinal %>% group_by(COM,YEAR) %>%  summarise(secciones = n(), 
                                                 Spanish = sum(Spanish), 
                                                 LatinaAmerica = sum(LatinAmerica), 
                                                 WesternEurope = sum(WesternEurope), 
                                                 EasternEurope = sum(EasternEurope), 
                                                 Africa = sum(Africa), 
                                                 Asia = sum(Asia), 
                                                 Others = sum(Others),
                                                 Total_ext=sum(Total_ext),
                                                 Totalpop=sum(Totalpop))
POPEXT <- with(POPEXTCOM, c(LatinaAmerica,WesternEurope,EasternEurope,Africa,Asia))
GRUP <-rep(c("Latinoamérica","Europa Occidental","Europa Oriental","África","Asia"), each=255)
COM <- rep(POPEXTCOM$COM, 5)
YEAR<- rep(POPEXTCOM$YEAR, 5)
POPEXT <-data.frame(GRUP,POPEXT,YEAR,COM)
POPEXT$COM <- factor(POPEXTCOM$COM,
                   levels = c( "CATALUÑA","COMUNIDAD\nMADRID","COMUNIDAD\nVALENCIANA",
                               "ANDALUCIA","CANARIAS","BALEARES","CASTILLA\nLA MANCHA",
                               "MURCIA","GALICIA","CASTILLA\nLEON", "ARAGON",
                               "PAIS\nVASCO", "COMUNIDAD\nNAVARRA","ASTURIAS","CANTABRIA",
                               "EXTREMADURA", "LA RIOJA"))
POPEXT$GRUP <- factor(POPEXT$GRUP,
                     levels = c("Latinoamérica","Europa Occidental","Europa Oriental","África","Asia"))
A <-ggplot(data=POPEXT, aes(x=YEAR, y=POPEXT, fill=GRUP)) + 
  geom_area(stat="identity", colour="#585858")+
  scale_y_continuous(breaks=seq(0, 1250000, 250000))+
  scale_x_continuous(breaks=c(2000, 2014))+
  facet_wrap(~ COM, nrow=1)+
  scale_fill_manual(name="",
                     values=c("#D8D8D8", "#A4A4A4", "#6E6E6E", "#424242", "#1C1C1C"),
                     breaks=c("Latinoamérica","Europa Occidental","Europa Oriental","África","Asia"),
                     labels=c("Latinoamérica","Europa Occidental","Europa Oriental","África","Asia"))+
  theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 20),
              legend.position="bottom",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 90,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=20),
              axis.text.y  = element_text(vjust=0.5, size=20,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=7.5,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+ ylab("Población nacida en el extranjero\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP1_FIG1_POBLACION_EXTRANJERA_POR_COMUNIDAD_2000_2014.tiff", scale = 3, width = 16.5, height = 8.8, units = c("cm"), dpi = 300)
```

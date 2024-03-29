## Inmigracion y diversidad en España Crisis Económica y Gestión Municipal

#### **Editorial:** [***ICARIA***](http://www.icariaeditorial.com/)    
#### **Editor:** [***Andreu Domingo***](http://ced.uab.es/directori/andreu-domingo-valls/)    
#### **Data visualization:** [***Juan Galeano***](http://ced.uab.es/directori/juan-galeano/)  

***

Este documento contiene el código R utilizado para la elaboración de cada uno de los gráficos que se presentan en los distintos capítulos del libro. El único cambio que se ha realizado a la hora de elaborar este documento han sido unos ligeros ajustes sobre el tamaño de las fuentes.

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

<br>

#### **Figura 1.2: Índice de disimilitud 2000-2007-2014, municipios de más de 25.000 habitantes agrupados por comunidades autónomas**           
```{r fig.width=16.5, fig.height=9.8}
load(url('http://gedemced.uab.cat/DATOS_ICARIA/DATOS_ICARIA_2.Rdata'))
CODIGOS<-seg0111[(seg0111$TotPop>=25000&seg0111$year==2014&seg0111$AREAS>=10),1]
CODIGOS <-data.frame(CODIGOS)
colnames(CODIGOS)<-"CODMUN"
NOMBRES_MUNICIPIOS_2014_UTF <-read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/NOMBRES_MUNICIPIOS_2014_UTF.csv"), stringsAsFactors=FALSE)
NOMBRES_MUNICIPIOS_2014_UTF<-NOMBRES_MUNICIPIOS_2014_UTF[,1:2]
colnames(NOMBRES_MUNICIPIOS_2014_UTF)<- c("Nom_Mun","CODMUN")
NOMBRES_MUNICIPIOS_2014_UTF$codmun <- sprintf("%.5d",NOMBRES_MUNICIPIOS_2014_UTF$CODMUN)
CODIGOS <- merge(CODIGOS,NOMBRES_MUNICIPIOS_2014_UTF, by="CODMUN")
SEG2b <- seg0111[seg0111$CODMUN %in% CODIGOS$CODMUN, ]
YEAR <-rep(SEG2b$year, 5)
COM <- rep(SEG2b$COM, 5)
CONT <- rep(c("LATINOAMERICA", "EUROPA\nOCCIDENTAL","EUROPA\nORIENTAL","AFRICA","ASIA"),each=663)
DIS<-c(SEG2b$D2,SEG2b$D3,SEG2b$D4,SEG2b$D5,SEG2b$D6)
IS<-c(SEG2b$IS2,SEG2b$IS3,SEG2b$IS4,SEG2b$IS5,SEG2b$IS6)
SEG <- data.frame(COM,YEAR,CONT,DIS,IS)
SEG <-SEG[!SEG$COM=="0",]
SEG$CONT <- factor(SEG$CONT,
                   levels = c("ASIA","AFRICA",
                              "EUROPA\nORIENTAL", 
                              "EUROPA\nOCCIDENTAL",
                              "LATINOAMERICA"))
p <- ggplot(SEG, aes((YEAR), DIS))
A <-p + annotate("segment", x = 2000, xend =2014, y = 50, yend = 50,
        colour = "black", alpha=1,linetype=1)+
        geom_boxplot(aes(fill = factor(YEAR)), outlier.colour = "black", outlier.size = 1)+
        scale_y_continuous(limits = c(0, 100))+
        scale_x_continuous(breaks=c(2000, 2007,2014))+
        facet_grid(CONT ~ COM,scales = "free", space="free")+
        scale_fill_manual(values=c("#BDBDBD", "#848484","#585858"))+
        theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 20),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 90,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=10.5,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("Indice de disimilitud, Municipios > 25.000 personas (306)\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP1_FIG2_SEGREGACION_POR_COMUNIDAD_2000__2007_2014.tiff", scale = 3, width = 16.5, height = 8.8, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 1.3:  Estructura por sexo, edad y lugar de nacimiento de la población residente en enclaves, 2014**   
####**Municipio de Barcelona (Barcelona)[Pirámide superpuesta en términos relativos]**

```{r fig.width=6.3, fig.height=4.7}
library(ggplot2)
library(RColorBrewer)
require(dplyr)
library(scales)
AMB_EDADES_2014 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/AMB_EDADES_2014.csv"), sep=";", stringsAsFactors=FALSE)
# creo un codigo de indentificación (prov+mun+dist+secc) se fija la amplitud, poniendo 0s en frente:
AMB_EDADES_2014$provincia <- sprintf("%.2d",AMB_EDADES_2014$provincia) # prov tiene 2 digitos
AMB_EDADES_2014$municipio <- sprintf("%.5d",AMB_EDADES_2014$municipio)  # mun tiene 3 digitos
AMB_EDADES_2014$distrito  <- sprintf("%.2d",AMB_EDADES_2014$distrito) # dist tiene 2
AMB_EDADES_2014$seccion   <- sprintf("%.3d",AMB_EDADES_2014$seccion) # secc tiene 3
# concatenar
AMB_EDADES_2014$ID   <- with(AMB_EDADES_2014, paste(provincia, substr(municipio, 3,5), distrito, seccion, sep = "-"))
AMB_EDADES_2014$munnac <-NULL
AMB_EDADES_2014$nf <- ifelse(AMB_EDADES_2014$pronac < 66, "108",0)
AMB_EDADES_2014$REGION_NAC <- ifelse(AMB_EDADES_2014$nf == 108, "1",
                         ifelse(AMB_EDADES_2014$paisnac > 302 & AMB_EDADES_2014$paisnac < 400, "2",
                                ifelse(AMB_EDADES_2014$paisnac > 101 & AMB_EDADES_2014$paisnac < 104 | 
                                         AMB_EDADES_2014$paisnac == 107 | 
                                         AMB_EDADES_2014$paisnac > 108 & AMB_EDADES_2014$paisnac < 112 | 
                                         AMB_EDADES_2014$paisnac > 112 & AMB_EDADES_2014$paisnac < 122 | 
                                         AMB_EDADES_2014$paisnac > 122 & AMB_EDADES_2014$paisnac < 127 | 
                                         AMB_EDADES_2014$paisnac > 128 & AMB_EDADES_2014$paisnac < 133, "3",
                                       ifelse(AMB_EDADES_2014$paisnac == 101 | 
                                                AMB_EDADES_2014$paisnac == 112 | 
                                                AMB_EDADES_2014$paisnac == 122 |
                                                AMB_EDADES_2014$paisnac == 128 |
                                                AMB_EDADES_2014$paisnac > 103 & AMB_EDADES_2014$paisnac < 107 | 
                                                AMB_EDADES_2014$paisnac > 134 & AMB_EDADES_2014$paisnac < 200, "4",
                                              ifelse(AMB_EDADES_2014$paisnac > 200 & AMB_EDADES_2014$paisnac < 300, "5",
                                                     ifelse(AMB_EDADES_2014$paisnac > 400 & AMB_EDADES_2014$paisnac < 500, "6",
                                                            0))))))
AMB_EDADES_2014$nf<- NULL
AMB_EDADES_2014$REGION_NAC2 <- ifelse(AMB_EDADES_2014$REGION_NAC == "1", "ESP","EXT")
ENCLAVES <- c("08-019-01-002","08-019-01-005","08-019-01-006",
              "08-019-01-007","08-019-01-008","08-019-01-009","08-019-01-010",
              "08-019-01-011","08-019-01-012","08-019-01-014","08-019-01-015",
              "08-019-01-016","08-019-01-017","08-019-01-018","08-019-01-019",
              "08-019-01-026","08-019-01-029","08-019-01-031","08-019-01-051")
AMB_ENC<-  AMB_EDADES_2014[AMB_EDADES_2014$ID %in% ENCLAVES,]    
b <- AMB_ENC %>%group_by(REGION_NAC2,sexo,edad) %>% tally()
#b <- AMB_ENC %>%group_by(REGION_NAC,paisnac,municipio) %>% tally()
b <- b[order(-b$n),] 
ages <- data.frame(c(0:110))
colnames(ages)<- "edad"
HOMESP <-b[(b$REGION_NAC2=="ESP" & b$sexo==1),]
HOMESP<-merge(HOMESP,ages, by="edad",all = TRUE)
HOMESP$REGION_NAC2<- "ESP"
HOMESP$sexo<- "Hombres\nEspaña"
HOMESP[is.na(HOMESP)]<- 0
MUJESP <-b[(b$REGION_NAC2=="ESP" & b$sexo==6),]
MUJESP<-merge(MUJESP,ages, by="edad",all = TRUE)
MUJESP$REGION_NAC2<- "ESP"
MUJESP$sexo<- "Mujeres\nEspaña"
MUJESP[is.na(MUJESP)]<- 0
HOMEXT <-b[(b$REGION_NAC2=="EXT" & b$sexo==1),]
HOMEXT<-merge(HOMEXT,ages, by="edad",all = TRUE)
HOMEXT$REGION_NAC2<- "EXT"
HOMEXT$sexo<- "Hombres\nExtranjero"
HOMEXT[is.na(HOMEXT)]<- 0
MUJEXT <-b[(b$REGION_NAC2=="EXT" & b$sexo==6),]
MUJEXT<-merge(MUJEXT,ages, by="edad",all = TRUE)
MUJEXT$REGION_NAC2<- "EXT"
MUJEXT$sexo<- "Mujeres\nExtranjero"
MUJEXT[is.na(MUJEXT)]<- 0
HOMESP$nREL <- ((HOMESP$n/(sum(HOMESP$n)+sum(MUJESP$n)))*100)*-1
MUJESP$nREL <- MUJESP$n/(sum(HOMESP$n)+sum(MUJESP$n))*100
HOMEXT$nREL <- ((HOMEXT$n/(sum(HOMEXT$n)+sum(MUJEXT$n)))*100)*-1
MUJEXT$nREL <- MUJEXT$n/(sum(HOMEXT$n)+sum(MUJEXT$n))*100
ESP <- rbind(HOMESP,MUJESP,HOMEXT,MUJEXT)
A<- ggplot(ESP, aes(x=edad, y=nREL, fill=sexo))+
   geom_bar(data = ESP[(ESP$sexo=="Mujeres\nEspaña"|
                   ESP$sexo=="Hombres\nEspaña"),], 
                   colour = I("Black"),stat="identity", position="identity", size=.3, colour="black",
                   aes(x = edad, y = nREL,fill = sexo), alpha = 1)+
   geom_bar(data = ESP[(ESP$sexo=="Hombres\nExtranjero"|
                          ESP$sexo=="Mujeres\nExtranjero"),], 
            colour = I("Black"),stat="identity", position="identity", size=.3, colour="black",
            aes(x = edad, y = nREL,fill = sexo), alpha = .5)+
           coord_flip()+
 # facet_grid(. ~ TITOL)+
  scale_y_continuous(limits=c(-2.5,2.5),breaks = c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2, 2.5), 
                     labels = paste0(as.character(c(seq(2.5, 0, -0.5), seq(0.5, 2.5, 0.5))), "%")) + 
           scale_x_continuous(breaks=seq(0,110,5)) +
           scale_fill_manual(values = c("#424242", "#BDBDBD","#585858", "#D8D8D8"))+
  annotate("text", x = 107.5, y = -2.5*.25, label = "Hombres",size=5)+
  annotate("text", x = 107.5, y = 2.5*0.25, label = "Mujeres",size=5)+
  annotate("rect", xmin = 101, xmax = 104, ymin = -2.5*.90, ymax =-2.5*.80, alpha=1, fill="#424242")+
  annotate("text", x = 102.5, y = -1.40, label = "España",size=5)+
  annotate("rect", xmin = 91, xmax = 94,  ymin = -2.5*.90, ymax =-2.5*.80,alpha=1, fill="#BDBDBD")+
  annotate("text", x = 92.5,  y = -1.30, label = "Extranjeros",size=5)+ 
  annotate("rect", xmin = 101, xmax = 104,  ymin = 2.5*.90, ymax =2.5*.80, alpha=1, fill="#585858")+
  annotate("text", x = 102.5, y = 1.40, label = "España",size=5)+
  annotate("rect", xmin = 91, xmax = 94, ymin = 2.5*.90, ymax =2.5*.80, alpha=1, fill="#D8D8D8")+
  annotate("text", x = 92.5,  y = 1.30, label = "Extranjeros",size=5)+ 
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP1_FIG3_PIRAMIDE_BARCELONA_RELATIVOS_2014.tiff", scale = 3, width = 6.3, height = 4.7, units = c("cm"), dpi = 300)
```

<br>

####**Municipio de Barcelona (Barcelona)[Pirámide compuesta en términos absolutos]**

```{r fig.width=6.3, fig.height=4.7}
AMB_ENC<-  AMB_EDADES_2014[AMB_EDADES_2014$ID %in% ENCLAVES,]    
b <- AMB_ENC %>%group_by(REGION_NAC,sexo,edad) %>% tally()
ages <- data.frame(c(0:110))
colnames(ages)<- "edad"
HOMESP <-b[(b$REGION_NAC==1 & b$sexo==1),]
HOMESP<-merge(HOMESP,ages, by="edad",all = TRUE)
HOMESP$REGION_NAC<- 1
HOMESP$sexo<- "Hombres\nEspaña"
HOMESP[is.na(HOMESP)]<- 0
MUJESP <-b[(b$REGION_NAC==1 & b$sexo==6),]
MUJESP<-merge(MUJESP,ages, by="edad",all = TRUE)
MUJESP$REGION_NAC<- 1
MUJESP$sexo<- "Mujeres\nEspaña"
MUJESP[is.na(MUJESP)]<- 0
HOMLA <-b[(b$REGION_NAC==2 & b$sexo==1),]
HOMLA<-merge(HOMLA,ages, by="edad",all = TRUE)
HOMLA$REGION_NAC<- 2
HOMLA$sexo<- "Hombres\nLatinoamerica"
HOMLA[is.na(HOMLA)]<- 0
MUJLA <-b[(b$REGION_NAC==2 & b$sexo==6),]
MUJLA<-merge(MUJLA,ages, by="edad",all = TRUE)
MUJLA$REGION_NAC<- 2
MUJLA$sexo<- "Mujeres\nLatinoamerica"
MUJLA[is.na(MUJLA)]<- 0
HOMWE <-b[(b$REGION_NAC==3 & b$sexo==1),]
HOMWE<-merge(HOMWE,ages, by="edad",all = TRUE)
HOMWE$REGION_NAC<- 3
HOMWE<-HOMWE[1:111,]
HOMWE$sexo<- "Hombres\nEuropa_Occ."
HOMWE[is.na(HOMWE)]<- 0
MUJWE <-b[(b$REGION_NAC==3 & b$sexo==6),]
MUJWE<-merge(MUJWE,ages, by="edad",all = TRUE)
MUJWE$REGION_NAC<- 3
MUJWE$sexo<- "Mujeres\nEuropa_Occ."
MUJWE[is.na(MUJWE)]<- 0
HOMEE <-b[(b$REGION_NAC==4 & b$sexo==1),]
HOMEE<-merge(HOMEE,ages, by="edad",all = TRUE)
HOMEE$REGION_NAC<- 4
HOMEE$sexo<- "Hombres\nEuropa_Or."
HOMEE[is.na(HOMEE)]<- 0
MUJEE <-b[(b$REGION_NAC==4 & b$sexo==6),]
MUJEE<-merge(MUJEE,ages, by="edad",all = TRUE)
MUJEE$REGION_NAC<- 4
MUJEE$sexo<- "Mujeres\nEuropa_Or."
MUJEE[is.na(MUJEE)]<- 0
HOMAF <-b[(b$REGION_NAC==5 & b$sexo==1),]
HOMAF<-merge(HOMAF,ages, by="edad",all = TRUE)
HOMAF$REGION_NAC<- 5
HOMAF$sexo<- "Hombres\nAfrica"
HOMAF[is.na(HOMAF)]<- 0
MUJAF <-b[(b$REGION_NAC==5 & b$sexo==6),]
MUJAF<-merge(MUJAF,ages, by="edad",all = TRUE)
MUJAF$REGION_NAC<- 5
MUJAF$sexo<- "Mujeres\nAfrica"
MUJAF[is.na(MUJAF)]<- 0
HOMAS <-b[(b$REGION_NAC==6 & b$sexo==1),]
HOMAS<-merge(HOMAS,ages, by="edad",all = TRUE)
HOMAS$REGION_NAC<- 6
HOMAS$sexo<- "Hombres\nAsia"
HOMAS[is.na(HOMAS)]<- 0
MUJAS <-b[(b$REGION_NAC==6 & b$sexo==6),]
MUJAS<-merge(MUJAS,ages, by="edad",all = TRUE)
MUJAS$REGION_NAC<- 6
MUJAS$sexo<- "Mujeres\nAsia"
MUJAS[is.na(MUJAS)]<- 0
HOMOT <-b[(b$REGION_NAC==0 & b$sexo==1),]
HOMOT<-merge(HOMOT,ages, by="edad",all = TRUE)
HOMOT$REGION_NAC<- 0
HOMOT$sexo<- "Hombres\nOtros"
HOMOT[is.na(HOMOT)]<- 0
MUJOT <-b[(b$REGION_NAC==0 & b$sexo==6),]
MUJOT<-merge(MUJOT,ages, by="edad",all = TRUE)
MUJOT$REGION_NAC<- 0
MUJOT$sexo<- "Mujeres\nOtros"
MUJOT[is.na(MUJOT)]<- 0
HOMESP$n <- HOMESP$n*-1
HOMLA$n <- HOMLA$n*-1
HOMWE$n <- HOMWE$n*-1
HOMEE$n <- HOMEE$n*-1
HOMAF$n <- HOMAF$n*-1
HOMAS$n <- HOMAS$n*-1
HOMOT$n <- HOMOT$n*-1
ESP <- rbind(HOMESP,MUJESP,HOMLA,MUJLA,HOMWE, MUJWE, HOMEE,MUJEE,HOMAF,MUJAF,
             HOMAS,MUJAS,HOMOT,MUJOT)
ESP$SEXO2 <- substr(ESP$sexo, 9,25)
ESP$SEXO2<- factor(ESP$SEXO2,
                   levels = c("España", "Latinoamerica", "Europa_Occ.", "Europa_Or.", "Africa", "Asia", "Otros"))
A<-ggplot(ESP, aes(x=edad, y=n, fill=sexo))+
  geom_bar(data = ESP[(ESP$sexo=="Mujeres\nEspaña"|
                                         ESP$sexo=="Mujeres\nLatinoamerica"|
                                         ESP$sexo=="Mujeres\nEuropa_Occ."|
                                         ESP$sexo=="Mujeres\nEuropa_Or."|
                                         ESP$sexo=="Mujeres\nAfrica"|
                                         ESP$sexo=="Mujeres\nAsia"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = edad, y = n,fill = SEXO2), alpha = 1)+
  geom_bar(data = ESP[(ESP$sexo=="Hombres\nEspaña"|
                                           ESP$sexo=="Hombres\nLatinoamerica"|
                                           ESP$sexo=="Hombres\nEuropa_Occ."|
                                           ESP$sexo=="Hombres\nEuropa_Or."|
                                           ESP$sexo=="Hombres\nAfrica"|
                                           ESP$sexo=="Hombres\nAsia"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = edad, y = n,fill = SEXO2), alpha = 1)+
  coord_flip()+
 scale_y_continuous(limits=c(-750,750),breaks = c(-750, -500, -250,0,250,500,750), 
                     labels = paste0(as.character(c(seq(750, 0, -250), seq(250, 750, 250))), "")) + 
  scale_x_continuous(breaks=seq(0,110,5)) +
  scale_fill_manual(values = c("#F2F2F2","#D8D8D8","#A4A4A4","#6E6E6E",
                               "#424242","#1C1C1C"))+
  
  
  annotate("text", x = 107.5, y = -750*0.25,  label = "Hombres",size=5)+
  annotate("text", x = 107.5, y = 750*0.25, label = "Mujeres",size=5)+
   theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=10),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP1_FIG3_PIRAMIDE_BARCELONA_ABSOLUTOS_2014.tiff", scale = 3, width = 6.3, height = 4.7, units = c("cm"), dpi = 300)
```

<br>

####**Municipios de las comarcas de Alt y Baix Marina y Baix Segura (Alicante)[Pirámide superpuesta en términos relativos]**

```{r fig.width=6.3, fig.height=4.7}
library(ggplot2)
library(RColorBrewer)
require(dplyr)
library(scales)
AMB_EDADES_2014 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/VAL_EDADES_2014.csv"), sep=";", stringsAsFactors=FALSE)
# creo un codigo de indentificaciÃ³n (prov+mun+dist+secc) se fija la amplitud, poniendo 0s en frente:
AMB_EDADES_2014$provincia <- sprintf("%.2d",AMB_EDADES_2014$provincia) # prov tiene 2 digitos 
AMB_EDADES_2014$municipio <- sprintf("%.5d",AMB_EDADES_2014$municipio)  # mun tiene 3 digitos
AMB_EDADES_2014$distrito  <- sprintf("%.2d",AMB_EDADES_2014$distrito) # dist tiene 2
AMB_EDADES_2014$seccion   <- sprintf("%.3d",AMB_EDADES_2014$seccion) # secc tiene 3
# concatenar
AMB_EDADES_2014$ID   <- with(AMB_EDADES_2014, paste(provincia, substr(municipio, 3,5), distrito, seccion, sep = "-"))
AMB_EDADES_2014$munnac <-NULL
AMB_EDADES_2014$nf <- ifelse(AMB_EDADES_2014$pronac < 66, "108",0)
AMB_EDADES_2014$REGION_NAC <- ifelse(AMB_EDADES_2014$nf == 108, "1",
                         ifelse(AMB_EDADES_2014$paisnac > 302 & AMB_EDADES_2014$paisnac < 400, "2",
                                ifelse(AMB_EDADES_2014$paisnac > 101 & AMB_EDADES_2014$paisnac < 104 | 
                                         AMB_EDADES_2014$paisnac == 107 | 
                                         AMB_EDADES_2014$paisnac > 108 & AMB_EDADES_2014$paisnac < 112 | 
                                         AMB_EDADES_2014$paisnac > 112 & AMB_EDADES_2014$paisnac < 122 | 
                                         AMB_EDADES_2014$paisnac > 122 & AMB_EDADES_2014$paisnac < 127 | 
                                         AMB_EDADES_2014$paisnac > 128 & AMB_EDADES_2014$paisnac < 133, "3",
                                       ifelse(AMB_EDADES_2014$paisnac == 101 | 
                                                AMB_EDADES_2014$paisnac == 112 | 
                                                AMB_EDADES_2014$paisnac == 122 |
                                                AMB_EDADES_2014$paisnac == 128 |
                                                AMB_EDADES_2014$paisnac > 103 & AMB_EDADES_2014$paisnac < 107 | 
                                                AMB_EDADES_2014$paisnac > 134 & AMB_EDADES_2014$paisnac < 200, "4",
                                              ifelse(AMB_EDADES_2014$paisnac > 200 & AMB_EDADES_2014$paisnac < 300, "5",
                                                     ifelse(AMB_EDADES_2014$paisnac > 400 & AMB_EDADES_2014$paisnac < 500, "6",
                                                            0))))))
AMB_EDADES_2014$nf<- NULL
AMB_EDADES_2014$REGION_NAC2 <- ifelse(AMB_EDADES_2014$REGION_NAC == "1", "ESP","EXT")
ENCLAVES <- c("03-006-01-001","03-011-01-002","03-011-01-004","03-011-01-005",
              "03-011-01-006","03-012-01-001","03-018-03-002","03-018-03-005",
              "03-029-01-001","03-031-02-003","03-031-02-006","03-031-02-007",
              "03-031-02-008","03-041-02-001","03-042-01-001","03-047-01-004",
              "03-047-02-001","03-047-02-002","03-062-01-001","03-070-01-002",
              "03-076-01-007","03-082-02-002","03-082-03-001","03-082-03-003",
              "03-085-01-001","03-094-01-003","03-094-01-004","03-094-01-007",
              "03-099-05-001","03-099-05-002","03-099-05-003","03-099-05-004",
              "03-113-01-002","03-118-01-002","03-120-01-002","03-133-01-006",
              "03-133-01-008","03-133-01-009","03-133-02-003","03-133-02-005",
              "03-133-02-007","03-133-02-009","03-133-03-004","03-133-03-006",
              "03-133-03-008","03-133-03-010","03-133-03-011","03-133-03-013",
              "03-133-03-014","03-133-03-015","03-139-03-005","03-139-03-006",
              "03-901-01-001","03-902-01-004","03-902-01-008") 
AMB_ENC<-  AMB_EDADES_2014[AMB_EDADES_2014$ID %in% ENCLAVES,]    
b <- AMB_ENC %>%group_by(REGION_NAC2,sexo,edad) %>% tally()
#b <- AMB_ENC %>%group_by(REGION_NAC,paisnac,municipio) %>% tally()
b <- b[order(-b$n),] 
ages <- data.frame(c(0:110))
colnames(ages)<- "edad"
HOMESP <-b[(b$REGION_NAC2=="ESP" & b$sexo==1),]
HOMESP<-merge(HOMESP,ages, by="edad",all = TRUE)
HOMESP$REGION_NAC2<- "ESP"
HOMESP$sexo<- "Hombres\nEspaña"
HOMESP[is.na(HOMESP)]<- 0
MUJESP <-b[(b$REGION_NAC2=="ESP" & b$sexo==6),]
MUJESP<-merge(MUJESP,ages, by="edad",all = TRUE)
MUJESP$REGION_NAC2<- "ESP"
MUJESP$sexo<- "Mujeres\nEspaña"
MUJESP[is.na(MUJESP)]<- 0
HOMEXT <-b[(b$REGION_NAC2=="EXT" & b$sexo==1),]
HOMEXT<-HOMEXT[c(1:105),]
HOMEXT<-merge(HOMEXT,ages, by="edad",all = TRUE)
HOMEXT$REGION_NAC2<- "EXT"
HOMEXT$sexo<- "Hombres\nExtranjero"
HOMEXT[is.na(HOMEXT)]<- 0
MUJEXT <-b[(b$REGION_NAC2=="EXT" & b$sexo==6),]
MUJEXT<-merge(MUJEXT,ages, by="edad",all = TRUE)
MUJEXT$REGION_NAC2<- "EXT"
MUJEXT$sexo<- "Mujeres\nExtranjero"
MUJEXT[is.na(MUJEXT)]<- 0
HOMESP$nREL <- ((HOMESP$n/(sum(HOMESP$n)+sum(MUJESP$n)))*100)*-1
MUJESP$nREL <- MUJESP$n/(sum(HOMESP$n)+sum(MUJESP$n))*100
HOMEXT$nREL <- ((HOMEXT$n/(sum(HOMEXT$n)+sum(MUJEXT$n)))*100)*-1
MUJEXT$nREL <- MUJEXT$n/(sum(HOMEXT$n)+sum(MUJEXT$n))*100 
ESP <- rbind(HOMESP,MUJESP,HOMEXT,MUJEXT)
A<- ggplot(ESP, aes(x=edad, y=nREL, fill=sexo))+
   geom_bar(data = ESP[(ESP$sexo=="Mujeres\nEspaña"|
                   ESP$sexo=="Hombres\nEspaña"),], 
                   colour = I("Black"),stat="identity", position="identity", size=.3, colour="black",
                   aes(x = edad, y = nREL,fill = sexo), alpha = 1)+
   geom_bar(data = ESP[(ESP$sexo=="Hombres\nExtranjero"|
                          ESP$sexo=="Mujeres\nExtranjero"),], 
            colour = I("Black"),stat="identity", position="identity", size=.3, colour="black",
            aes(x = edad, y = nREL,fill = sexo), alpha = .5)+
           coord_flip()+
 # facet_grid(. ~ TITOL)+
  scale_y_continuous(limits=c(-2.5,2.5),breaks = c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2, 2.5), 
                     labels = paste0(as.character(c(seq(2.5, 0, -0.5), seq(0.5, 2.5, 0.5))), "%")) + 
           scale_x_continuous(breaks=seq(0,110,5)) +
           scale_fill_manual(values = c("#424242", "#BDBDBD","#585858", "#D8D8D8"))+
    annotate("text", x = 107.5, y = -2.5*.25, label = "Hombres",size=5)+
  annotate("text", x = 107.5, y = 2.5*0.25, label = "Mujeres",size=5)+
  annotate("rect", xmin = 101, xmax = 104, ymin = -2.5*.90, ymax =-2.5*.80, alpha=1, fill="#424242")+
  annotate("text", x = 102.5, y = -1.40, label = "España",size=5)+
  annotate("rect", xmin = 91, xmax = 94,  ymin = -2.5*.90, ymax =-2.5*.80,alpha=1, fill="#BDBDBD")+
  annotate("text", x = 92.5,  y = -1.30, label = "Extranjeros",size=5)+ 
  annotate("rect", xmin = 101, xmax = 104,  ymin = 2.5*.90, ymax =2.5*.80, alpha=1, fill="#585858")+
  annotate("text", x = 102.5, y = 1.40, label = "España",size=5)+
  annotate("rect", xmin = 91, xmax = 94, ymin = 2.5*.90, ymax =2.5*.80, alpha=1, fill="#D8D8D8")+
  annotate("text", x = 92.5,  y = 1.30, label = "Extranjeros",size=5)+ 
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP1_FIG3_PIRAMIDE_VALENCIA_RELATIVOS_2014.tiff", scale = 3, width = 6.3, height = 4.7, units = c("cm"), dpi = 300)
```

<br>

####**Municipios de las comarcas de Alt y Baix Marina y Baix Segura (Alicante)[Pirámide compuesta en términos absolutos]**

```{r fig.width=6.3, fig.height=4.7}
AMB_ENC<-  AMB_EDADES_2014[AMB_EDADES_2014$ID %in% ENCLAVES,]    
b <- AMB_ENC %>%group_by(REGION_NAC,sexo,edad) %>% tally()
ages <- data.frame(c(0:110))
colnames(ages)<- "edad"
HOMESP <-b[(b$REGION_NAC==1 & b$sexo==1),]
HOMESP<-merge(HOMESP,ages, by="edad",all = TRUE)
HOMESP$REGION_NAC<- 1
HOMESP$sexo<- "Hombres\nEspaña"
HOMESP[is.na(HOMESP)]<- 0
MUJESP <-b[(b$REGION_NAC==1 & b$sexo==6),]
MUJESP<-merge(MUJESP,ages, by="edad",all = TRUE)
MUJESP$REGION_NAC<- 1
MUJESP$sexo<- "Mujeres\nEspaña"
MUJESP[is.na(MUJESP)]<- 0
HOMLA <-b[(b$REGION_NAC==2 & b$sexo==1),]
HOMLA<-merge(HOMLA,ages, by="edad",all = TRUE)
HOMLA$REGION_NAC<- 2
HOMLA$sexo<- "Hombres\nLatinoamerica"
HOMLA[is.na(HOMLA)]<- 0
MUJLA <-b[(b$REGION_NAC==2 & b$sexo==6),]
MUJLA<-merge(MUJLA,ages, by="edad",all = TRUE)
MUJLA$REGION_NAC<- 2
MUJLA$sexo<- "Mujeres\nLatinoamerica"
MUJLA[is.na(MUJLA)]<- 0
HOMWE <-b[(b$REGION_NAC==3 & b$sexo==1),]
HOMWE<-merge(HOMWE,ages, by="edad",all = TRUE)
HOMWE$REGION_NAC<- 3
HOMWE<-HOMWE[1:111,]
HOMWE$sexo<- "Hombres\nEuropa_Occ."
HOMWE[is.na(HOMWE)]<- 0
MUJWE <-b[(b$REGION_NAC==3 & b$sexo==6),]
MUJWE<-merge(MUJWE,ages, by="edad",all = TRUE)
MUJWE$REGION_NAC<- 3
MUJWE$sexo<- "Mujeres\nEuropa_Occ."
MUJWE[is.na(MUJWE)]<- 0
HOMEE <-b[(b$REGION_NAC==4 & b$sexo==1),]
HOMEE<-merge(HOMEE,ages, by="edad",all = TRUE)
HOMEE$REGION_NAC<- 4
HOMEE$sexo<- "Hombres\nEuropa_Or."
HOMEE[is.na(HOMEE)]<- 0
MUJEE <-b[(b$REGION_NAC==4 & b$sexo==6),]
MUJEE<-merge(MUJEE,ages, by="edad",all = TRUE)
MUJEE$REGION_NAC<- 4
MUJEE$sexo<- "Mujeres\nEuropa_Or."
MUJEE[is.na(MUJEE)]<- 0
HOMAF <-b[(b$REGION_NAC==5 & b$sexo==1),]
HOMAF<-merge(HOMAF,ages, by="edad",all = TRUE)
HOMAF$REGION_NAC<- 5
HOMAF$sexo<- "Hombres\nAfrica"
HOMAF[is.na(HOMAF)]<- 0
 
MUJAF <-b[(b$REGION_NAC==5 & b$sexo==6),]
MUJAF<-merge(MUJAF,ages, by="edad",all = TRUE)
MUJAF$REGION_NAC<- 5
MUJAF$sexo<- "Mujeres\nAfrica"
MUJAF[is.na(MUJAF)]<- 0
HOMAS <-b[(b$REGION_NAC==6 & b$sexo==1),]
HOMAS<-merge(HOMAS,ages, by="edad",all = TRUE)
HOMAS$REGION_NAC<- 6
HOMAS$sexo<- "Hombres\nAsia"
HOMAS[is.na(HOMAS)]<- 0
MUJAS <-b[(b$REGION_NAC==6 & b$sexo==6),]
MUJAS<-merge(MUJAS,ages, by="edad",all = TRUE)
MUJAS$REGION_NAC<- 6
MUJAS$sexo<- "Mujeres\nAsia"
MUJAS[is.na(MUJAS)]<- 0
HOMOT <-b[(b$REGION_NAC==0 & b$sexo==1),]
HOMOT<-merge(HOMOT,ages, by="edad",all = TRUE)
HOMOT$REGION_NAC<- 0
HOMOT$sexo<- "Hombres\nOtros"
HOMOT[is.na(HOMOT)]<- 0
MUJOT <-b[(b$REGION_NAC==0 & b$sexo==6),]
MUJOT<-merge(MUJOT,ages, by="edad",all = TRUE)
MUJOT$REGION_NAC<- 0
MUJOT$sexo<- "Mujeres\nOtros"
MUJOT[is.na(MUJOT)]<- 0
HOMESP$n <- HOMESP$n*-1
HOMLA$n <- HOMLA$n*-1
HOMWE$n <- HOMWE$n*-1
HOMEE$n <- HOMEE$n*-1
HOMAF$n <- HOMAF$n*-1
HOMAS$n <- HOMAS$n*-1
HOMOT$n <- HOMOT$n*-1
ESP <- rbind(HOMESP,MUJESP,HOMLA,MUJLA,HOMWE, MUJWE, HOMEE,MUJEE,HOMAF,MUJAF,
             HOMAS,MUJAS,HOMOT,MUJOT)
ESP$SEXO2 <- substr(ESP$sexo, 9,25)
ESP$SEXO2<- factor(ESP$SEXO2,
                   levels = c("España", "Latinoamerica", "Europa_Occ.", "Europa_Or.", "Africa", "Asia", "Otros"))
A<-ggplot(ESP, aes(x=edad, y=n, fill=sexo))+
  geom_bar(data = ESP[(ESP$sexo=="Mujeres\nEspaña"|
                                         ESP$sexo=="Mujeres\nLatinoamerica"|
                                         ESP$sexo=="Mujeres\nEuropa_Occ."|
                                         ESP$sexo=="Mujeres\nEuropa_Or."|
                                         ESP$sexo=="Mujeres\nAfrica"|
                                         ESP$sexo=="Mujeres\nAsia"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = edad, y = n,fill = SEXO2), alpha = 1)+
  geom_bar(data = ESP[(ESP$sexo=="Hombres\nEspaña"|
                                           ESP$sexo=="Hombres\nLatinoamerica"|
                                           ESP$sexo=="Hombres\nEuropa_Occ."|
                                           ESP$sexo=="Hombres\nEuropa_Or."|
                                           ESP$sexo=="Hombres\nAfrica"|
                                           ESP$sexo=="Hombres\nAsia"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = edad, y = n,fill = SEXO2), alpha = 1)+
  coord_flip()+
  scale_y_continuous(limits=c(-3000,3000),breaks = c(-3000,-2500, -2000, -1500,-1000,-500, 0,500,1000,1500,2000,2500, 3000), 
                     labels = paste0(as.character(c(seq(3000, 0, -500), seq(500, 3000, 500))), "")) + 
  scale_x_continuous(breaks=seq(0,110,5)) +
  annotate("text", x = 107.5, y = -3000*0.25, label = "Hombres",size=5)+
  annotate("text", x = 107.5, y = 3000*0.25, label = "Mujeres",size=5)+
  
  scale_x_continuous(breaks=seq(0,110,5)) +
  scale_fill_manual(values = c("#F2F2F2","#D8D8D8","#A4A4A4","#6E6E6E",
                               "#424242","#1C1C1C"))+
   theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=10),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP1_FIG3_PIRAMIDE_VALENCIA_ABSOLUTOS_2014.tiff", scale = 3, width = 6.3, height = 4.7, units = c("cm"), dpi = 300)
```

<br>

####**Arona y Adeje (Canarias)[Pirámide superpuesta en términos relativos]**

```{r fig.width=6.3, fig.height=4.7}
library(ggplot2)
library(RColorBrewer)
require(dplyr)
library(scales)
AMB_EDADES_2014 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAN_EDADES_2014.csv"), sep=";", stringsAsFactors=FALSE)
AMB_EDADES_2014$provincia <- sprintf("%.2d",AMB_EDADES_2014$provincia) # prov tiene 2 digitos
AMB_EDADES_2014$municipio <- sprintf("%.5d",AMB_EDADES_2014$municipio)  # mun tiene 3 digitos
AMB_EDADES_2014$distrito  <- sprintf("%.2d",AMB_EDADES_2014$distrito) # dist tiene 2
AMB_EDADES_2014$seccion   <- sprintf("%.3d",AMB_EDADES_2014$seccion) # secc tiene 3
# concatenar
AMB_EDADES_2014$ID   <- with(AMB_EDADES_2014, paste(provincia, substr(municipio, 3,5), distrito, seccion, sep = "-"))
AMB_EDADES_2014$munnac <-NULL
AMB_EDADES_2014$nf <- ifelse(AMB_EDADES_2014$pronac < 66, "108",0)
AMB_EDADES_2014$REGION_NAC <- ifelse(AMB_EDADES_2014$nf == 108, "1",
                         ifelse(AMB_EDADES_2014$paisnac > 302 & AMB_EDADES_2014$paisnac < 400, "2",
                                ifelse(AMB_EDADES_2014$paisnac > 101 & AMB_EDADES_2014$paisnac < 104 | 
                                         AMB_EDADES_2014$paisnac == 107 | 
                                         AMB_EDADES_2014$paisnac > 108 & AMB_EDADES_2014$paisnac < 112 | 
                                         AMB_EDADES_2014$paisnac > 112 & AMB_EDADES_2014$paisnac < 122 | 
                                         AMB_EDADES_2014$paisnac > 122 & AMB_EDADES_2014$paisnac < 127 | 
                                         AMB_EDADES_2014$paisnac > 128 & AMB_EDADES_2014$paisnac < 133, "3",
                                       ifelse(AMB_EDADES_2014$paisnac == 101 | 
                                                AMB_EDADES_2014$paisnac == 112 | 
                                                AMB_EDADES_2014$paisnac == 122 |
                                                AMB_EDADES_2014$paisnac == 128 |
                                                AMB_EDADES_2014$paisnac > 103 & AMB_EDADES_2014$paisnac < 107 | 
                                                AMB_EDADES_2014$paisnac > 134 & AMB_EDADES_2014$paisnac < 200, "4",
                                              ifelse(AMB_EDADES_2014$paisnac > 200 & AMB_EDADES_2014$paisnac < 300, "5",
                                                     ifelse(AMB_EDADES_2014$paisnac > 400 & AMB_EDADES_2014$paisnac < 500, "6",
                                                            0))))))
AMB_EDADES_2014$nf<- NULL
AMB_EDADES_2014$REGION_NAC2 <- ifelse(AMB_EDADES_2014$REGION_NAC == "1", "ESP","EXT")
ENCLAVES <- c("38-001-01-003","38-001-01-007","38-001-01-008","38-001-01-011",
              "38-001-01-013","38-001-01-017","38-001-01-018","38-006-01-005",
              "38-006-01-006","38-006-01-007","38-006-01-011","38-006-01-014",
              "38-006-01-015","38-006-01-016","38-006-01-018","38-006-01-019",
              "38-006-01-020","38-006-01-023","38-006-01-026","38-006-01-029",
              "38-006-01-031")
AMB_ENC<-  AMB_EDADES_2014[AMB_EDADES_2014$ID %in% ENCLAVES,]    
b <- AMB_ENC %>%group_by(REGION_NAC2,sexo,edad) %>% tally()
b <- b[order(-b$n),] 
ages <- data.frame(c(0:110))
colnames(ages)<- "edad"
HOMESP <-b[(b$REGION_NAC2=="ESP" & b$sexo==1),]
HOMESP<-merge(HOMESP,ages, by="edad",all = TRUE)
HOMESP$REGION_NAC2<- "ESP"
HOMESP$sexo<- "Hombres\nEspaña"
HOMESP[is.na(HOMESP)]<- 0
MUJESP <-b[(b$REGION_NAC2=="ESP" & b$sexo==6),]
MUJESP<-merge(MUJESP,ages, by="edad",all = TRUE)
MUJESP$REGION_NAC2<- "ESP"
MUJESP$sexo<- "Mujeres\nEspaña"
MUJESP[is.na(MUJESP)]<- 0
HOMEXT <-b[(b$REGION_NAC2=="EXT" & b$sexo==1),]
HOMEXT<-merge(HOMEXT,ages, by="edad",all = TRUE)
HOMEXT$REGION_NAC2<- "EXT"
HOMEXT$sexo<- "Hombres\nExtranjero"
HOMEXT[is.na(HOMEXT)]<- 0
MUJEXT <-b[(b$REGION_NAC2=="EXT" & b$sexo==6),]
MUJEXT<-merge(MUJEXT,ages, by="edad",all = TRUE)
MUJEXT$REGION_NAC2<- "EXT"
MUJEXT$sexo<- "Mujeres\nExtranjero"
MUJEXT[is.na(MUJEXT)]<- 0
HOMESP$nREL <- ((HOMESP$n/(sum(HOMESP$n)+sum(MUJESP$n)))*100)*-1
MUJESP$nREL <- MUJESP$n/(sum(HOMESP$n)+sum(MUJESP$n))*100
HOMEXT$nREL <- ((HOMEXT$n/(sum(HOMEXT$n)+sum(MUJEXT$n)))*100)*-1
MUJEXT$nREL <- MUJEXT$n/(sum(HOMEXT$n)+sum(MUJEXT$n))*100
ESP <- rbind(HOMESP,MUJESP,HOMEXT,MUJEXT)
A<- ggplot(ESP, aes(x=edad, y=nREL, fill=sexo))+
   geom_bar(data = ESP[(ESP$sexo=="Mujeres\nEspaña"|
                   ESP$sexo=="Hombres\nEspaña"),], 
                   colour = I("Black"),stat="identity", position="identity", size=.3, colour="black",
                   aes(x = edad, y = nREL,fill = sexo), alpha = 1)+
   geom_bar(data = ESP[(ESP$sexo=="Hombres\nExtranjero"|
                          ESP$sexo=="Mujeres\nExtranjero"),], 
            colour = I("Black"),stat="identity", position="identity", size=.3, colour="black",
            aes(x = edad, y = nREL,fill = sexo), alpha = .5)+
           coord_flip()+
  scale_y_continuous(limits=c(-2.5,2.5),breaks = c(-2.5,-2,-1.5,-1,-0.5,0,0.5,1,1.5,2, 2.5), 
                     labels = paste0(as.character(c(seq(2.5, 0, -0.5), seq(0.5, 2.5, 0.5))), "%")) + 
           scale_x_continuous(breaks=seq(0,110,5)) +
           scale_fill_manual(values = c("#424242", "#BDBDBD","#585858", "#D8D8D8"))+
  annotate("text", x = 107.5, y = -2.5*.25, label = "Hombres",size=5)+
  annotate("text", x = 107.5, y = 2.5*0.25, label = "Mujeres",size=5)+
  annotate("rect", xmin = 101, xmax = 104, ymin = -2.5*.90, ymax =-2.5*.80, alpha=1, fill="#424242")+
  annotate("text", x = 102.5, y = -1.40, label = "España",size=5)+
  annotate("rect", xmin = 91, xmax = 94,  ymin = -2.5*.90, ymax =-2.5*.80,alpha=1, fill="#BDBDBD")+
  annotate("text", x = 92.5,  y = -1.30, label = "Extranjeros",size=5)+ 
  annotate("rect", xmin = 101, xmax = 104,  ymin = 2.5*.90, ymax =2.5*.80, alpha=1, fill="#585858")+
  annotate("text", x = 102.5, y = 1.40, label = "España",size=5)+
  annotate("rect", xmin = 91, xmax = 94, ymin = 2.5*.90, ymax =2.5*.80, alpha=1, fill="#D8D8D8")+
  annotate("text", x = 92.5,  y = 1.30, label = "Extranjeros",size=5)+ 
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP1_FIG3_PIRAMIDE_CANARIAS_RELATIVOS_2014.tiff", scale = 3, width = 6.3, height = 4.7, units = c("cm"), dpi = 300)
```

<br>

####**Arona y Adeje (Canarias)[Pirámide compuesta en términos absolutos]**

```{r fig.width=6.3, fig.height=5.7}
AMB_ENC<-  AMB_EDADES_2014[AMB_EDADES_2014$ID %in% ENCLAVES,]    
b <- AMB_ENC %>%group_by(REGION_NAC,sexo,edad) %>% tally()
ages <- data.frame(c(0:110))
colnames(ages)<- "edad"
HOMESP <-b[(b$REGION_NAC==1 & b$sexo==1),]
HOMESP<-merge(HOMESP,ages, by="edad",all = TRUE)
HOMESP$REGION_NAC<- 1
HOMESP$sexo<- "Hombres\nEspaña"
HOMESP[is.na(HOMESP)]<- 0
MUJESP <-b[(b$REGION_NAC==1 & b$sexo==6),]
MUJESP<-merge(MUJESP,ages, by="edad",all = TRUE)
MUJESP$REGION_NAC<- 1
MUJESP$sexo<- "Mujeres\nEspaña"
MUJESP[is.na(MUJESP)]<- 0
HOMLA <-b[(b$REGION_NAC==2 & b$sexo==1),]
HOMLA<-merge(HOMLA,ages, by="edad",all = TRUE)
HOMLA$REGION_NAC<- 2
HOMLA$sexo<- "Hombres\nLatinoamerica"
HOMLA[is.na(HOMLA)]<- 0
MUJLA <-b[(b$REGION_NAC==2 & b$sexo==6),]
MUJLA<-merge(MUJLA,ages, by="edad",all = TRUE)
MUJLA$REGION_NAC<- 2
MUJLA$sexo<- "Mujeres\nLatinoamerica"
MUJLA[is.na(MUJLA)]<- 0
HOMWE <-b[(b$REGION_NAC==3 & b$sexo==1),]
HOMWE<-merge(HOMWE,ages, by="edad",all = TRUE)
HOMWE$REGION_NAC<- 3
HOMWE<-HOMWE[1:111,]
HOMWE$sexo<- "Hombres\nEuropa_Occ."
HOMWE[is.na(HOMWE)]<- 0
MUJWE <-b[(b$REGION_NAC==3 & b$sexo==6),]
MUJWE<-merge(MUJWE,ages, by="edad",all = TRUE)
MUJWE$REGION_NAC<- 3
MUJWE$sexo<- "Mujeres\nEuropa_Occ."
MUJWE[is.na(MUJWE)]<- 0
HOMEE <-b[(b$REGION_NAC==4 & b$sexo==1),]
HOMEE<-merge(HOMEE,ages, by="edad",all = TRUE)
HOMEE$REGION_NAC<- 4
HOMEE$sexo<- "Hombres\nEuropa_Or."
HOMEE[is.na(HOMEE)]<- 0
MUJEE <-b[(b$REGION_NAC==4 & b$sexo==6),]
MUJEE<-merge(MUJEE,ages, by="edad",all = TRUE)
MUJEE$REGION_NAC<- 4
MUJEE$sexo<- "Mujeres\nEuropa_Or."
MUJEE[is.na(MUJEE)]<- 0
HOMAF <-b[(b$REGION_NAC==5 & b$sexo==1),]
HOMAF<-merge(HOMAF,ages, by="edad",all = TRUE)
HOMAF$REGION_NAC<- 5
HOMAF$sexo<- "Hombres\nAfrica"
HOMAF[is.na(HOMAF)]<- 0
MUJAF <-b[(b$REGION_NAC==5 & b$sexo==6),]
MUJAF<-merge(MUJAF,ages, by="edad",all = TRUE)
MUJAF$REGION_NAC<- 5
MUJAF$sexo<- "Mujeres\nAfrica"
MUJAF[is.na(MUJAF)]<- 0
HOMAS <-b[(b$REGION_NAC==6 & b$sexo==1),]
HOMAS<-merge(HOMAS,ages, by="edad",all = TRUE)
HOMAS$REGION_NAC<- 6
HOMAS$sexo<- "Hombres\nAsia"
HOMAS[is.na(HOMAS)]<- 0
MUJAS <-b[(b$REGION_NAC==6 & b$sexo==6),]
MUJAS<-merge(MUJAS,ages, by="edad",all = TRUE)
MUJAS$REGION_NAC<- 6
MUJAS$sexo<- "Mujeres\nAsia"
MUJAS[is.na(MUJAS)]<- 0
HOMOT <-b[(b$REGION_NAC==0 & b$sexo==1),]
HOMOT<-merge(HOMOT,ages, by="edad",all = TRUE)
HOMOT$REGION_NAC<- 0
HOMOT$sexo<- "Hombres\nOtros"
HOMOT[is.na(HOMOT)]<- 0
MUJOT <-b[(b$REGION_NAC==0 & b$sexo==6),]
MUJOT<-merge(MUJOT,ages, by="edad",all = TRUE)
MUJOT$REGION_NAC<- 0
MUJOT$sexo<- "Mujeres\nOtros"
MUJOT[is.na(MUJOT)]<- 0
HOMESP$n <- HOMESP$n*-1
HOMLA$n <- HOMLA$n*-1
HOMWE$n <- HOMWE$n*-1
HOMEE$n <- HOMEE$n*-1
HOMAF$n <- HOMAF$n*-1
HOMAS$n <- HOMAS$n*-1
HOMOT$n <- HOMOT$n*-1
ESP <- rbind(HOMESP,MUJESP,HOMLA,MUJLA,HOMWE, MUJWE, HOMEE,MUJEE,HOMAF,MUJAF,
             HOMAS,MUJAS,HOMOT,MUJOT)
ESP$SEXO2 <- substr(ESP$sexo, 9,25)
ESP$SEXO2<- factor(ESP$SEXO2,
                   levels = c("España", "Latinoamerica", "Europa_Occ.", "Europa_Or.", "Africa", "Asia", "Otros"))
A<-ggplot(ESP, aes(x=edad, y=n, fill=sexo))+
  geom_bar(data = ESP[(ESP$sexo=="Mujeres\nEspaña"|
                                         ESP$sexo=="Mujeres\nLatinoamerica"|
                                         ESP$sexo=="Mujeres\nEuropa_Occ."|
                                         ESP$sexo=="Mujeres\nEuropa_Or."|
                                         ESP$sexo=="Mujeres\nAfrica"|
                                         ESP$sexo=="Mujeres\nAsia"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = edad, y = n,fill = SEXO2), alpha = 1)+
  geom_bar(data = ESP[(ESP$sexo=="Hombres\nEspaña"|
                                           ESP$sexo=="Hombres\nLatinoamerica"|
                                           ESP$sexo=="Hombres\nEuropa_Occ."|
                                           ESP$sexo=="Hombres\nEuropa_Or."|
                                           ESP$sexo=="Hombres\nAfrica"|
                                           ESP$sexo=="Hombres\nAsia"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = edad, y = n,fill = SEXO2), alpha = 1)+
  coord_flip()+
  scale_y_continuous(limits=c(-900,900),breaks = c(-900, -600, -300,0,300,600,900), 
                     labels = paste0(as.character(c(seq(900, 0, -300), seq(300, 900, 300))), "")) + 
 scale_x_continuous(breaks=seq(0,110,5)) +
  annotate("text", x = 107.5, y = -900*0.25, label = "Hombres",size=5)+
  annotate("text", x = 107.5, y = 900*0.25, label = "Mujeres",size=5)+
  scale_fill_manual(values = c("#F2F2F2","#D8D8D8","#A4A4A4","#6E6E6E",
                               "#424242","#1C1C1C"))+
   theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="bottom",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
         legend.direction = "horizontal", 
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=10),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP1_FIG3_PIRAMIDE_ABSOLUTOS_LEYENDA.tiff", scale = 3, width = 6.3, height = 4.7, units = c("cm"), dpi = 300)
```

<br>

### **<span style="color:red">Capítulo 2: LA DIVERSIDAD EN LAS METRÓPOLIS ESPAÑOLAS [Autores: Juan Galeano & Jordi Bayona i Carrasco]</span>**  

<br>

#### **Figura 2.2: Evolución de la diversidad en las regiones metropolitanas, 2004-2013**  

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP2_GRAFIC2 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_2_GRAFIC2.csv"), stringsAsFactors=FALSE)
CAP2_GRAFIC2$RMB <- factor(CAP2_GRAFIC2$RMB, levels = c("Madrid", "Barcelona", "Málaga", "Valencia", "Bilbao", "Sevilla")) 
CAP2_GRAFIC2$CAT <- factor(CAP2_GRAFIC2$CAT, levels = c("Proporción de secciones censales de alta y muy alta diversidad","Proporción de población inmigrante en secciones de alta y muy alta diversidad"))
levels(CAP2_GRAFIC2$CAT)<- c("Proporción de secciones censales\nde alta\ny muy alta diversidad","Proporción de población inmigrante\nen secciones de alta\ny muy alta diversidad")
A<- ggplot(CAP2_GRAFIC2, aes(x = factor(YEAR), y = PROP, fill=factor(YEAR))) + geom_bar(stat = "identity",position = "dodge",colour="black")+#coord_flip()+
  facet_grid(CAT~RMB)+
  expand_limits(y=c(0,0.8))+
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8),labels=percent)+
    scale_fill_manual(name="",
                     values=c("#A4A4A4", "#6E6E6E", "#424242"),
                     breaks=c("2004","2009","2013"),
                     labels=c("2004","2009","2013"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
                 legend.position = "none",
              legend.background = element_rect(fill=NA),
              #legend.justification=c(6,2),
              legend.direction = "horizontal", 
              legend.key=element_rect(size=10),
              legend.key.size = unit(1.5, "lines"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=12,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP2_FIG2.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
``` 

<br>

#### **Figura 2.3: Composición de la población metropolitana según la diversidad de la sección de residencia, 2014**  

```{r fig.width=16.5, fig.height=8.8}
library(ggplot2)
library(scales)
COMPIDVERSIDAD <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/COMPDIVERSIDAD.csv"), stringsAsFactors=FALSE) 
COMPIDVERSIDAD$CATDIVERSITY2 <- factor(COMPIDVERSIDAD$CATDIVERSITY2 ,
                                levels = c("Baja", "Media", "Alta+Muy Alta"))
COMPIDVERSIDAD$CONT <- factor(COMPIDVERSIDAD$CONT ,
                                       levels = c("España", "Latinoamérica", "Europa Occidental", 
                                                  "Europa Oriental", "África", "Asia"))
COMPIDVERSIDAD$AREA <- factor(COMPIDVERSIDAD$AREA ,
                       levels = c("Madrid","Barcelona",
                                  "Málaga",
                                  "Valencia",
                                  "Bilbao",
                                  "Sevilla"))
COMPIDVERSIDAD$secciones2 <- COMPIDVERSIDAD$secciones2 *1.5
COMPIDVERSIDAD<-COMPIDVERSIDAD[!COMPIDVERSIDAD$CONT=="España", ]
A<-ggplot(data=COMPIDVERSIDAD, aes(x=CATDIVERSITY2, y=PROP, fill=factor(CONT),width=secciones2,label=round(PROP*100, digits =0))) + 
  geom_bar(stat="identity", position=position_dodge(), colour="black")+
  coord_flip()+
  geom_text(hjust=-0.15,size=5.5,position = position_dodge(width=1))+
  facet_grid(AREA~CONT)+
  scale_y_continuous(limits=c(0, .2),labels = percent, oob = rescale_none)+
  scale_fill_manual(name="",
                     values=c("#D8D8D8", "#A4A4A4", "#6E6E6E", "#424242", "#1C1C1C"),
                     breaks=c("Latinoamérica","Europa Occidental","Europa Oriental","África","Asia"),
                     labels=c("Latinoamérica","Europa Occidental","Europa Oriental","África","Asia"))+
  guides(fill = guide_legend(reverse=TRUE))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 90,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=15,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP2_FIG3_POBLACION_SEGUN_DICERSIDAD_AREA_2014.tiff", scale = 3, width = 16.5, height = 8.8, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 2.4: Distribución relativa de la población metropolitana según la diversidad de la sección de residencia, 2014** 

```{r fig.width=16.5, fig.height=8.8}
library(scales)
COMPDIVERSIDAD <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/DIVERSIDADAREAS2.csv"), stringsAsFactors=FALSE)
COMPDIVERSIDAD$CATDIVERSITY2 <- factor(COMPDIVERSIDAD$CATDIVERSITY2 ,
                                       levels = c("Alta+Muy Alta", "Media", "Baja"))
COMPDIVERSIDAD$CONT <- factor(COMPDIVERSIDAD$CONT ,
                              levels = c("España", "Latinoamérica", "Europa Occidental", 
                                         "Europa Oriental", "África", "Asia"))
COMPDIVERSIDAD$AREA <- factor(COMPDIVERSIDAD$AREA ,
                              levels = c("Madrid","Barcelona",
                                         "Málaga",
                                         "Valencia",
                                         "Bilbao",
                                         "Sevilla"))
A<-ggplot(data=COMPDIVERSIDAD, aes(x=CONT, y=PROP, fill=CONT,label=round(PROP*100, digits =0)))+ 
  geom_bar(stat="identity", colour="black")+
  scale_y_continuous(limits=c(0, 1),labels = percent)+
   geom_text(hjust=-0.15,size=5.5,position = position_dodge(width=1))+
  coord_flip()+
  facet_grid(AREA~CATDIVERSITY2)+
  scale_fill_manual(name="",
                     values=c("#F2F2F2", "#D8D8D8", "#A4A4A4", "#6E6E6E", "#424242", "#1C1C1C"),
                     breaks=c("España","Latinoamérica","Europa Occidental","Europa Oriental","África","Asia"),
                     labels=c("España","Latinoamérica","Europa Occidental","Europa Oriental","África","Asia"))+
    theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 90,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=18,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP2_FIG4_POBLACION_EN_DICERSIDAD_AREA_2014.tiff", scale = 3, width = 16.5, height = 8.8, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 2.5: Figura 5 Evolución de la diversidad en las regiones metropolitanas, 2000-2014.** 

```{r fig.width=16.5, fig.height=8.8}
load(url("http://gedemced.uab.cat/DATOS_ICARIA/DATOS_ICARIA_3.Rdata"))
dffinal$AREA <- factor(dffinal$AREA ,
                   levels = c("Madrid","Barcelona",
                              "Málaga",
                              "Valencia",
                              "Bilbao",
                              "Sevilla"))
dffinal$CATDIVERSITY2 <- factor(dffinal$CATDIVERSITY2 ,
                   levels = c("Baja", "Media", "Alta", "Muy Alta"))
POP <- dffinal %>% group_by(AREA,YEAR) %>%  summarise(secciones = n(), 
                                                 Spanish = sum(Spanish), 
                                                 LatinaAmerica = sum(LatinAmerica), 
                                                 WesternEurope = sum(WesternEurope), 
                                                 EasternEurope = sum(EasternEurope), 
                                                 Africa = sum(Africa), 
                                                 Asia = sum(Asia), 
                                                 Others = sum(Others),
                                                 Total_ext=sum(Total_ext),
                                                 Totalpop=sum(Totalpop))
POP$Totalpop2 <-with(POP, Totalpop-Others)
POP$DIVERSITY <- with(POP, (1/(((Spanish/Totalpop2)^2)+
                    ((LatinaAmerica/Totalpop2)^2)+
                    ((WesternEurope/Totalpop2)^2)+
                    ((EasternEurope/Totalpop2)^2)+
                    ((Africa/Totalpop2)^2)+
                    ((Asia/Totalpop2)^2))))
p <- ggplot(dffinal, aes(YEAR, DIVERSITY))
A<- p + geom_jitter(aes(colour = CATDIVERSITY2),width = .75,size=1)+
  scale_y_continuous(limits = c(1, 6))+ 
  facet_wrap( ~ AREA, ncol=2)+
  scale_x_continuous(breaks=seq(2000,2014, 7))+
  scale_colour_manual(name="",
                      values=c("#D8D8D8", "#A4A4A4", "#6E6E6E","#424242"),
                      breaks=c("Baja", "Media", "Alta","Muy Alta"),
                      labels=c("Baja", "Media", "Alta","Muy Alta"))+
  geom_text(data=POP, size=5,aes(x=YEAR, y= DIVERSITY+2.75, 
                                   label=paste(round(DIVERSITY, 2),sep="" ), 
                                   inherit.aes=FALSE, parse=FALSE))+
  
    theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="bottom",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=18,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP2_FIG5_DIVERSIDAD_SECCIONES_AREA_2000_2014.tiff", scale = 3, width = 16.5, height = 8.8, units = c("cm"), dpi = 300)
```

<br>

### **<span style="color:red">Capítulo 5: LA TENTACIÓN PIGMENTOCRÁTICA. FISURAS EN EL DISCURSO INTERCULTURAL [Autores: Andreu Domingo & Xiana Bueno]</span>**  

<br>

#### **Figura 5.1: Percepciones sobre la integración de los diferentes grupos de origen, Técnicos municipales de inmigración, Cataluña, Madrid y Andalucía, 2013** 

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP5_GRAFIC1 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_5_GRAFIC1.csv"), stringsAsFactors=FALSE)
CAP5_GRAFIC1$CAT <- factor(CAP5_GRAFIC1$CAT,  levels = c("Muy fácil","Fácil","Igual","Difícil","Muy difícil"))
CAP5_GRAFIC1$GROUP<-factor(CAP5_GRAFIC1$GROUP, levels=subset(CAP5_GRAFIC1, CAT  == "Muy fácil")[order(subset(CAP5_GRAFIC1, CAT  == "Muy fácil")$PROP),3])
A<- ggplot(data=CAP5_GRAFIC1, aes(x=GROUP, y=PROP, fill=CAT, label=round(PROP*100, digits =2))) + geom_bar(stat="identity",colour="black")+
  coord_flip()+geom_text(hjust=-0.15, size=3)+
  scale_y_continuous(breaks=c(0,0.2,0.4,0.6,0.8,1),labels=percent)+
  expand_limits(y=c(0,1))+
  facet_wrap(~ CAT, nrow=1)+
  scale_fill_manual(name="",
                     values=rev(c("#2E2E2E", "#585858", "#848484", "#A4A4A4", "#BDBDBD")),
                     breaks=rev(c("Muy fácil","Fácil","Igual","Difícil","Muy difícil")),
                     labels=rev(c("Muy fácil","Fácil","Igual","Difícil","Muy difícil")))+ 
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position = "none",
              legend.background = element_rect(fill=NA),
              #legend.justification=c(6,2),
              #legend.direction = "vertical", 
              #legend.key=element_rect(size=10),
              legend.key.size = unit(1.5, "lines"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 90,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=12,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP5_FIG1.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

### **<span style="color:red">Capítulo 6: INMIGRACIÓN INTERNACIONAL Y DIVERSIFICACIÓN DE LOS HOGARES [Autores: Rocío Treviño & Pau Miret Gamundi]</span>**  

<br>

#### **Figura 6.1: Distribución de los hogares y de la población por tipo de hogar, población nacida en España y población nacida en el extranjero, 2011** 

```{r fig.width=16.5, fig.height=8.8}
CAP6_GRAFIC1 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC1.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC1$HOGAR2 <- rep(c("Unipersonales", 
                             "Pareja sin hijos\n(sin otras personas)",
                             "Pareja con hijos\n(sin otras personas)",
                             "Monoparentales\n(sin otras personas)", 
                             "Pareja sin hijos\n(con otras personas)", 
                             "Pareja con hijos\n(con otras personas)",
                             "Monoparentales\n(con otras personas)",
                             "Dos o + núcleos\nde pareja o filiación", 
                             "Sin lazos\npareja o filiación"),4) 
CAP6_GRAFIC1$HOGAR2<-as.factor(CAP6_GRAFIC1$HOGAR2) 
CAP6_GRAFIC1$HOGAR2 <- factor(CAP6_GRAFIC1$HOGAR2,
                       levels = c("Unipersonales", 
                             "Pareja sin hijos\n(sin otras personas)",
                             "Pareja con hijos\n(sin otras personas)",
                             "Monoparentales\n(sin otras personas)", 
                             "Pareja sin hijos\n(con otras personas)", 
                             "Pareja con hijos\n(con otras personas)",
                             "Monoparentales\n(con otras personas)",
                             "Dos o + núcleos\nde pareja o filiación", 
                             "Sin lazos\npareja o filiación"))
levels(CAP6_GRAFIC1$HOGAR2)
CAP6_GRAFIC1$TIPO <- factor(CAP6_GRAFIC1$TIPO,
                   levels = c("Población","Hogares"))
CAP6_GRAFIC1$POP <- factor(CAP6_GRAFIC1$POP,
                   levels = c("Nativos","Inmigrantes"))
A<-ggplot(data=CAP6_GRAFIC1, aes(x=POP, y=PROP, fill=HOGAR2, label=round(PROP*100, digits =2))) + 
  geom_bar(stat="identity",colour="black")+
  scale_y_continuous(breaks=c(0,.2,.4,.6,.8,1),labels=percent)+
  expand_limits(y=c(0,1))+
  facet_grid(~TIPO)+
    scale_fill_manual(name="",
                     guide = guide_legend(reverse=TRUE),
                      values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"),
                     breaks=c("Unipersonales", 
                             "Pareja sin hijos\n(sin otras personas)",
                             "Pareja con hijos\n(sin otras personas)",
                             "Monoparentales\n(sin otras personas)", 
                             "Pareja sin hijos\n(con otras personas)", 
                             "Pareja con hijos\n(con otras personas)",
                             "Monoparentales\n(con otras personas)",
                             "Dos o + núcleos\nde pareja o filiación", 
                             "Sin lazos\npareja o filiación"),
                     labels=c("Unipersonales", 
                             "Pareja sin hijos\n(sin otras personas)",
                             "Pareja con hijos\n(sin otras personas)",
                             "Monoparentales\n(sin otras personas)", 
                             "Pareja sin hijos\n(con otras personas)", 
                             "Pareja con hijos\n(con otras personas)",
                             "Monoparentales\n(con otras personas)",
                             "Dos o + núcleos\nde pareja o filiación", 
                             "Sin lazos\npareja o filiación"))+
      theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="right",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=18,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=18,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG1_HOGARES_2011.tiff", scale = 3, width = 16.5, height = 8.8, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.2: Distribución de los hogares y de la población por tipo según estatus migratorio del hogar. 2011** 
```{r fig.width=16.5, fig.height=8.8}
CAP6_GRAFIC2 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC2.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC2$HOGAR2 <- rep(c("Unipersonales", "Pareja sin hijos\n(sin otras personas)","Pareja con hijos\n(sin otras personas)",
                          "Monoparentales\n(sin otras personas)", "Pareja sin hijos\n(con otras personas)", 
                          "Pareja con hijos\n(con otras personas)","Monoparentales\n(con otras personas)",
                          "2 o + nucleos\nde pareja o filiación", "Sin lazos\npareja o filiación"),4)
CAP6_GRAFIC2$HOGAR2 <- factor(CAP6_GRAFIC2$HOGAR2,
                   levels = c("Pareja con hijos\n(sin otras personas)","Pareja sin hijos\n(sin otras personas)",
                              "Unipersonales","Monoparentales\n(sin otras personas)","Sin lazos\npareja o filiación",
                              "Pareja con hijos\n(con otras personas)","2 o + nucleos\nde pareja o filiación",
                              "Monoparentales\n(con otras personas)","Pareja sin hijos\n(con otras personas)"))
CAP6_GRAFIC2$TIPO <- factor(CAP6_GRAFIC2$TIPO,
                   levels = c("Población","Hogares"))
CAP6_GRAFIC2$GRUPO <- factor(CAP6_GRAFIC2$GRUPO,
                   levels = c("Nativos","Inmigrante", "Generación 1.5", "2da Generación"))
A<-ggplot(data=CAP6_GRAFIC2, aes(x=TIPO, y=PROP, fill=TIPO, label=round(PROP*100, digits =2))) + 
  geom_bar(stat="identity",colour="black")+
  coord_flip()+
  geom_text(hjust=-0.15, size=5)+
  scale_y_continuous(breaks=c(0,0.5,1),labels=percent)+
  expand_limits(y=c(0,1))+
  facet_grid(GRUPO~  HOGAR2)+
    scale_fill_manual(name="",
                     values=c( "#A4A4A4","#F2F2F2"), #"#D8D8D8","#6E6E6E", "#424242", "#1C1C1C"),
                     breaks=c("Hogares","Población"),
                     labels=c("Hogares","Población"))+
      theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 90,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=12,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG2_HOGARES_2011.tiff", scale = 3, width = 16.5, height = 8.8, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.3: Peso de los hogares complejos en el total según el estatus del hogar y el tipo de complejidad del hogar. 2011** 
```{r fig.width=9.8, fig.height=7}
CAP6_GRAFIC3 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC3.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC3$HOGAR2 <- rep(c("Hogares con núcleo\nsin complejidad vertical\n(otros no ascendentes)","Hogares con núcleo\ny complejidad vertical\n(con ascendentes)","Personas no vinculadas\npor pareja o filiación"),5)
CAP6_GRAFIC3$POP <- factor(CAP6_GRAFIC3$POP,
                   levels = c("Nativos","Generación 1.5", "2da Generación", "Inmigrantes", "Total hogares"))
A<- ggplot(data=CAP6_GRAFIC3, aes(x=POP, y=PROP, fill=HOGAR2,label=round(PROP*100, digits =2))) + 
   geom_bar(stat="identity", position=position_dodge(),colour="black")+
    #geom_text(hjust=-0.15, size=5)+
        scale_y_continuous(labels=percent)+
        expand_limits(y=c(0,0.15))+
        scale_fill_manual(values=c("#D8D8D8", "#BDBDBD", "#A4A4A4"),
                          breaks=c("Hogares con núcleo\nsin complejidad vertical\n(otros no ascendentes)","Hogares con núcleo\ny complejidad vertical\n(con ascendentes)","Personas no vinculadas\npor pareja o filiación"),
                          labels=c("Hogares con núcleo\nsin complejidad vertical\n(otros no ascendentes)","Hogares con núcleo\ny complejidad vertical\n(con ascendentes)","Personas no vinculadas\npor pareja o filiación"))+
         theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position = c(.61, .95),
              legend.background = element_rect(fill="#FFFFFF"),
              #legend.justification=c(6,2),
              legend.direction = "horizontal", 
              legend.key=element_rect(size=10),
              legend.key.size = unit(2.5, "lines"),
              
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=13,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG3_HOGARES_2011.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (Marruecos)** 

```{r fig.width=5.5, fig.height=4.5}
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_MARRUECOS.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Marruecos",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_MARRUECOS.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (Rumania)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_RUMANIA.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Rumania",size=5)+
  
  theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_RUMANIA.tiff", scale = 3,width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (ECUADOR)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_ECUADOR.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación")) 
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Ecuador",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_ECUADOR.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (FRANCIA)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_FRANCIA.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  annotate("text", x = 17.5, y = -9.5, label = "Francia",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_FRANCIA.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (PERU)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_PERU.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Perú",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_PERU.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (REINO UNIDO)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_UK.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  annotate("text", x = 17.5, y = -9.5, label = "Reino Unido",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_UK.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (ARGENTINA)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_ARGENTINA.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Argentina",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_ARGENTINA.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (BOLIVIA)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_BOLIVIA.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Bolivia",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_BOLIVIA.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (FILIPINAS)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_FILIPINAS.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Filipinas",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_FILIPINAS.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (CHINA)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_CHINA.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "China",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_CHINA.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (RUSIA)** 

```{r fig.width=5.5, fig.height=4.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_RUSIA.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Rusia",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDE_RUSIA.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.4: Pirámides por origen del hogar y estructura familiar, 2011 (SENEGAL)** 

```{r fig.width=10.5, fig.height=5.5}
library(ggplot2)
CAP6_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC4_SENEGAL.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC4$EDAD <- factor(CAP6_GRAFIC4$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89"))
CAP6_GRAFIC4 [151,2] <- -6.4
CAP6_GRAFIC4$HOGARES <- as.factor(CAP6_GRAFIC4$HOGARES)
CAP6_GRAFIC4$HOGARES <- factor(CAP6_GRAFIC4$HOGARES,
                       levels = c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                  "Pareja con hijos y otros",
                                  "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                  "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))
A<-ggplot(CAP6_GRAFIC4, aes(x=factor(EDAD), y=PROP, fill=HOGARES))+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Unipersonales Mujeres"|
                                         CAP6_GRAFIC4$SEX2=="Monoparentales Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  geom_bar(data = CAP6_GRAFIC4[(CAP6_GRAFIC4$SEX2=="Pareja con hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja con hijos y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Personas no vinculadas por pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Dos o más nucleos de pareja o filiación Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales y otros Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Pareja sin hijos y otros Hombre"|
                                           CAP6_GRAFIC4$SEX2=="Unipersonales Hombres"|
                                           CAP6_GRAFIC4$SEX2=="Monoparentales Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = HOGARES), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-12,12),breaks = c(-12, -10, -8, -6, -4, -2,0, 2, 4, 6, 8, 10, 12), 
                     labels = paste0(as.character(c(seq(12, 0, -2), seq(2, 12, 2))), "%")) + 
   scale_fill_manual(
                     values=c("#013ADF", "#2E64FE", "#5882FA", "#819FF7", 
                              "#B40404", "#FF0000", "#FA5858", "#F78181", "#F5A9A9"), 
                     breaks=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"), 
                     labels=c("Unipersonales",
                                  "Pareja con hijos",
                                  "Pareja sin hijos",  
                                  "Monoparentales", 
                                 "Pareja con hijos y otros",
                                 "Pareja sin hijos y otros",
                                  "Monoparentales y otros", 
                                 "Dos o más nucleos de pareja o filiación",
                                  "Personas no vinculadas por pareja o filiación"))+
  
  annotate("text", x = 17.5, y = -9.5, label = "Senegal",size=5)+
  
 theme(plot.title = element_text(lineheight=5.6, size=10, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position = "none",
              legend.background = element_rect(fill="#FFFFFF"),
              #legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG4_PIRAMIDES_SENEGAL.tiff", scale = 3, width = 5.5, height = 4.1, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 6.5: Indicador resumen de la vulnerabilidad del hogar (% hogares vulnerables) por origen**

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP6_GRAFIC5 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_6_GRAFIC5.csv"), stringsAsFactors=FALSE)
CAP6_GRAFIC5$PAIS <- factor(CAP6_GRAFIC5$PAIS, levels = c("Reino Unido","Nativos","China","Francia","Rusia",
                                                          "Perú", "Argentina","Filipinas","Ecuador","Rumanía",
                                                          "Bolivia","Senegal","Marruecos"))
A<- ggplot(CAP6_GRAFIC5, aes(x = PAIS, y = PROP, fill=PAIS)) + geom_bar(stat = "identity", colour="black")+coord_flip()+
  scale_y_continuous(breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35),labels=percent)+
  expand_limits(y=c(0,0.35))+
    scale_fill_manual(values = c("#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD",
                               "#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position = "none",
              legend.background = element_rect(fill="#FFFFFF"),
              #legend.justification=c(6,2),
              legend.direction = "horizontal", 
              legend.key=element_rect(size=10),
              legend.key.size = unit(2.5, "lines"),
              
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=13,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP6_FIG5_PASIES HOGARES.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

### **<span style="color:red">Capítulo 7: DIVERSIDAD Y CONCENTRACIÓN TERRITORIAL DE LA POBLACIÓN MARROQUÍ EN ESPAÑA [Autores: Jordi Bayona-i-Carrasco & Hicham Achebak]</span>**  

<br>

#### **Figura 7.2: Evolución de los espacios de concentración de la población marroquí en España, 2000-2014** 

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP7_GRAFIC2 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_7_GRAFIC2.csv"), stringsAsFactors=FALSE)
CAP7_GRAFIC2$CAT <- factor(CAP7_GRAFIC2$CAT,  levels = c("< 5%",
                                                        "(5-10%]",
                                                        "(10-15%]",
                                                        "(15-20%]",
                                                        "(20-25%]",
                                                        "(25-30%]",
                                                        "(30-35%]",
                                                        "(35-40%]",
                                                        "(40-45%]",
                                                        "(45-50%]",
                                                        "> 50%"))
A<- ggplot(CAP7_GRAFIC2, aes(x = CAT, y = PROP, fill=factor(YEAR))) + geom_bar(stat = "identity",position = "dodge",colour="black")+#coord_flip()+
  scale_y_continuous(breaks=c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1),labels=percent)+
  expand_limits(y=c(0,1))+
    scale_fill_manual(name="",
                     values=c("#D8D8D8", "#A4A4A4", "#6E6E6E", "#424242"),
                     breaks=c("2000","2005","2010","2014"),
                     labels=c("2000","2005","2010","2014"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
                 legend.position = c(.80, .91),
              legend.background = element_rect(fill=NA),
              #legend.justification=c(6,2),
              legend.direction = "horizontal", 
              legend.key=element_rect(size=10),
              legend.key.size = unit(1.5, "lines"),
              
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 90,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=13,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP7_FIG2.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 7.3: Composición por nacionalidad de los espacios de concentración de población marroquí, 2000-2014** 

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP7_GRAFIC3 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_7_GRAFIC3.csv"), stringsAsFactors=FALSE)
CAP7_GRAFIC3$PAIS <- factor(CAP7_GRAFIC3$PAIS,  levels = c("Marruecos","España","África","Asia","América Latina y el Caribe","Europa Occidental","Europa Oriental")) 
A<- ggplot(CAP7_GRAFIC3, aes(x = CAT, y = POP, fill=factor(PAIS))) + geom_bar(stat = "identity",  colour="black")+#coord_flip()+
  expand_limits(y=c(0,200000))+
  facet_grid(~YEAR)+
  scale_fill_manual(name="",
                     values=c("#2E2E2E", "#585858", "#848484", "#A4A4A4", "#BDBDBD", "#D8D8D8", "#E6E6E6"),
                     breaks=rev(c("Marruecos","España","África","Asia","América Latina y el Caribe","Europa Occidental","Europa Oriental")),
                     labels=rev(c("Marruecos","España","África","Asia","América Latina y el Caribe","Europa Occidental","Europa Oriental")))+ 
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position = c(.165, .80),
              legend.background = element_rect(fill=NA),
              #legend.justification=c(6,2),
              #legend.direction = "vertical", 
              #legend.key=element_rect(size=10),
              legend.key.size = unit(1.5, "lines"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 90,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=12,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP7_FIG3.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 7.4: Evolución de la diversidad (índice de diversidad de Simpson) en función de la concentración de la población marroquí, 2000-2014** 

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP7_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_7_GRAFIC4.csv"), stringsAsFactors=FALSE)
CAP7_GRAFIC4$CAT <- factor(CAP7_GRAFIC4$CAT,  levels = c("< 20%","(20-30%]","(30-40%]", "(40-50%]", "(50-60%]", "(60-70%]"))
A <- ggplot(CAP7_GRAFIC4, aes(x=YEAR, y=PROP, group=CAT))+
  geom_line(aes(colour = CAT), linetype = 1, size=2)+
  scale_y_continuous(breaks=c(1,1.5,2,2.5,3))+
  expand_limits(y=c(1,3))+
   scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))+
  scale_colour_manual(name="",
                     values=rev(c("#2E2E2E", "#585858", "#848484", "#A4A4A4", "#BDBDBD", "#D8D8D8")), 
                     breaks=rev(c("< 20%","(20-30%]","(30-40%]", "(40-50%]", "(50-60%]", "(60-70%]")), 
                     labels=rev(c("< 20%","(20-30%]","(30-40%]", "(40-50%]", "(50-60%]", "(60-70%]"))) +
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position = c(.105, .837),
              legend.background = element_rect(fill=NA),
              legend.key.size = unit(1.5, "lines"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=13,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP7_FIG4.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

### **<span style="color:red">Capítulo 8: SURASIÁTICOS EN MADRID Y BARCELONA: ENCARNANDO LA DIVERSIDAD [Autores: Nachatter Singh Garha, Andreu Domingo & Ana María López Sala]</span>**  

<br>

#### **Figura 8.1: Flujos inmigratorios de surasiáticos en España, por sexo y lugar de nacimiento, 2000-2014** 
```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP8_GRAFIC1 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC1.csv"), stringsAsFactors=FALSE)
CAP7_GRAFIC4$CAT <- factor(CAP7_GRAFIC4$CAT,  levels = c("< 20%","(20-30%]","(30-40%]", "(40-50%]", "(50-60%]", "(60-70%]"))
A <- ggplot(CAP8_GRAFIC1, aes(x=YEAR, y=POP, group=SEX))+
  geom_line(aes(colour = SEX), linetype = 1, size=2)+
  expand_limits(y=c(0,25000))+
   scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))+
  scale_colour_manual(name="",
                     values=c( "#A4A4A4","#2E2E2E"), #"#D8D8D8","#6E6E6E", "#424242", "#1C1C1C"),
                     breaks=c("Males","Females"),
                     labels=c("Hombres","Mujeres"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position = c(.90, .90),
              legend.background = element_rect(fill=NA),
              #legend.justification=c(6,2),
              #legend.direction = "vertical", 
              #legend.key=element_rect(size=10),
              legend.key.size = unit(1.5, "lines"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=13,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  ylab("Población\n")+ xlab("")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG1.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 8.2: Pirámide de la población nacida en el Sur de Asia empadronada en España, 2014** 

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
CAP8_GRAFIC2 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC2.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC2$EDAD <- factor(CAP8_GRAFIC2$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89", "90-94", "95 y más"))
A<-ggplot(CAP8_GRAFIC2, aes(x=factor(EDAD), y=PROP2, fill=SEX))+
  geom_bar(data = CAP8_GRAFIC2[(CAP8_GRAFIC2$SEX=="Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP2,fill = SEX), alpha = 1)+
  geom_bar(data = CAP8_GRAFIC2[(CAP8_GRAFIC2$SEX=="Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP2,fill = SEX), alpha = 1)+
  coord_flip()+
scale_y_continuous(limits=c(-15,15),breaks = c(-15, -10, -5,0,5,10,15), 
                   labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "%")) + 
   scale_fill_grey()+
  annotate("text", x = 18.7, y = -4.98, label = "Hombres\n96.741",size=8)+
  annotate("text", x = 18.7, y = 4.95, label = "Mujeres\n34.485",size=8)+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position = "none",
              legend.background = element_rect(fill="#FFFFFF"),
              #legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG2.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 8.4: Principales municipios con población surasiática y distribución porcentual, España, 2014** 

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP8_GRAFIC4 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC4.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC4$MUN <- factor(CAP8_GRAFIC4$MUN, levels = c("Salou","Torrevieja", "Adeje", "Olot","Vitoria-Gasteiz","Lloret de Mar",
                                                          "Benidorm","Palma de Mallorca","Logroño","St. Coloma de Gramanet",
                                                           "Hospitalet de llobregat","Valencia","Badalona","Madrid","Barcelona"))
A<- ggplot(CAP8_GRAFIC4, aes(x = MUN, y = POP, fill=MUN)) + geom_bar(stat = "identity", colour="black")+coord_flip()+
  #scale_y_continuous(breaks=c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35),labels=percent)+
  expand_limits(y=c(0,30000))+
    scale_fill_manual(values = c("#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD",
                               "#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position = "none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.direction = "horizontal", 
              legend.key=element_rect(size=10),
              legend.key.size = unit(2.5, "lines"),
              axis.title.x = element_text(colour="black", size=15),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=13,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("")+ ylab("\nPoblación")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG4.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 8.5: Índice de disimilitud de la población surasiática residente en Madrid y Barcelona, sobre la tendencia de los 10 principales municipios donde se encuentra cada una de las poblaciones, 2000-2014** 

```{r fig.width=9.8, fig.height=14.5}
library(ggplot2)
library(scales)
CAP8_GRAFIC5 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC5_PAKISTAN.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC5$PAIS <- factor(CAP8_GRAFIC5$PAIS, levels = c("Pakistán","India", "Bangladesh"))
CAP8_GRAFIC5$MUN <- factor(CAP8_GRAFIC5$MUN, levels = c("Benidorm","Badalona","Hospitalet de Llobregat","Valencia","St. Coloma de Gramenet","Vitoria-Gasteiz","Madrid","Barcelona","Palma de Mallorca"))
A <- ggplot(CAP8_GRAFIC5, aes(x=YEAR, y=D, group=MUN))+
     geom_line(aes(colour = MUN,alpha=TRAN,linetype=factor(LINE)), size=2)+
     expand_limits(y=c(0,100))+
     scale_x_continuous(breaks=c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2011,2012,2013,2014))+
     scale_colour_manual(name="",
                     values=c( "#585858","#585858","#585858","#585858","#585858","#585858","#000000","#1C1C1C","#585858"),
                     breaks=c("Benidorm","Badalona","Hospitalet de Llobregat","Valencia","St. Coloma de Gramenet",
                               "Vitoria-Gasteiz","Madrid","Barcelona","Palma de Mallorca"),
                     labels=c("Benidorm","Badalona","Hospitalet de Llobregat","Valencia","St. Coloma de Gramenet",
                               "Vitoria-Gasteiz","Madrid","Barcelona","Palma de Mallorca"))+
   facet_wrap(~PAIS, ncol = 1)+
   annotate("segment", x = 2010.5, xend = 2012, y = 27.5, yend = 27.5,size=2,colour = c("#1C1C1C","#1C1C1C","#1C1C1C") )+
   annotate("text", label = "Madrid", size = 6, x = 2013, y = 27.5)+
   annotate("segment", x = 2010.5, xend = 2012, y = 17.5, yend = 17.5,size=2, linetype= 2,colour = c("#1C1C1C","#1C1C1C","#1C1C1C") )+
   annotate("text", label = "Barcelona", size = 6, x = 2013, y = 17.5)+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 12),
              legend.position = "none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.direction = "horizontal", 
              legend.key=element_rect(size=10),
              legend.key.size = unit(2.5, "lines"),
              axis.title.x = element_text(colour="black", size=17),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=15,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("")+ ylab("Índice de disimilitud\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG5.tiff", scale = 3, width = 9.8, height = 14.5, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 8.11: Concentración de la población surasiática en el barrio de El Raval, 10 primeras nacionalizaciones, y pirámides de población surasiática y española, 2014** 

```{r fig.width=9.8, fig.height=9.8}
library(ggplot2)
library(scales)
CAP8_GRAFIC11 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC11_RAVAL.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC11$PAIS <- factor(CAP8_GRAFIC11$PAIS, levels = c("Rumania","China","Colombia","Ecuador","India","Argentina",                                                       "Bangladesh","Italia","Marruecos","Filipinas","Pakistán"))
A<- ggplot(CAP8_GRAFIC11, aes(x = PAIS, y = POP, fill=PAIS)) + geom_bar(stat = "identity", colour="black")+coord_flip()+
    expand_limits(y=c(0,8000))+
    scale_fill_manual(values = c("#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD",
                               "#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD"))+
    theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position = "none",
              legend.background = element_rect(fill="#FFFFFF"),
              #legend.justification=c(6,2),
              legend.direction = "horizontal", 
              legend.key=element_rect(size=10),
              legend.key.size = unit(2.5, "lines"),
              axis.title.x = element_text(colour="black", size=12),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=12,colour="black"),
              axis.title.y = element_text(colour="black", size=12),
              axis.text.y  = element_text(vjust=0.5, size=12,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=12,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("")+ ylab("\nPoblación")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG11.tiff", scale = 3, width = 9.8, height = 9.8, units = c("cm"), dpi = 300)
```

<br>

```{r fig.width=16, fig.height=10}
library(ggplot2)
library(scales)
CAP8_GRAFIC11 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC11_RAVAL_PIRAMIDE1.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC11$EDAD <- factor(CAP8_GRAFIC11$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84", "85 y más"))
CAP8_GRAFIC11$CAT <- factor(CAP8_GRAFIC11$CAT,
                   levels = c("Surasiático nacido en España", "Surasiático nacido en Surasia","Español nacido en Surasia" ))
A<-ggplot(CAP8_GRAFIC11, aes(x=EDAD, y=POP, fill=SEX))+
  geom_bar(data = CAP8_GRAFIC11[(CAP8_GRAFIC11$SEX2=="Mujeres-Surasiático nacido en Surasia"|
                                         CAP8_GRAFIC11$SEX2=="Mujeres-Español nacido en Surasia"|
                                         CAP8_GRAFIC11$SEX2=="Mujeres-Surasiático nacido en España"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = POP,fill = CAT), alpha = 1)+
  geom_bar(data = CAP8_GRAFIC11[(CAP8_GRAFIC11$SEX2=="Hombres-Surasiático nacido en Surasia"|
                                           CAP8_GRAFIC11$SEX2=="Hombres-Español nacido en Surasia"|
                                           CAP8_GRAFIC11$SEX2=="Hombres-Surasiático nacido en España"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = POP,fill = CAT), alpha = 1)+
  coord_flip()+
 scale_y_continuous(limits=c(-15,15),breaks = c(-15, -10, -5,0,5,10,15), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "%")) + 
  scale_fill_manual(values = c("#BDBDBD","#848484","#1C1C1C"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=30,colour="black"),
              axis.title.y = element_text(colour="black", size=30),
              axis.text.y  = element_text(vjust=0.5, size=30,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG11_PIRAMIDE_RAVAL.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP8_GRAFIC11 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC11_RAVAL_PIRAMIDE2.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC11$EDAD <- factor(CAP8_GRAFIC11$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84", "85 y más"))
#CAP8_GRAFIC11$CAT <- factor(CAP8_GRAFIC11$CAT,
 #                  levels = c("Surasiático nacido en España", "Surasiático nacido en Surasia","Español nacido en Surasia" ))
A<-ggplot(CAP8_GRAFIC11, aes(x=EDAD, y=PROP, fill=SEX))+
  geom_bar(data = CAP8_GRAFIC11[(CAP8_GRAFIC11$SEX=="Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = SEX), alpha = 1)+
  geom_bar(data = CAP8_GRAFIC11[(CAP8_GRAFIC11$SEX=="Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = SEX), alpha = 1)+
  coord_flip()+
 scale_y_continuous(limits=c(-15,15),breaks = c(-15, -10, -5,0,5,10,15), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "%")) + 
  scale_fill_manual(values = c("#1C1C1C","#BDBDBD"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG11_PIRAMIDE_RAVAL2.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

#### **Figura 8.12: Concentración de la población surasiática en el barrio de Lavapiés, 10 primeras nacionalizaciones, y pirámides de población surasiática y española, 2014** 

```{r fig.width=9.8, fig.height=9.8}
library(ggplot2)
library(scales)
CAP8_GRAFIC11 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC11_LAVAPIES.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC11$PAIS <- factor(CAP8_GRAFIC11$PAIS, levels = c("India","Pakistán","Rumania","Italia","Marruecos","China",
"Filipinas","Colombia","Argentina","Bangladesh","Ecuador"))
A<- ggplot(CAP8_GRAFIC11, aes(x = PAIS, y = POP, fill=PAIS)) + geom_bar(stat = "identity", colour="black")+coord_flip()+
  expand_limits(y=c(0,3000))+
    scale_fill_manual(values = c("#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD",
                               "#BDBDBD","#BDBDBD","#BDBDBD","#BDBDBD"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position = "none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.direction = "horizontal", 
              legend.key=element_rect(size=10),
              legend.key.size = unit(2.5, "lines"),
              axis.title.x = element_text(colour="black", size=15),
              axis.text.x  = element_text(angle = 00,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=13,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("")+ ylab("\nPoblación")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG11_B.tiff", scale = 3, width = 9.8, height = 9.8, units = c("cm"), dpi = 300)
```

<br>

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP8_GRAFIC11 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC11_LAVAPIES_PIRAMIDE1.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC11$EDAD <- factor(CAP8_GRAFIC11$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84", "85 y más"))
CAP8_GRAFIC11$CAT <- factor(CAP8_GRAFIC11$CAT,
                   levels = c("Surasiático nacido en España", "Surasiático nacido en Surasia","Español nacido en Surasia" ))
A<-ggplot(CAP8_GRAFIC11, aes(x=EDAD, y=POP, fill=SEX))+
  geom_bar(data = CAP8_GRAFIC11[(CAP8_GRAFIC11$SEX2=="Mujeres-Surasiático nacido en Surasia"|
                                         CAP8_GRAFIC11$SEX2=="Mujeres-Español nacido en Surasia"|
                                         CAP8_GRAFIC11$SEX2=="Mujeres-Surasiático nacido en España"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = POP,fill = CAT), alpha = 1)+
  geom_bar(data = CAP8_GRAFIC11[(CAP8_GRAFIC11$SEX2=="Hombres-Surasiático nacido en Surasia"|
                                           CAP8_GRAFIC11$SEX2=="Hombres-Español nacido en Surasia"|
                                           CAP8_GRAFIC11$SEX2=="Hombres-Surasiático nacido en España"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = POP,fill = CAT), alpha = 1)+
  coord_flip()+
 scale_y_continuous(limits=c(-20,20),breaks = c(-20,-15, -10, -5,0,5,10,15,20), 
                     labels = paste0(as.character(c(seq(20, 0, -5), seq(5, 20, 5))), "%")) + 
  scale_fill_manual(values = c("#BDBDBD","#848484","#1C1C1C"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG11_PIRAMIDE_LAVAPIES.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"), dpi = 300)
```

<br>

```{r fig.width=9.8, fig.height=7}
library(ggplot2)
library(scales)
CAP8_GRAFIC11 <- read.csv(url("http://gedemced.uab.cat/DATOS_ICARIA/CAP_8_GRAFIC11_LAVAPIES_PIRAMIDE2.csv"), stringsAsFactors=FALSE)
CAP8_GRAFIC11$EDAD <- factor(CAP8_GRAFIC11$EDAD,
                   levels = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                              "50-54","55-59","60-64","65-69","70-74","75-79","80-84", "85 y más"))
#CAP8_GRAFIC11$CAT <- factor(CAP8_GRAFIC11$CAT,
 #                  levels = c("Surasiático nacido en España", "Surasiático nacido en Surasia","Español nacido en Surasia" ))
A<-ggplot(CAP8_GRAFIC11, aes(x=EDAD, y=PROP, fill=SEX))+
  geom_bar(data = CAP8_GRAFIC11[(CAP8_GRAFIC11$SEX=="Mujeres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = SEX), alpha = 1)+
  geom_bar(data = CAP8_GRAFIC11[(CAP8_GRAFIC11$SEX=="Hombres"),], 
           colour = I("Black"),stat="identity", size=.3, colour="black",
           aes(x = EDAD, y = PROP,fill = SEX), alpha = 1)+
  coord_flip()+
 scale_y_continuous(limits=c(-15,15),breaks = c(-15, -10, -5,0,5,10,15), 
                     labels = paste0(as.character(c(seq(15, 0, -5), seq(5, 15, 5))), "%")) + 
  scale_fill_manual(values = c("#1C1C1C","#BDBDBD"))+
   theme(plot.title = element_text(lineheight=5.6, size=20, face="bold"),
              legend.title = element_blank(),
              legend.text = element_text(colour="black", size = 15),
              legend.position="none",
              legend.background = element_rect(fill="#FFFFFF"),
              legend.justification=c(0,1), 
              legend.key.size = unit(1, "cm"),
              axis.title.x = element_blank(),
              axis.text.x  = element_text(angle = 0,vjust=0.5, size=15,colour="black"),
              axis.title.y = element_text(colour="black", size=15),
              axis.text.y  = element_text(vjust=0.5, size=15,colour="black"),
              strip.text=element_text(angle = 0,vjust=0.5, size=9,colour="black", face = "bold"),
              panel.background = element_rect(fill = "#FFFFFF"), 
              panel.grid = element_line(colour="#000000"),
              panel.grid.major=element_line(colour="#BDBDBD"), 
              panel.grid.minor=element_line(colour="white"),
              plot.background = element_rect(fill = "#FFFFFF"))+
  xlab("Edad\n")
A
setwd("C:\\Users\\jgaleano\\Desktop\\ICARIA\\GRAFICOS")
#ggsave("CAP8_FIG11_PIRAMIDE_LAVAPIES2.tiff", scale = 3, width = 9.8, height = 7, units = c("cm"),dpi = 300, limitsize = TRUE)
```

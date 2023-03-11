"ANÁLISIS DE EVENTOS EXTREMOS DE PRECIPITACIONES A DIFERENTES PERIODO DE RETORNO"
#################################################################################
###################*****RARQ********RARQ*******RARQ******########################
#################################################################################
rm(list = ls())
library(extremeStat)
library(hydrostats)
library(base)
library(zoo)
library(tidyverse)
library(readxl)
library(naniar)
library(rio)
library(dplyr)
setwd("C:/Users/ANTHONYQUIROZ/Downloads/IDF")
Max_pp<-read.csv(file = "p24max-pisco.csv",header =TRUE)
Max_pp<-Max_pp[2:95]
Max_pp_long<-Max_pp%>%
  gather(key ="Estacion",value ="valor",1:94)

#Obtenemos las Estaciones y el numero en un vector.
vector_Estaciones<-unique(Max_pp_long$Estacion)
length(vector_Estaciones)

#Realizamos el proceso iterativo para cada una de las estaciones con valores simulados y observados
for (i in 1:length(vector_Estaciones)){
  Max_pp<-Max_pp_long%>%
    filter(Estacion==vector_Estaciones[i])%>%
    select(-(Estacion))%>%as.matrix()
  dlf <- distLfit(Max_pp)
  #*str(dlf)
  #*View(dlf$gof) ### Vemos el ajuste de las mejores fdp
  #*dlf$distnames # Distribuciones probadas 
  #*dlf$parameter # Parametros de las distribuciones
  ###Ajuste de histogramas
  #*plotLfit(dlf,nbest=5,las=0.1,lty=1,legend = T)
  #### Ajuste probabilidad acumulada
  #*plotLfit(dlf, cdf=TRUE,las=0.1)
  # ..............................................................................
  # ESTADISTICA DE VALORES EXTREMOS (PERIODOS DE RETORNO) 
  # adaptado para precipitaciones
  #periodos de retorno
  dlf <- distLextreme(Max_pp, quiet = TRUE,RPs=c(5,10,20,50,100,1000))
  
  #*plotLextreme(dlf, log = TRUE, legargs = list(cex = 0.5, bg = "transparent", horiz = T), 
               #*nbest = 5, ylab = "Precipitation [mm]", xlab = "Periodo de retorno [year]")
  
  # Ver los resultados 
  Resultado<-dlf$returnlev[1,]
  # Guardar los resultados
  Resultado%>%
    write.csv(file=paste0('OUT_2/', vector_Estaciones[i], '.csv'))
  # Banda de incertidumbre para la distribucion de Wakeby (la mejor)
  dle_boot <- distLexBoot(dlf, n=100, nbest=1,RPs=c(5,10,20,50,100,1000))
  #Graf<-plotLexBoot(dle_boot, distcol="green",legargs = list(cex = 0.5, bg = "transparent", horiz = T))%>%
    #png(file=paste0('OUT_3/limitPR.png', vector_Estaciones[i], '.png'),width =1080 , height = 480, units = "px")%>%
    #dev.off()
    #ggsave(file=paste0('OUT_3/limitPR.png', vector_Estaciones[i], ".png"),
           #height = 11, width = 18, dpi=300)
    
  A<-as.data.frame(dle_boot$exBootCI)%>%
    write.csv(file=paste0('OUT_4/', vector_Estaciones[i], '.csv'))
}

getwd()
setwd("C:/Users/ANTHONYQUIROZ/Downloads/IDF")
filename<-list.files(pattern = "*.csv")
Corto_Total<-lapply(filename, read.csv)%>%bind_cols()
library(purrr)
setwd("C:/Users/ANTHONYQUIROZ/Downloads/IDF/OUT_2")
merge.Corto<-list.files(pattern="*.csv")%>%set_names()%>%map_dfr(read.csv,.id = "file_name")
setwd("C:/Users/ANTHONYQUIROZ/Downloads/IDF/OUT_4")
merge.Lim<-list.files(pattern = "*.csv")%>%set_names()%>%map_dfr(read.csv,.id = "file_name")
export(merge.Lim, file = "EST_LIM.xlsx", format = "xlsx")
export(merge.Corto, file = "EST_CORTO.xlsx", format = "xlsx")
######
merge.Lim$file_name<-paste(merge.Lim$file_name,merge.Lim$X,sep = ".")
merge.Lim<-merge.Lim%>%select(-X)
Lim<-merge.Lim%>%
  gather(Distribucion,Valor,'kap.5':'lap.1000') #Criterio Estación, com valores 
Lim<-Lim[!is.na(Lim$Valor),]
kap_5<-Lim%>%
  filter(Distribucion=="kap.5")
setwd("C:/Users/ANTHONYQUIROZ/Downloads/IDF/DIST")
D<-unique(Lim$Distribucion)
for(j in 1:length(D)){
  F<-Lim%>%
    filter(Distribucion==D[j])
  F%>% 
    export(paste0(".",D[j], '.xlsx'))
}
p1<-Lim%>%
  mutate(rp=substring(Distribucion,first=5))
p2<-p1%>%
  mutate(l=substring(file_name,first=8))
export(p2,file="LIMITES.xlsx","xlsx")

"CODIGO ADAPTADO DE ANALISIS DE EVENTOS EXTREMOS UTILIZANDO PISCO
@autor: Kevin Arnold Traverso / Waldo Lavado
e-mail: arnold.traverso@gmail.com
e-mail: waldo.lavado@gmail.com"

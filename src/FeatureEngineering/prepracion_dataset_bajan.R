require("data.table")
require("randomForest")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")
require("lubridate")

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasetsOri/paquete_premium.csv.gz", stringsAsFactors= TRUE)
#gc()

palancas  <- list()  #variable con las palancas para activar/desactivar
palancas$version  <- "v1"   #Muy importante, ir cambiando la version

#achico el dataset
cantidad_meses<-c(2)
## los baja+1
dataset  <-  dataset[  clase_ternaria =="BAJA+1", ]

#los que se fueron en 2020
dataset<-dataset[foto_mes>=202001  & foto_mes<=202011,]                 #restrinjo a los que se van el ultimo año

dataset<-dataset[,foto_mes_salida:=max(foto_mes),by=numero_de_cliente]  #caclulo en que mes se van
dataset<-dataset[,foto_mes_date:=as.Date(paste0(as.character(foto_mes_salida),"01"), "%Y%m%d")]
dataset<-dataset[,foto_mes_date_ini:=foto_mes_date %m+% months(-cantidad_meses)]   #le resto los meses
dataset<-dataset[,foto_mes_ini:=as.numeric(gsub("([0-9]+)-([0-9]+)-[0-9]+","\\1\\2",foto_mes_date_ini))]
dataset[, foto_mes_date:=NULL]
dataset[, foto_mes_date_ini:=NULL]
dataset[, foto_mes_salida:=NULL]

dataset<-dataset[foto_mes>=foto_mes_ini] #me quedo solo con los meses elegidos

dataset<-dataset[foto_mes>=202001  & foto_mes<=202011, ]

fwrite( dataset,
        paste0( "./datasets/dataset_bajan", palancas$version, ".csv.gz" ),
        logical01 = TRUE,
        sep= "," )
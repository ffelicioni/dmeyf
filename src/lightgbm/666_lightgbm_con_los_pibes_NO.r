#Este LightGBM fue construido  para destronar a quienes desde el inicio utilizaron XGBoost y  LightGBM
#mientras sus compañeros luchaban por correr un rpart

#Con los pibes NO

#limpio la memoria
rm( list=ls() )
gc()

require("data.table")
require("lightgbm")

#library(pacman)
#p_load(this::path)
#setwd(this.path::this.dir())
#source('./lib/features_eng.R')

setwd("C:/Users/Flavia/Documents/DataScience/dmeyf") 

#datos<-6
#datitos<-mostrar(5)
#cargo el dataset
#dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")

dataset  <- fread("./datasets/paquete_premium_202009_ext.csv")

#creo la clase_binaria donde en la misma bolsa estan los BAJA+1 y BAJA+2
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0, 1 ) ]

#Quito el Data Drifting de  "ccajas_transacciones"  "Master_mpagominimo"
campos_buenos  <- setdiff( colnames(dataset),
                           c("clase_ternaria", "clase01", "ccajas_transacciones", "Master_mpagominimo" ) )




#genero el formato requerido por LightGBM
dtrain  <- lgb.Dataset( data=  data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset[ , clase01]
                      )

#cargo el dataset donde aplico el modelo
#dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")
dapply  <- fread("./datasets/paquete_premium_202011_ext.csv")

#-----------------------------------  correcion Master_Finicio mora

library("lubridate")
correccion_habiles <- function(fin_de_mes,x)
{ 
  if (!is.na(x)){
    dias_mora<-as.numeric(x)
    if (dias_mora>0){
      rango<-fin_de_mes-days(seq(0,dias_mora-1))
      feriados<-date(c("2020-07-09","2020-07-10","2020-08-17","2020-10-12","2020-11-23"))#faltan cargar todos los feriados del año
      rangof<-rango[!rango %in% feriados]                                   #se sacan los dias que son feriados de la secuencia
      #print(rangof)
      rm(rango)
      return (sum(!weekdays(rangof) %in% c("Saturday", "Sunday")))
    }
    else{ return (0)    } 
  }
  else {
    return (x)
  }
}

fin_foto_mes<-date(ym(dataset$foto_mes[1]))+months(1) - days(1)
dataset$Master_Finiciomora_h<-ifelse(!is.na(dataset$Master_Finiciomora),dataset$Master_Finiciomora+5,NA)
zz<-data.frame(dataset$Master_Finiciomora_h)
niveles<-levels(factor(zz[[1]]))
niveles_corregido<-rep(0,length(niveles))
for (i in 1:length(niveles)){
  niveles_corregido[i]<-correccion_habiles(fin_foto_mes,niveles[i])
  dataset$Master_Finiciomora_h[dataset$Master_Finiciomora_h == as.numeric(niveles[i])] <- as.numeric(niveles_corregido[i])
}
rm(niveles)
rm(niveles_corregido)
rm(zz)

fin_foto_mes<-date(ym(dapply$foto_mes[1]))+months(1) - days(1)
dapply$Master_Finiciomora_h<-dapply$Master_Finiciomora
zz<-dapply[,"Master_Finiciomora"]
niveles<-levels(factor(zz[[1]]))
niveles_corregido<-rep(0,length(niveles))
for (i in 1:length(niveles)){
  niveles_corregido[i]<-correccion_habiles(fin_foto_mes,as.numeric(niveles[i]))
  dapply$Master_Finiciomora_h[dapply$Master_Finiciomora_h == as.numeric(niveles[i])] <- as.numeric(niveles_corregido[i])
}
rm(niveles)
rm(niveles_corregido)
rm(zz)

#dataset<-dataset[,-c("Master_Finiciomora")]
#dapply<-dapply[,-c("Master_Finiciomora")]

#dataset[,c("Master_Finiciomora")]<-dataset$Master_Finiciomora_h
#dapply[,c("Master_Finiciomora")]<-dapply$Master_Finiciomora_h

dataset<-dataset[,-c("Master_Finiciomora_h")]
dapply<-dapply[,-c("Master_Finiciomora_h")]

dapply$cliente_antiguedad<-dapply$cliente_antiguedad-2 # se refiere al mes del entrenamiento

#Solo uso DOS hiperparametros,  max_bin  y min_data_in_leaf
#Dadme un punto de apoyo y movere el mundo, Arquimedes
modelo  <- lightgbm( data= dtrain, 
                     params= list( objective= "binary",
                                   max_bin= 15,   #15
                                   min_data_in_leaf= 4000,
                                   learning_rate= 0.05, #0.05
                                   num_iterations=100
                                   )  )


#------------------------------------------------------------------------------
#Funcion que lleva el registro de los experimentos

get_experimento  <- function()
{
  if( !file.exists( "./maestro.yaml" ) )  cat( file="./maestro.yaml", "experimento: 1000" )
  
  exp  <- read_yaml( "./maestro.yaml" )
  experimento_actual  <- exp$experimento
  
  exp$experimento  <- as.integer(exp$experimento + 1)
  Sys.chmod( "./maestro.yaml", mode = "0644", use_umask = TRUE)
  write_yaml( exp, "./maestro.yaml" )
  Sys.chmod( "./maestro.yaml", mode = "0444", use_umask = TRUE) #dejo el archivo readonly
  
  return( experimento_actual )
}

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

kimp<- paste0("./work/E",  kexperimento, "_", kscript, "_" )
tb_importancia  <- lgb.importance( model= modelo )
fwrite( tb_importancia, 
        file= paste0(kimp, "imp_", GLOBAL_iteracion, ".txt"),
        sep="\t" )

#aplico el modelo a los datos nuevos, dapply
prediccion  <- predict( modelo,  data.matrix( dapply[  , campos_buenos, with=FALSE]))

#la probabilidad de corte ya no es 0.025,  sino que 0.031
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion > 0.0309) ) ) #genero la salida 0.031

#genero el archivo para Kaggle
fwrite( entrega, 
        file= "./kaggle/lightgbm_con_los_pibes_NO2.csv",
        sep=  "," )



prediccion_ingenuo  <- predict( modelo,  data.matrix( dataset[  , campos_buenos, with=FALSE]))
ingenuo  <- as.data.table( list( "numero_de_cliente"= dataset[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion_ingenuo > 0.0309) ) ) #genero la salida 0.031

#ingenuo  <- as.data.table( list( "numero_de_cliente"= dataset[  , numero_de_cliente],
#                                 "Predicted"= as.numeric(prediccion_ingenuo > 0.05664) ) ) #genero la salida 0.031

evaluacion<-data.table(prediccion_ingenuo,ingenuo,dataset$clase_ternaria)
colnames(evaluacion)<- c('prediccion_ingenuo','numero_de_cliente','Predicted','clase_ternaria') 
evaluacion$clase_ternaria<-factor(evaluacion$clase_ternaria)
evaluacion$Predicted<-factor(evaluacion$Predicted)
tablita<-table(evaluacion$clase_ternaria,evaluacion$Predicted)
print(tablita)
ganancia_ing<-tablita[2,2]*48500-(tablita[1,2]+tablita[3,2])*1250
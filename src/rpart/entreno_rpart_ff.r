#Grid Search con Arboles de Decision

#limpio la memoria
rm(list=ls())   #remove all objects
gc()            #garbage collection

require("data.table")
require("rlist")
require("parallel")
require("rpart")


#ksemillas  <- c(102191, 200177, 410551, 552581, 892237) #reemplazar por las propias semillas
ksemillas  <- c(100003, 101207, 103577, 103457, 104089)
setwd("C:/Users/Flavia/Documents/DataScience/dmeyf")  


#------------------------------------------------------------------------------

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )

  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )

  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N], by= agrupa ]
}
#------------------------------------------------------------------------------

ArbolSimple  <- function( fold_test, data, param )
{
  #genero el modelo
  modelo  <- rpart("clase_ternaria ~ .", 
                   data= data[ fold != fold_test, ],
                   xval= 0,
                   control= param )
  
  #aplico el modelo a los datos de testing, fold==2
  prediccion  <- predict( modelo, data[ fold==fold_test, ], type = "prob")

  prob_baja2  <- prediccion[, "BAJA+2"]

  ganancia_testing  <- sum(  data[ fold==fold_test ][ prob_baja2 >0.025,  ifelse( clase_ternaria=="BAJA+2", 48750, -1250 ) ] )

  print(paste0("simple->",ganancia_testing))
  
  return( ganancia_testing)
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( data, param, qfolds, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla )

  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a 5 si posee Linux o Mac OS
  print(ganancias)
  
  #devuelvo la primer ganancia y el promedio
  #print(mean( unlist( ganancias )) *  qfolds)
  return(mean( unlist( ganancias )) *  qfolds) 
}
#------------------------------------------------------------------------------

#cargo los datos donde voy a ENTRENAR el modelo
dataset  <- fread("./datasetsOri/paquete_premium_202009.csv")



param_basicos  <- list( "cp"= -1, "minsplit"= 10,"minbucket"= 2,"maxdepth"= 5 )
gan  <- ArbolesCrossValidation( dataset,
                                  param_basicos,
                                  qfolds= 5, # 5-fold cross validation
                                  ksemillas[1] )  #uso solo la primer semilla para particionar el dataset
#gan<-salidacv$gan
modelo<-gan


                          
                          
#prueba ingenuo
dtrain<-dataset
rm(dataset)
gc()
  
prediccion_naive  <- predict( modelo, dtrain , type = "prob") #aplico el modelo
dtrain[ , prob_baja2_naive := prediccion_naive[, "BAJA+2"] ]
dtrain[ , Predicted_naive  := as.numeric(prob_baja2_naive > 0.025) ]
  
prediccion_naive  <- dtrain[   , list(numero_de_cliente, Predicted_naive) ] #genero la salida
  
library(caret)
library(pROC)
salida<-ifelse(dtrain$clase_ternaria=="BAJA+2",1,0)
confusionMatrix(factor(salida),factor(prediccion_naive$Predicted_naive))
zz<-confusionMatrix(factor(salida),factor(prediccion_naive$Predicted_naive))
ganancia<-zz$table[2,2]*48500-zz$table[1,2]*1250
  
#load necessary packages
library(ggplot2)
  
#define object to plot
rocobj <- roc(salida, dtrain$prob_baja2_naive)
  
#create ROC plot
ggroc(rocobj)
auc(rocobj)
  
  
#datos para kaggle
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")
prediccion  <- predict( modelo, dapply , type= "prob") #aplico el modelo
    
#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 
    
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]
    
entrega  <- dapply[  , list(numero_de_cliente, Predicted) ] #genero la salida
    
#genero el archivo para Kaggle
fwrite( entrega, file= paste0( ".kaggle/K_ff",param_basicos[1],param_basicos[2],param_basicos[3],param_basicos[4],  profundidad, ".csv"), sep= "," )
  





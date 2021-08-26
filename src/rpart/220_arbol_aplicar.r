#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/Flavia/Documents/DataScience/dmeyf") 

#cargo los datos donde entreno
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")

#cargar aqui los parametros
# el peor modelo
#-1   100     34    16  -851250
#segundo peor modelo
#-1   100     20    16  -411250
#modelo que da ganancia 0
#0 50 17 4 0

#mejor modelo para cp=-1
#cp=-1, minsplit=15,	minbucket=8,	maxdepth=8	ganancia(4490000)

# el mejor modelo
#0       15         8       14  4753750

# el segundo mejor modelo
#cp=0, minsplit=50,	minbucket=4,	maxdepth=12	ganancia(472750)

# el mejor de profundidad 8
#cp=0, minsplit=50, minbucket=2, maxdepth=8 ganancia(4515000)


parametros  <-  list( "cp"=0, "minsplit"=50,  "minbucket"=2, "maxdepth"=8 )

modelo  <- rpart("clase_ternaria ~ .",
                 data= dtrain,
                 xval= 0,
                 control= parametros )


#ingenuo
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


#cargo los datos donde aplico el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply , type= "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[  , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, 
        file= paste0( "./kaggle/arbol_aplicado.csv"), 
        sep= "," )

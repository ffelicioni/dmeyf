#Arbol elemental con libreria  rpart
require("data.table")
require("rpart")

#Aqui se debe poner la carpeta de la computadora local
setwd("C:/Users/Flavia/Documents/DataScience/dmeyf")  #Establezco el Working Directory

#cargo los datos de 202009 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("./datasetsOri/paquete_premium_202009.csv")
dtrain$clase_ternaria<-factor(dtrain$clase_ternaria)

loss<-matrix(c(0,1,1,1,0,1,1,220,0),ncol=3)

#genero el modelo
set.seed(100) #ff
modelo  <- rpart("clase_ternaria ~ .",
                 data = dtrain[,-c(1:2)],  #saco columnas de id y fechas
                 xval=0,
                 cp=        -0.3, 
                 minsplit=  80,
                 minbucket=  1,
                 maxdepth=   8,
                 parms=list(loss=loss))

# ff- prediccion naive
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

#loss<-matrix(c(0,1,1,1,0,220,1,1,0),ncol=3)
#aplico al modelo  a los datos de 202011

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("./datasetsOri/paquete_premium_202011.csv")

prediccion  <- predict( modelo, dapply , type = "prob") #aplico el modelo

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]
dapply[ , Predicted  := as.numeric(prob_baja2 > 0.025) ]

entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
fwrite( entrega, file="./kaggle/K101_003.csv", sep="," )

entrega3<-entrega

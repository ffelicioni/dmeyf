#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasetsOri/paquete_premium.csv.gz" )


#setwd("C:/Users/Flavia/Documents/DataScience/dmeyf/")
#dataset  <- fread("./datasets/dataset_bajan.csv.gz")

setorder(  dataset,  numero_de_cliente, -foto_mes )   #ordeno, pero a la inversa

dataset[   , morire := 0 ]
dataset[ clase_ternaria=="BAJA+1" , morire := 1 ]  #si tengo un BAJA+1 , ese mes se que voy a morir

dataset[  , morire := cummax( morire ), numero_de_cliente ]   #calculo el maximo acumulado hace atras
dataset[  , meses_muerte := cumsum( morire ), numero_de_cliente ]   #calculo la suma acumulada


dataset[  meses_muerte==0,  meses_muerte := NA ]
dataset[  , morire := NULL ]

dataset1<-dataset[meses_muerte==1, ]

fwrite( dataset1,
        file="datasets/paquete_premium_1_meses_antes.csv.gz",
        sep="\t" )

dataset2<-dataset[meses_muerte==2, ]
fwrite( dataset2,
        file="datasets/paquete_premium_2_meses_antes.csv.gz",
        sep="\t" )

dataset3<-dataset[meses_muerte==3, ]
fwrite( dataset3,
        file="datasets/paquete_premium_3_meses_antes.csv.gz",
        sep="\t" )


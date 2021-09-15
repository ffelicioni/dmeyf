#Optimizacion Bayesiana de hiperparametros de  rpart
#funciona automaticamente con EXPERIMENTOS
#va generando incrementalmente salidas para Kaggle

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("rlist")
require("yaml")

require("rpart")
require("parallel")

#paquetes necesarios para la Bayesian Optimization
require("DiceKriging")
require("mlrMBO")

#install.packages('Rcpp')
library(Rcpp)


#para poder usarlo en la PC y en la nube
switch ( Sys.info()[['sysname']],
         Windows = { directory.root   <-  "C:/Users/Flavia/Documents/DataScience/dmeyf" },   #Microsoft Windows
         Darwin  = { directory.root   <-  "~/dm/" },  #Apple MAC
         Linux   = { directory.root   <-  "~/buckets/b1/crudo/" }  #Entorno Google Cloud
)
#defino la carpeta donde trabajo
#setwd( directory.root )

setwd("C:/Users/Flavia/Documents/DataScience/dmeyf") 


kexperimento  <- NA   #NA si se corre la primera vez, un valor concreto si es para continuar procesando

kscript           <- "320_rpart_BO"
karch_generacion  <- "./datasetsOri/paquete_premium_202009.csv"
karch_aplicacion  <- "./datasetsOri/paquete_premium_202011.csv"
kBO_iter    <-  200   #cantidad de iteraciones de la Optimizacion Bayesiana



hs  <- makeParamSet(
  makeNumericParam("cp"       , lower= -1   , upper=    0.1),
  makeIntegerParam("minsplit" , lower=  1L  , upper= 8000L),  #la letra L al final significa ENTERO
  makeIntegerParam("minbucket", lower=  1L  , upper= 2000L),
  makeIntegerParam("maxdepth" , lower=  3L  , upper=   20L),
  forbidden = quote( minbucket > 0.5*minsplit ) )


ksemilla_azar  <- 100003
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
#------------------------------------------------------------------------------
#graba a un archivo los componentes de lista
#para el primer registro, escribe antes los titulos

loguear  <- function( reg, arch=NA, folder="./work/", ext=".txt", verbose=TRUE )
{
  archivo  <- arch
  if( is.na(arch) )  archivo  <- paste0(  folder, substitute( reg), ext )
  
  if( !file.exists( archivo ) )  #Escribo los titulos
  {
    linea  <- paste0( "fecha\t", 
                      paste( list.names(reg), collapse="\t" ), "\n" )
    
    cat( linea, file=archivo )
  }
  
  linea  <- paste0( format(Sys.time(), "%Y%m%d %H%M%S"),  "\t",     #la fecha y hora
                    gsub( ", ", "\t", toString( reg ) ),  "\n" )
  
  cat( linea, file=archivo, append=TRUE )  #grabo al archivo
  
  if( verbose )  cat( linea )   #imprimo por pantalla
}
#------------------------------------------------------------------------------
#funcion para particionar, es la que Andres reemplaza con caret

particionar  <- function( data, division, agrupa="", campo="fold", start=1, seed=NA )
{
  if( !is.na( seed)  )   set.seed( seed )
  
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x ) }, division, seq( from=start, length.out=length(division) )  ) )
  
  data[ , (campo) :=  sample( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],
        by= agrupa ]
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
  
  return( ganancia_testing )
}
#------------------------------------------------------------------------------

ArbolesCrossValidation  <- function( data, param, qfolds, pagrupa, semilla )
{
  divi  <- rep( 1, qfolds )
  particionar( data, divi, seed=semilla, agrupa=pagrupa )
  
  ganancias  <- mcmapply( ArbolSimple, 
                          seq(qfolds), # 1 2 3 4 5  
                          MoreArgs= list( data, param), 
                          SIMPLIFY= FALSE,
                          mc.cores= 1 )   #se puede subir a 5 si posee Linux o Mac OS
  
  data[ , fold := NULL ]
  #devuelvo la primer ganancia y el promedio
  return( mean( unlist( ganancias )) *  qfolds )   #aqui normalizo la ganancia
}
#------------------------------------------------------------------------------
#esta funcion solo puede recibir los parametros que se estan optimizando
#el resto de los parametros se pasan como variables globales

EstimarGanancia  <- function( x )
{
  GLOBAL_iteracion  <<-  GLOBAL_iteracion + 1
  
  xval_folds  <- 5
  ganancia  <-  ArbolesCrossValidation( dataset, param=x, qfolds= xval_folds, pagrupa="clase_ternaria", semilla=ksemilla_azar )
  
  #si tengo una ganancia superadora, genero el archivo para Kaggle
  if(  ganancia > GLOBAL_ganancia_max )
  {
    GLOBAL_ganancia_max <<-  ganancia  #asigno la nueva maxima ganancia
    
    modelo  <- rpart("clase_ternaria ~ .",
                     data= dataset,
                     xval= 0,
                     control= x )
    
    #genero el vector con la prediccion, la probabilidad de ser positivo
    prediccion  <- predict( modelo, dapply)
    
    prob_baja2  <- prediccion[, "BAJA+2"]
    Predicted   <- ifelse( prob_baja2 > 0.025, 1, 0 )
    
    entrega  <-  as.data.table( list( "numero_de_cliente"=dapply$numero_de_cliente, "Predicted"=Predicted)  )
    
    #genero el archivo para Kaggle
    fwrite( entrega, 
            file= paste0(kkaggle, GLOBAL_iteracion, ".csv" ),
            sep=  "," )
  }
  
  #logueo 
  xx  <- x
  xx$xval_folds  <-  xval_folds
  xx$ganancia  <- ganancia
  loguear( xx,  arch= klog )
  
  
  return( ganancia )
}
#------------------------------------------------------------------------------
#Aqui empieza el programa

if( is.na(kexperimento ) )   kexperimento <- get_experimento()  #creo el experimento

#en estos archivos quedan los resultados
kbayesiana  <- paste0("./work/E",  kexperimento, "_rpart.RDATA" )
klog        <- paste0("./work/E",  kexperimento, "_rpart_log.txt" )
kkaggle     <- paste0("./kaggle/E",kexperimento, "_rpart_kaggle_" )


GLOBAL_ganancia_max  <-  -Inf
GLOBAL_iteracion  <- 0

if( file.exists(klog) )
{
  tabla_log  <- fread( klog)
  GLOBAL_iteracion  <- nrow( tabla_log ) -1
  GLOBAL_ganancia_max  <-  tabla_log[ , max(ganancia) ]
}


#cargo los datasets
dataset  <- fread(karch_generacion)   #donde entreno
# se cambia el dataset

campos_buenos <-  setdiff(  colnames( dataset),  c("numero_de_cliente","internet","tmobile_app") )

datasetA<-dataset[ , campos_buenos,   with=FALSE ]
dataset<-datasetA
rm(datasetA)

dataset$clase_ternaria<-factor(dataset$clase_ternaria)
#dataset$cmobile_app_trx<-as.numeric(dataset$cmobile_app_trx > 0.5)  #al final lo dejo que patine

#------------------------------------------- correccion margen activos
N<-200
library(infotheo)
variable<-dataset$mactivos_margen
bin_eq_freq <- discretize(variable,"equalfreq",N)
# Nos copiamos el atributo original
bin_eq_freq$Y = variable
# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
for(bin in 1:N){
  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = mean(bin_eq_freq$Y[bin_eq_freq$X==bin])
}
dataset$mactivos_margen_rank<-bin_eq_freq$suavizado
rm(bin_eq_freq)

dapply  <- fread(karch_aplicacion)    #donde aplico el modelo
dapply$cliente_antiguedad<-dapply$cliente_antiguedad-2 # se refiere al mes del entrenamiento

variable<-dapply$mactivos_margen
bin_eq_freq <- discretize(variable,"equalfreq",N)
# Nos copiamos el atributo original
bin_eq_freq$Y = variable
# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
for(bin in 1:N){
  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = mean(bin_eq_freq$Y[bin_eq_freq$X==bin])
}
dapply$mactivos_margen_rank<-bin_eq_freq$suavizado
dataset<-dataset[,-c("mactivos_margen")]
dapply<-dapply[,-c("mactivos_margen")]

#------------------------------------- correccion atm_other y atm
variable<-ifelse(dataset$catm_trx_other==0, 0, dataset$matm_other/dataset$catm_trx_other)
dataset$matm_other_extraccion<-variable
variable<-ifelse(dataset$catm_trx==0, 0, dataset$matm/dataset$catm_trx)
dataset$matm_extraccion<-variable


variable<-ifelse(dapply$catm_trx_other==0, 0, dapply$matm_other/dapply$catm_trx_other)
dapply$matm_other_extraccion<-variable
variable<-ifelse(dapply$catm_trx==0, 0, dapply$matm/dapply$catm_trx)
dapply$matm_extraccion<-variable
rm(variable)

dataset<-dataset[,-c("matm","matm_other")]
dapply<-dapply[,-c("matm","matm_other")]


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

dataset<-dataset[,-c("Master_Finiciomora")]
dapply<-dapply[,-c("Master_Finiciomora")]


#Aqui comienza la configuracion de la Bayesian Optimization

configureMlr( show.learner.output = FALSE)

funcion_optimizar  <- EstimarGanancia

#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
#por favor, no desesperarse por lo complejo
obj.fun  <- makeSingleObjectiveFunction(
  fn=       funcion_optimizar,
  minimize= FALSE,   #estoy Maximizando la ganancia
  noisy=    TRUE,
  par.set=  hs,
  has.simple.signature = FALSE
)

ctrl  <- makeMBOControl( save.on.disk.at.time= 600,  save.file.path= kbayesiana)
ctrl  <- setMBOControlTermination(ctrl, iters= kBO_iter )
ctrl  <- setMBOControlInfill(ctrl, crit= makeMBOInfillCritEI())

surr.km  <-  makeLearner("regr.km", predict.type= "se", covtype= "matern3_2", control= list(trace= TRUE))

#inicio la optimizacion bayesiana
if(!file.exists(kbayesiana)) {
  run  <- mbo(obj.fun, learner = surr.km, control = ctrl)
} else  run  <- mboContinue( kbayesiana )   #retomo en caso que ya exista


#quit( save="no" )

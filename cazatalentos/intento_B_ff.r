#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#ideas principales, en cada ronda se acumulan los aciertos de los que siguen en carrera
#la probabilida del primer tiro tiene que superar al 50% para que super a la mediana,
#el segundo tiro deberia superar a 75% ya que es la mediana de la mediana (si se dio el evento ell segundo tiro p>1-(1-p(1))*.5)
#sigue así hasta que hayan tirado los ultimos los 415 tiros y con suerte el hganador llego hasta ahí


#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )
  
  return( res )
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}
#------------------------------------------------------------------------------


Estrategia_B  <- function()
{
  #Estrategia
  #Se juegan varias rondas
  #En cada ronda, los jugadores que participan, tiran x tiros
  #De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  #Se elige el mejor jugador de la septima ronda

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  ctiros<-c(85,61,61,61,61,61,30) 
  
  #ctiros<-c(115,61,61,61,61,61,30) 
  
  
  ids_juegan_ini  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan_ini,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini,  acumulados := 0]  #registro en la planilla
  
  for (i in 1:1){
    planilla_cazatalentos[ ids_juegan_ini,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
    resultado  <- gimnasio_tirar( ids_juegan_ini, ctiros[i])
    
    planilla_cazatalentos[ ids_juegan_ini,  c(paste0('aciertos',i))  := resultado ]  
    planilla_cazatalentos[ ids_juegan_ini,  tiros_acum := tiros_acum+ctiros[i]]  
    planilla_cazatalentos[ ids_juegan_ini,  acumulados := acumulados+resultado]  
    #planilla_cazatalentos[ ids_juegan_ini,  c(paste0('p',i)) := acumulados/tiros_acum]  #para calcular la tasa de aciertos si uno quisiera
    #planilla_cazatalentos[ ids_juegan_ini,  c(paste0('s',i)):=sqrt(acumulados)/tiros_acum] #para calcular el desvio si uno quisiera

    mediana  <- planilla_cazatalentos[ ids_juegan_ini, median(acumulados) ]
    ids_juegan_fin  <- planilla_cazatalentos[ ids_juegan_ini][ acumulados >= mediana, id ]
    ids_juegan_ini<-ids_juegan_fin
  }
  

  pos_mejor <-  planilla_cazatalentos[ , which.max(acumulados) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 100003 )  #debe ir una sola vez, ANTES de los experimentos
#set.seed(123456)

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy
  
  veredicto  <- Estrategia_B()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total 
tasa_eleccion_correcta




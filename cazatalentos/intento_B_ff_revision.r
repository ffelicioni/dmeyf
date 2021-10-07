#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

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
  #En cada ronda, los jugadores que participan, tiran 70 tiros
  #De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  #Se elige el mejor jugador de la sexta ronda

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  ctiros<-c(90,150-90,210-150,261-210,320-261,415-375) # 375-320,
  ctiros<-c(89,151-89,212-151,280-212,340-280,410-340) # 375-320,
  
  ctiros<-c(65,132-65,198-132,272-198,344-272,420-344) # 375-320,
  ctiros<-c(63,125-63,187-125,250-187,312-250,420-312) # 375-320,
  ctiros<-c(90,153-90,210-154,261-210,320-261,375-320) # 375-320,
  
  #0.880407895600534   1.528208998927585   2.176010102254637   2.823811205581689   3.471612308908740   4.119413412235792
  #0.942036448292571   1.635183628852516   2.328330809412461   3.021477989972407 3.714625170532352   4.407772351092297
  ctiros<-c(88,152-88,217-152,282-217,347-282,412-347) # 375-320,
  ctiros<-c(90,153-90,217-153,273-217,325-273,412-325) # 375-320,
  
  ctiros<-c(90,150-90,217-150,273-217,325-273,412-325) # 375-320, este dio bien en una prediccion
  #ctiros<-c(89,151-89,217-151,273-217,325-273,412-325) # 375-320,
  #ctiros<-c(90,63,65,56,52,87) # 375-320,
  
  #ctiros<-c(90,60,65,58,52,87) # 375-320,
  
  ids_juegan1  <- 1:100
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   70  tiros libres cada uno
  planilla_cazatalentos[ ids_juegan1,  tiros1 := ctiros[1]]  #registro en la planilla que tiran 90 tiros
  #Hago que tiren
  resultado1  <- gimnasio_tirar( ids_juegan1, ctiros[1])
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan1,acumulado := aciertos1 ]  #registro en la planilla
  
  
  #Rondas -------------------------------------------------------
  
  for (i in 2:6){
    
    torneo_anterior<-c(paste0('aciertos',i))
    #los mejores 40 jugadores tiran 400 tiros cada uno
    mediana  <- planilla_cazatalentos[ ids_juegan1, median(acumulado) ]
    ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][acumulado > mediana, id ]
    
    #print(length(ids_juegan2))
    #ids_juegan2<-1:100 
    N2<-ctiros[i]
    
    
    planilla_cazatalentos[ ids_juegan2,  c(paste0('tiros',i)) := N2 ]  #registro en la planilla que tiran 400 tiros
    resultado2  <- gimnasio_tirar( ids_juegan2, N2)
    #torneo_actual<-c(paste0('aciertos',i+1))
    planilla_cazatalentos[ ids_juegan2,  c(paste0('aciertos',i)) := resultado2 ]  #registro en la planilla
    
    #planilla_cazatalentos[,p2:=aciertos2/tiros2]
    #planilla_cazatalentos[,s2:=sqrt(aciertos2/tiros2)/tiros2]
    
    #planilla_cazatalentos[,p3:=(aciertos2+aciertos1)/(tiros2+tiros1)]
    
    
    N1<-N2
    ids_juegan1<-ids_juegan2
    print(ids_juegan1)
    N<-N+length(ids_juegan1)*N2
    planilla_cazatalentos[ ids_juegan2,  acumulado := acumulado+resultado2 ]  #registro en la planilla
  }
  
  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(acumulado) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 100003 )  #debe ir una sola vez, ANTES de los experimentos

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:1 )
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




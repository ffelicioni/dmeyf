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
  #ctiros<-c(85)
  
  ctiros<-c(75,61,61,61,61,61,30) 
  
  ctiros<-c(85,61,61,61,61,61,30) 
  #ctiros<-c(50,50,50,50,50,50,50,70) 
  #ctiros<-c(115,61,61,61,61,61,30) 
  
  
  ids_juegan_ini  <- 1:100   #los jugadores que participan en la ronda,
  ids_juegan_ini1  <- 1:50   #los jugadores que participan en la ronda,
  ids_juegan_ini2  <- 51:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan_ini1,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini1,  acumulados1 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini2,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini2,  acumulados2 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini,  acumulados := 0]  #registro en la planilla
  
  media1<-list()
  media2<-list()
  
  usa_grupo1<-T
  usa_grupo2<-T
  
  #t=c(0.718,0.677,0.815,1.026,1.1374,1.2384,1.282)
  t<-c(.0178,0.6745,0.6745,0.6745,0.6745,0.6745,1.282)
  for (i in 1:7){
    
    print(paste0('ronda',i))
    maximos<-list()
    desvios<-list()
    #planilla_cazatalentos<-planilla_cazatalentos[,fold:=]
    #indices_part1<-ids_juegan_ini1[seq(1,floor(length(ids_juegan_ini)/2))]
    #indices_part2<-ids_juegan_ini2[seq(floor(length(ids_juegan_ini)/2)+1,length(ids_juegan_ini))]
    if (usa_grupo1==T){
      
      #media_pre1<-planilla_cazatalentos[ ids_juegan_ini1,  mean(acumulados1/ tiros_acum)]
      
      resultado1  <- gimnasio_tirar( ids_juegan_ini1, ctiros[i])
      planilla_cazatalentos[ ids_juegan_ini1,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini1,  c(paste0('aciertos',i))  := resultado1 ]  
      planilla_cazatalentos[ ids_juegan_ini1,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini1,  acumulados1 := acumulados1+resultado1] 
      planilla_cazatalentos[ ids_juegan_ini1,  c(paste0('p',i)) := acumulados1/tiros_acum]  
      planilla_cazatalentos[ ids_juegan_ini1,  c(paste0('s',i)):=sqrt(acumulados1)/tiros_acum]
      planilla_cazatalentos[ ids_juegan_ini1,  acumulados := acumulados+resultado1] 
      
      mediana1  <- planilla_cazatalentos[ ids_juegan_ini1, median(acumulados1) ]
      ids_juegan_fin1  <- planilla_cazatalentos[ ids_juegan_ini1][ acumulados1 >= mediana1, id ]
      ids_juegan_ini1<-ids_juegan_fin1
      
      media_pos1<-planilla_cazatalentos[ ids_juegan_ini1,  mean(acumulados1/ tiros_acum)]
      
      
      indice_max1<-planilla_cazatalentos[ ,  which.max(acumulados1/tiros_acum)]
      id_max1  <-  planilla_cazatalentos[ indice_max1, id ]
      max_actual1<-planilla_cazatalentos[ id_max1, acumulados1/tiros_acum] 
      #print(id_max1)
      #print(max_actual1)
      desvio_actual1<-planilla_cazatalentos[ id_max1, sqrt(acumulados1)/tiros_acum] 
      media1<-c(unlist(media1),media_pos1)
      desvios<-c(unlist(desvios),desvio_actual1)
    }
    
    if (usa_grupo2==T){
      
      #media_pre2<-planilla_cazatalentos[ ids_juegan_ini2,  mean(acumulados2/ tiros_acum)]
      
      resultado2  <- gimnasio_tirar( ids_juegan_ini2, ctiros[i])
      planilla_cazatalentos[ ids_juegan_ini2,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini2,  c(paste0('aciertos',i))  := resultado2 ]  
      planilla_cazatalentos[ ids_juegan_ini2,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini2,  acumulados2 := acumulados2+resultado2] 
      planilla_cazatalentos[ ids_juegan_ini2,  acumulados := acumulados+resultado2] 
      planilla_cazatalentos[ ids_juegan_ini2,  c(paste0('p',i)) := acumulados2/tiros_acum]  
      planilla_cazatalentos[ ids_juegan_ini2,  c(paste0('s',i)):=sqrt(acumulados2)/tiros_acum]
      
      
      
      mediana2  <- planilla_cazatalentos[ ids_juegan_ini2, median(acumulados2) ]
      ids_juegan_fin2  <- planilla_cazatalentos[ ids_juegan_ini2][ acumulados2 >= mediana2, id ]
      ids_juegan_ini2<-ids_juegan_fin2
      
      media_pos2<-planilla_cazatalentos[ ids_juegan_ini2,  mean(acumulados/ tiros_acum)]
      
      indice_max2<-planilla_cazatalentos[ ,  which.max(acumulados2/tiros_acum)]
      id_max2  <-  planilla_cazatalentos[ indice_max2, id ]
      max_actual2<-planilla_cazatalentos[ id_max2, acumulados2/tiros_acum] 
      #print(id_max2)
      #print(max_actual2)
      desvio_actual2<-planilla_cazatalentos[ id_max2, sqrt(acumulados2)/tiros_acum] 
      
      #maximos<-c(unlist(maximos),media_pos2)
      media2<-c(unlist(media2),media_pos2)
      desvios<-c(unlist(desvios),desvio_actual2)
    }
    #print(i)
    #print(mediana1)
    #print(planilla_cazatalentos[ ids_juegan_ini1, mean(acumulados1/tiros_acum) ])
    #print(mediana2)
    #print(planilla_cazatalentos[ ids_juegan_ini2, mean(acumulados2/tiros_acum) ])
    
    #media1<-c(unlist(media1),planilla_cazatalentos[ ids_juegan_ini1, mean(acumulados1/tiros_acum) ])
    #media2<-c(unlist(media2),planilla_cazatalentos[ ids_juegan_ini2, mean(acumulados2/tiros_acum) ])
    #if 
    
    
    #print(media_pre1)
    print(media1)
    
    #print(media_pre2)
    print(media2)
    
    if (i==3) {
      print(i)
      indice_max<-which.max(maximos)
      indice_min<-which.min(maximos)
      #print(maximos)
      #print(desvios)
      #print(maximos[indice_max]-t[i]*desvios[indice_max])
      #print(maximos[indice_min]+t[i]*desvios[indice_min])
      #print((maximos[indice_max]-t[i]*desvios[indice_max]>maximos[indice_min]+t[i]*desvios[indice_min]))
      #if (maximos[indice_max]>maximos[indice_min]) {
      #if (maximos[indice_max]-t[i]*desvios[indice_max]>maximos[indice_min]+t[i]*desvios[indice_min]) {
      
      #(media1[2]-media1[1])/media1[1]
      #(media2[2]-media2[1])/media2[1]  
        if (media1[3]>media2[3]){
          usa_grupo2<-F
        } else {
          usa_grupo1<-F
        }  
      } 
    #}
    #media2<-c(unlist(media2),planilla_cazatalentos[ ids_juegan_ini2, mean(acumulados2/tiros_acum) ])
    #print(media1)
    #print(media2)
    #print(length(ids_juegan_ini1))
    #print(length(ids_juegan_ini2))
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

for( experimento  in  1:100 )
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





#para reducir el problema de "multiples comparaciones" se hizo una particion aleatoria de los tiros
#de los participantes de la primera ronda  y luego en cada subgrupo se elegio la mitad ganadora
#se juntaron los mejores de cada subgrupo para pasar a la segunda ronda


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

#se particionan los datos para conseguir particiones de qfolds (grupos con igual cantidad de individuos y disjuntos)
#pero se quito aleatoriedad

particionar  <- function( data, division, agrupa="", campo="fold", start=1)
{
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  ( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],by= agrupa ]
}

#------------------------------------------------------------------------------

Estrategia_B  <- function()
{
  #Estrategia
  #Se juegan varias rondas
  #En cada ronda, los jugadores que participan, tiran ctiros 
  #De una ronda a la otra, solo pasan aquello que tuvieron igual o mayores aciertos respecto de la mediana de aciertos de la ronda anterior
  #pero de cada grupo reducido de jugadores, por ensayos se eligio particionar en 5 (conjuntos disjuntos con la funcion particionar)
  #Se elige el mejor jugador de la ronda que sea

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  ctiros<-c(85,60,60,60,60,60) # primera ronda 90 tiros, segunda 60
  
  ctiros<-c(85,57,60,273-217,325-273,412-325) # 375-320,
  
  ctiros<-c(30,60,60,60,273-217,325-273,412-325) # 375-320,
  
  ctiros<-c(25,20,40,62,60,60,60,85)
  #Ronda 1  ------------------------------------------------------
  #tiran los 100 jugadores es decir 1:100   90  tiros libres cada uno
  
  ids_juegan1  <- 1:100   #los jugadores que participan en la primra ronda,
  
  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := ctiros[1] ]  #registro en la planilla que tiran 90 tiros
  resultado1  <- gimnasio_tirar( ids_juegan1, ctiros[1])
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan1,  acumulados := aciertos1]  #registro en la planilla
  
  
  #Ronda 2 -------------------------------------------------------
  # Para decidir quienes pasan a la segunda ronda calculo la mediana en cada
  # una de las 5 rondas de particiones, y luego concateno el vector de los indices 
  
  #ids_juegan1  <- 1:100  
  
  mediana  <- planilla_cazatalentos[ ids_juegan1, median(acumulados) ]
  mediana  <- planilla_cazatalentos[ ids_juegan1, quantile(acumulados,  probs = .3666) ]
  
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ acumulados >= mediana, id ]
  
  #print(planilla_cazatalentos[fold==1,id])
  
  #ids_juegan2<-sort(unlist(ids_jueganx))
  
  #print(ids_juegan1)
  
  #print(planilla_cazatalentos[ ids_juegan2, mean(aciertos1)])
  #print(length(ids_juegan2))
  
  #print(planilla_cazatalentos[ids_juegan2,])
  #planilla_cazatalentos[,fold:=NA]  #reinicio la numeracion de folds
  #planilla_cazatalentos<-planilla_cazatalentos[, !"fold"]
  #print(planilla_cazatalentos[ids_juegan2,])
  
  #segunda ronda, en esta ronda el cazatalentos agrega los aciertos de la segunda ronda a los que pasaron de la primera
  planilla_cazatalentos[ ids_juegan2,  tiros2 := ctiros[2] ]  #registro en la planilla que tiran ctiros[2]
  resultado2  <- gimnasio_tirar( ids_juegan2, ctiros[2])
  planilla_cazatalentos[ ids_juegan2,  aciertos2 := resultado2 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan2,  acumulados := aciertos2+acumulados]  #registro en la planilla los acumulados para decidir
  
  #print(planilla_cazatalentos[ ids_juegan2, mean(aciertos2)])
  
  #print(length(ids_juegan2))
  #grupo<-rep(seq(1:2),25)
  #length(planilla_cazatalentos[ids_juegan2])
  #planilla_cazatalentos[ids_juegan2,fold:=grupo]
  
  #Ronda 3 -------------------------------------------------------
  #preparo datos para la tercera ronda pero no se necesito tirar ;-)
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan2, median(acumulados) ]
  mediana  <- planilla_cazatalentos[ ids_juegan2, quantile(acumulados,  probs = 0.25) ]
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2][ acumulados >= mediana, id ]
  
  planilla_cazatalentos[ ids_juegan3,  tiros3 := ctiros[3] ]  #registro en la planilla que tiran 70 tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, ctiros[3])
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan3,  acumulados := acumulados+aciertos3]  #registro en la planilla
  
  #Ronda 4 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  
  mediana  <- planilla_cazatalentos[ ids_juegan3, median(acumulados) ]
  mediana  <- planilla_cazatalentos[ ids_juegan3, quantile(acumulados,  probs = .25) ]
  
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ acumulados >= mediana, id ]
  
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := ctiros[4]]  #registro en la planilla que tiran 70 tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, ctiros[4])
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan4,  acumulados := acumulados+aciertos4]  #registro en la planilla
  
  
  #Ronda 5 -------------------------------------------------------
  #A la mitad mejor la hago tirar 70 tiros cada uno
  #La mediana siempre parte a un conjunto en dos partes de igual cantidad
  mediana  <- planilla_cazatalentos[ ids_juegan4, median(acumulados) ]
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ acumulados >= mediana, id ]
  
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := ctiros[5]]  #registro en la planilla que tiran 70 tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, ctiros[5])
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan5,  acumulados := acumulados+aciertos5]  #registro en la planilla
  
  
  #ronda 6
  mediana  <- planilla_cazatalentos[ ids_juegan5, median(acumulados) ]
  ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][ acumulados >= mediana, id ]
  print(length(ids_juegan6))
  
  
  
  #planilla_cazatalentos[ ids_juegan6,  tiros6 := ctiros[6]]  #registro en la planilla que tiran 200 tiros
  #resultado6  <- gimnasio_tirar( ids_juegan6, ctiros[6])
  #planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ]  #registro en la planilla
  #planilla_cazatalentos[ ids_juegan6,  acumulados := acumulados+aciertos6]  #registro en la planilla
  

  #mediana  <- planilla_cazatalentos[ ids_juegan6, median(acumulados) ]
  #ids_juegan7 <- planilla_cazatalentos[ ids_juegan6 ][ acumulados >= mediana, id ]
  
  

  #planilla_cazatalentos[ids_juegan5,media:=aciertos1/ctiros[1]+aciertos2/ctiros[2]+aciertos3/ctiros[3]+aciertos4/ctiros[4]+aciertos5/ctiros[5]]

  #Epilogo
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en ambas rondas
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




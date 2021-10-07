#intencionalmente el mejor jugador va al final de la lista de jugadores
#porque la funcion which.max() de R hace trampa
#si hay un empate ( dos máximos) se queda con el que esta primero en el vector

require("data.table")

set.seed( 100003 )
#set.seed(102191)

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

#defino los jugadores
mejor      <-  0.7
peloton    <-  ( 501:599 ) / 1000
peloton    <-  seq(501,599,4) / 1000
peloton<-seq(501,599,10)/1000

jugadores  <-  c(peloton,mejor) #intencionalmente el mejor esta al comienzo

#veo que tiene el vector
jugadores

#hago que los 100 jugadores tiren 10 veces cada uno
#mapply(  ftirar, jugadores, 10 )

tb_resultados  <- data.table(tiros=numeric())

#c( 86,87,88,89,90,91,92,93,146,147,148,149,150,151,152,155,160,208,209,210,211,212,214,215,220,256,257,258,259,260,261,262,263,264,265,268,269,270,271,280,310,312,313,314,315,316,317,318,319,320,321,322,323,324,340,361,362,363,364,365,366,367,368,369,370,371,372,373,374,375,380,390,410,411,412,413,414,415,416,417,418) 
for(  tiros_libres  in seq(30,100)) #10, 20, 50, 100, 200,300,400, 415, 460, 600, 700, 1000 
{
#162
  primero_ganador  <- 0

  for( i in 1: 5000)  #diez mil experimentos 10000
  {
    vaciertos <- mapply( ftirar, jugadores, tiros_libres ) 
    #print(vaciertos)
    tb_resultados  <- rbind( tb_resultados, list(vaciertos[11]))
    mejor  <- which.max( vaciertos )
    if( mejor == 11)  primero_ganador <- primero_ganador + 1
  }

  cat( tiros_libres, primero_ganador/5000, "\n" )

}


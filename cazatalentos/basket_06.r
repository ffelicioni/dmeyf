require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

mejor   <- 0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton, mejor )  #intencionalmente el mejor esta al final


for( i in 1:100 )
{
   vaciertos  <- mapply( ftirar, jugadores, 100 )  #cada jugador tira 100 tiros libres

   mejor  <- which.max( vaciertos )

   print(mejor)
   aciertos_torneo  <-  vaciertos[ mejor ]

   aciertos_segunda  <- ftirar( jugadores[mejor], 100 )

   cat( mejor, aciertos_torneo, aciertos_segunda, "\n" )
}
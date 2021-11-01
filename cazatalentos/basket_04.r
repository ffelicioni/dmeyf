require("data.table")

set.seed( 100003 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}


jugadores  <- rep( 0.7, 10 )  #jugadores identicos


for( i in 1:100 )
{
   vaciertos  <- mapply( ftirar, jugadores, 100 )  #cada jugador tira 100 tiros libres

   mejor  <- which.max( vaciertos )
    print(mejor)
   aciertos_torneo  <- vaciertos[ mejor ]

   print(mean(aciertos_torneo))
   aciertos_segunda  <- ftirar( jugadores[mejor], 100 )
   print(mean(aciertos_segunda))
   
   cat( aciertos_torneo, aciertos_segunda, "\n" )
   
}
require("data.table")

set.seed( 102191 )

#calcula cuantos encestes logra un jugador con indice de enceste prob que hace qyt tiros libres
ftirar <- function( prob, qty )
{
  return(  sum( runif(qty) < prob ) )
}

particionar  <- function( data, division, agrupa="", campo="fold", start=1)
{
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  ( rep( bloque, ceiling(.N/length(bloque))) )[1:.N],by= agrupa ]
}

jugadores  <- rep( 0.7, 100 )  #jugadores identicos
mejor   <- 0.7
peloton    <-  ( 501:599 ) / 1000
jugadores  <-  c( peloton,mejor )  #intencionalmente el mejor esta al final

diferencias  <- c()

for( i in 1:9999 )
{
   vaciertos  <- mapply( ftirar, jugadores, 100 )  #cada jugador tira 100 tiros libres

   #qfolds<-50
   #divi  <- rep( 1, qfolds )
   
   #particionar(planilla_cazatalentos,divi)  #la particion no es aleatoria
   
   #grupo<-rep(seq(1:qfolds),length(ids_juegan1)/qfolds)
   #planilla_cazatalentos[,fold:=grupo]
   #ids_jueganx<-list()
   #for (i in 1:qfolds ) {
   #  ids_jueganf<-planilla_cazatalentos[ fold==i, id]
   #  mediana  <- planilla_cazatalentos[ ids_jueganf, median(acumulados) ]
   #  ids  <- planilla_cazatalentos[ ids_jueganf ][ acumulados >= mediana, id ]
   #  ids_jueganx<-c(unlist(ids_jueganx),ids)
   #}
   
   mejor  <- which.max( vaciertos )

   aciertos_torneo  <- vaciertos[ mejor ]

   aciertos_segunda  <- ftirar( jugadores[mejor], 100 )

   diferencias  <- c( diferencias, aciertos_torneo-aciertos_segunda)
}


mean( diferencias )
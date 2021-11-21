#Necesita para correr en Google Cloud
#32 GB de memoria RAM
#256 GB de espacio en el disco local
#8 vCPU

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#Aqui comienza el programa
setwd("~/buckets/b1")

#cargo el dataset donde voy a entrenar
dataset  <- fread("./datasets/dataset_epic_v952_ranking.csv.gz")

#ordeno el dataset
setorder( dataset,  foto_mes )

campos_buenos  <-  setdiff(  colnames( dataset),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )


#pdf("./work/zeroes_ratio_rank.pdf")
#for( campo in  campos_buenos )
#{
#  tbl <- dataset[ foto_mes<=202101 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]

#  ymin <-  min( tbl$zero_ratio )
#  ymax <-  max( tbl$zero_ratio )
#  if( ymin == 0 )  ymin <- -0.1
#  if( ymax == 0 )  ymax <-  0.1

#  plot(x= 1:nrow(tbl),
#       y= tbl$zero_ratio,
#       type= "o",
#       main= paste0("Zeroes ratio  -  ",  campo),
#       xlab= "Periodo",
#       ylab= "Zeroes  ratio",
#       ylim= c( ymin, ymax ),
#       xaxt= "n"
#     )

#  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)

#abline(v=c(35), col=c("red"), lty=c(1), lwd=c(1))

  #abline(v=c(1,13,25,37), col=c("green","green","green","green"), lty=c(1,1,1,1), lwd=c(1,1,1,1))
  #abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
#}
#dev.off()





pdf("./work/promedios_rank.pdf")
for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202101 ,  list( "promedio" = mean( get(campo), na.rm=TRUE)) , foto_mes ]
  ceros <- dataset[ foto_mes<=202101 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
  plot(x= 1:nrow(tbl),
       y= tbl$promedio,
       type= "o",
       main= paste0("Promedios  -  ",  campo),
       xlab= "Periodo",
       ylab= "Promedio",
       xaxt= "n"
     )

  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)

  abline(v=c(1,13,25,37), col=c("green","green","green","green"), lty=c(1,1,1,1), lwd=c(1,1,1,1))
  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
  
  for( i in 1:nrow(tbl) )
  {
    if( ceros[ i, zero_ratio]> 0.99 &  median(ceros[ , zero_ratio]) < 0.99 )
    {
      abline(v=c(i), col=c("red"), lty=c(1), lwd=c(1))
    }
  }
}
dev.off()

#como agrupar ranks segun el cliente
zz<-dataset[,mean(mpayroll_rankN),by=numero_de_cliente]
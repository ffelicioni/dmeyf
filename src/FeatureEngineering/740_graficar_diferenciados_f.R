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
dataset2  <- fread("./datasetsOri/paquete_premium.csv.gz")

#ordeno el dataset
setorder( dataset2,  foto_mes )

campos_buenos  <-  setdiff(  colnames( dataset2),  c("numero_de_cliente","foto_mes","clase_ternaria" ) )

id_baja2<-dataset2[clase_ternaria=='BAJA+2','numero_de_cliente'] #los que se van a los 2 meses
id_baja1<-dataset2[clase_ternaria=='BAJA+1','numero_de_cliente'] #los que se van al mes
id_bajas<-unique(rbind(id_baja2,id_baja1)) #me quedo con los indices unicos

dataset<-dataset2[numero_de_cliente %in% id_bajas$numero_de_cliente,]

pdf("./work/zeroes_ratio_bajas.pdf")
for( campo in  campos_buenos )
{
  tbl <- dataset[ foto_mes<=202012 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
  ymin <-  min( tbl$zero_ratio )
  ymax <-  max( tbl$zero_ratio )
  if( ymin == 0 )  ymin <- -0.1
  if( ymax == 0 )  ymax <-  0.1
  
  plot(x= 1:nrow(tbl),
       y= tbl$zero_ratio,
       type= "o",
       main= paste0("Zeroes ratio  -  ",  campo),
       xlab= "Periodo",
       ylab= "Zeroes  ratio",
       ylim= c( ymin, ymax ),
       xaxt= "n"
  )
  
  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)
  
  abline(v=c(35), col=c("red"), lty=c(1), lwd=c(1))
  
  abline(v=c(1,13,25,37), col=c("green","green","green","green"), lty=c(1,1,1,1), lwd=c(1,1,1,1))
  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
}
dev.off()



pdf("./work/nas_ratio_bajas.pdf")
for( campo in  campos_buenos )
{
  tbl <- dataset[ foto_mes<=202012 ,  list( "na_ratio" = sum( is.na(get(campo)), na.rm=TRUE)/.N ) , foto_mes ]
  
  ymin <-  min( tbl$na_ratio )
  ymax <-  max( tbl$na_ratio )
  if( ymin == 0 )  ymin <- -0.1
  if( ymax == 0 )  ymax <-  0.1
  
  plot(x= 1:nrow(tbl),
       y= tbl$na_ratio,
       type= "o",
       main= paste0("NAs ratio  -  ",  campo),
       xlab= "Periodo",
       ylab= "NAs  ratio",
       ylim= c( ymin, ymax ),
       xaxt= "n"
  )
  
  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)
  
  abline(v=c(35), col=c("red"), lty=c(1), lwd=c(1))
  
  abline(v=c(1,13,25,37), col=c("green","green","green","green"), lty=c(1,1,1,1), lwd=c(1,1,1,1))
  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
}
dev.off()



pdf("./work/promedios_bajas.pdf")
for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202012 ,  list( "promedio" = mean( get(campo), na.rm=TRUE)) , foto_mes ]
  ceros <- dataset[ foto_mes<=202012 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
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



#pdf("./work/promedios_nocero_bajas.pdf")
#for( campo in  campos_buenos )
#{
#  tbl   <- dataset[ foto_mes<=202012 & get(campo)!=0,  list( "promedio" = mean( get(campo), na.rm=TRUE)) , foto_mes ]
#  ceros <- dataset[ foto_mes<=202012 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
#  plot(x= 1:nrow(tbl),
#       y= tbl$promedio,
#       type= "o",
#       main= paste0("Promedios NO cero -  ",  campo),
#       xlab= "Periodo",
#       ylab= "Promedio valores no cero",
#       xaxt= "n"
#  )
  
#  axis(1, at=1:nrow(tbl), labels=tbl$foto_mes)
  
#  abline(v=c(1,13,25,37), col=c("green","green","green","green"), lty=c(1,1,1,1), lwd=c(1,1,1,1))
#  abline(v=c(7,19,31), col=c("green","green","green"), lty=c(3,3,3), lwd=c(1,1,1))
  
#  for( i in 1:nrow(tbl) )
#  {
#    if( ceros[ i, zero_ratio]> 0.99 &  median(ceros[ , zero_ratio]) < 0.99 )
#    {
#      abline(v=c(i), col=c("red"), lty=c(1), lwd=c(1))
#    }
#  }
#}
#dev.off()


pdf("./work/medianas_bajas.pdf")
for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202012 ,  list( "mediana" = quantile( get(campo),probs=0.5,names=FALSE, na.rm=TRUE)) , foto_mes ]
  ceros <- dataset[ foto_mes<=202012 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
  plot(x= 1:nrow(tbl),
       y= tbl$mediana,
       type= "o",
       main= paste0("Medianas  -  ",  campo),
       xlab= "Periodo",
       ylab= "mediana",
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


pdf("./work/quantile_25_bajas.pdf")
for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202012 ,  list( "q25" = quantile( get(campo),probs=0.25,names=FALSE, na.rm=TRUE)) , foto_mes ]
  ceros <- dataset[ foto_mes<=202012 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
  plot(x= 1:nrow(tbl),
       y= tbl$q25,
       type= "o",
       main= paste0("Quantile 25%  -  ",  campo),
       xlab= "Periodo",
       ylab= "Quantile25",
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

pdf("./work/quantile_75_bajas.pdf")
for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202012 ,  list( "q75" = quantile( get(campo),probs=0.75,names=FALSE, na.rm=TRUE)) , foto_mes ]
  ceros <- dataset[ foto_mes<=202012 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
  plot(x= 1:nrow(tbl),
       y= tbl$q75,
       type= "o",
       main= paste0("Quantile 75%  -  ",  campo),
       xlab= "Periodo",
       ylab= "Quantile 75%",
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


pdf("./work/quantile_10_bajas.pdf")
for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202012 ,  list( "q10" = quantile( get(campo),probs=0.1,names=FALSE, na.rm=TRUE)) , foto_mes ]
  ceros <- dataset[ foto_mes<=202012 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
  plot(x= 1:nrow(tbl),
       y= tbl$q10,
       type= "o",
       main= paste0("Quantile 10%  -  ",  campo),
       xlab= "Periodo",
       ylab= "Quantile 10%",
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


pdf("./work/quantile_90_bajas.pdf")
for( campo in  campos_buenos )
{
  tbl   <- dataset[ foto_mes<=202012 ,  list( "q90" = quantile( get(campo),probs=0.9,names=FALSE, na.rm=TRUE)) , foto_mes ]
  ceros <- dataset[ foto_mes<=202012 ,  list( "zero_ratio" = sum(get(campo)==0, na.rm=TRUE)/.N ) , foto_mes ]
  
  plot(x= 1:nrow(tbl),
       y= tbl$q90,
       type= "o",
       main= paste0("Quantile 90%  -  ",  campo),
       xlab= "Periodo",
       ylab= "Quantile 90%",
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
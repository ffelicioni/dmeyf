#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")


#cargo los datasets que voy a comparar
#setwd("~/buckets/b1/crudoB/" )  #establezco la carpeta donde voy a trabajar
setwd("C:/Users/Flavia/Documents/DataScience/dmeyf") 


datasetA  <- fread( "./datasetsOri/paquete_premium_202009.csv" )
datasetB  <- fread( "./datasetsOri/paquete_premium_202011.csv" )

datasetB$cliente_antiguedad<-datasetB$cliente_antiguedad-2
#los campos sobre los que voy a trabajar
campos_buenos <-  c("cliente_antiguedad") #se corrigio
campos_buenos <-  c("tmobile_app")        #se saco
campos_buenos<-c("cmobile_app_trx")   #se corrigio
sum(datasetA[ , get(campos_buenos)])
sum(datasetB[ , get(campos_buenos)])
summary(datasetA$cmobile_app_trx)
summary(datasetB$cmobile_app_trx)
datasetA$cmobile_app_trx<-as.numeric(datasetA$cmobile_app_trx > 0.5) 


campos_buenos<-c("mactivos_margen","mpasivos_margen")
N<-200

library(infotheo)
variable<-datasetA$mactivos_margen
bin_eq_freq <- discretize(variable,"equalfreq",N)
# Nos copiamos el atributo original
bin_eq_freq$Y = variable
# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
for(bin in 1:N){
  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = mean(bin_eq_freq$Y[bin_eq_freq$X==bin])
}

datasetA$mactivos_margen_rank<-bin_eq_freq$X #

datasetA$mactivos_margen_rank<-datasetA$mactivos_margen+datasetA$mcomisiones

datasetA$mactivos_margen_rank<-bin_eq_freq$suavizado


#datasetA$mact_pas_margen<-cut(datasetA[ , get(campos_buenos[1])], breaks = c(-114949,-1015,-231,31,190050),labels=c(1,2,3,4)) #
#datasetB$mact_pas_margen<-rowSums(datasetB[ , c("mactivos_margen","mpasivos_margen")]) #

rm(bin_eq_freq)

variable<-datasetB$mactivos_margen
bin_eq_freq <- discretize(variable,"equalfreq",N)
# Nos copiamos el atributo original
bin_eq_freq$Y = variable

# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
for(bin in 1:N){
  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = mean(bin_eq_freq$Y[bin_eq_freq$X==bin])
}
datasetB$mactivos_margen_rank<-bin_eq_freq$X #

datasetB$mactivos_margen_rank<-datasetB$mactivos_margen+datasetB$mcomisiones

datasetB$mactivos_margen_rank<-bin_eq_freq$suavizado

zz<-rank(round(datasetA$mactivos_margen),ties.method="min")

datasetA$mactivos_margen_rank<-datasetA$mactivos_margen+rep(-774.1267,235354)

zz<-rank(round(datasetB$mactivos_margen),ties.method="min")

zz<-datasetB$mactivos_margen
datasetB$mactivos_margen_rank<-zz
campos_buenos<-c("mactivos_margen_rank")


#datasetA$mactivos_margen_rank<-mactivos_todos_rank[1:235354]
#datasetB$mactivos_margen_rank<-mactivos_todos_rank[235355:474340]
#variable<-datasetA$mactivos_margen
#bin_eq_freq <- discretize(variable,"equalfreq",20)
# Nos copiamos el atributo original
#bin_eq_freq$Y = variable
#plot(bin_eq_freq$Y,bin_eq_freq$X)
# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
#for(bin in 1:20){
#  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = mean(bin_eq_freq$Y[bin_eq_freq$X==bin])
#}
# grafico ordenado de menor a mayor
plot(sort(variable) , type = "l", col="red", xlab = "Observaciones", ylim = c(-50000,50000), main = "Dato original vs suavizado")
# Agrego la serie de la variable media 
lines(sort(bin_eq_freq$suavizado), type = "l", col="blue")
legend("topleft", legend=c("Original", "Suavizado"), col=c("red", "blue"), lty=1)


#correcion de matm_other
#N<-200
variable<-ifelse(datasetA$catm_trx_other==0, 0, datasetA$matm_other/datasetA$catm_trx_other)
#bin_eq_freq <- discretize(variable,"equalfreq",N)
# Nos copiamos el atributo original
#bin_eq_freq$Y = variable

# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
#for(bin in 1:N){
#  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = median(bin_eq_freq$Y[bin_eq_freq$X==bin])
#}
#datasetA$matm_other_corregida<-(bin_eq_freq$suavizado*datasetB$catm_trx_other)
datasetA$matm_other_extraccion<-variable
#rm(bin_eq_freq)

variable<-ifelse(datasetB$catm_trx_other==0, 0, datasetB$matm_other/datasetB$catm_trx_other)
#bin_eq_freq <- discretize(variable,"equalfreq",N)
#bin_eq_freq$Y = variable

# Por cada bin calculamos la media y reemplazamos en el atributo suavizado
#for(bin in 1:N){
#  bin_eq_freq$suavizado[bin_eq_freq$X==bin] = median(bin_eq_freq$Y[bin_eq_freq$X==bin])
#}
#datasetB$matm_other_corregida<-(bin_eq_freq$suavizado*datasetB$catm_trx_other)
#rm(bin_eq_freq)
datasetB$matm_other_extraccion<-variable
rm(variable)
campos_buenos<-c("matm_other_extraccion")
#campos_buenos<-c("matm_other")  # revisar

variable<-ifelse(datasetA$catm_trx==0, 0, datasetA$matm/datasetA$catm_trx)
datasetA$matm_extraccion<-variable
variable<-ifelse(datasetB$catm_trx==0, 0, datasetB$matm/datasetB$catm_trx)
datasetB$matm_extraccion<-variable
rm(variable)
campos_buenos<-c("matm_extraccion")


#campos_buenos<-c("tpaquete1")  # en train valen todos 1 asi que seguro no hace ninguna regla en test aparecen 2, posiblmente promo acumulable
#campos_buenos<-c("mtarjeta_visa_descuentos")  # en train valen todos 1 asi que seguro no hace ninguna regla en test aparecen 2, posiblmente promo acumulable





library("lubridate")

correccion_habiles <- function(fin_de_mes,x)
{ 
  if (!is.na(x)){
  dias_mora<-as.numeric(x)
    if (dias_mora>0){
      rango<-fin_de_mes-days(seq(0,dias_mora-1))
      feriados<-date(c("2020-07-09","2020-07-10","2020-08-17","2020-10-12","2020-11-23"))#faltan cargar todos los feriados del año
      rangof<-rango[!rango %in% feriados]                                   #se sacan los dias que son feriados de la secuencia
      rm(rango)
      return (sum(!weekdays(rangof) %in% c("Saturday", "Sunday")))
    }
      else{ return (NA)    } 
    }
  else {
    return (x)
  }
}



fin_foto_mes<-date(ym(datasetA$foto_mes[1]))+months(1) - days(1)
datasetA$Master_Finiciomora_h<-ifelse(!is.na(datasetA$Master_Finiciomora),datasetA$Master_Finiciomora+5,NA)
zz<-ifelse(!is.na(datasetA$Master_Finiciomora),datasetA$Master_Finiciomora+5,NA)
niveles<-levels(factor(zz[[1]]))
#summary(factor(zz[[1]]))
niveles_corregido<-rep(0,length(niveles))
for (i in 1:length(niveles)){
  niveles_corregido[i]<-correccion_habiles(fin_foto_mes,as.numeric(niveles[i]))
  datasetA$Master_Finiciomora_h[datasetA$Master_Finiciomora_h == as.numeric(niveles[i])] <- as.numeric(niveles_corregido[i])
}

zr<-datasetA[,"Master_fultimo_cierre"]
nivelesz<-levels(factor(zr[[1]]))
nivelesr<-nivelesz[(as.numeric(nivelesz)<120) &(as.numeric(nivelesz)>0)]
nivelesr_corregido<-rep(0,length(nivelesr))
#summary(factor(zr[[1]]))
datasetA$Master_fultimo_cierre_h<-datasetA$Master_fultimo_cierre
for (i in 1:length(nivelesr)){
  nivelesr_corregido[i]<-correccion_habiles(fin_foto_mes,as.numeric(nivelesr[i]))
  datasetA$Master_fultimo_cierre_h[datasetA$Master_fultimo_cierre_h == as.numeric(nivelesr[i])] <- as.numeric(nivelesr_corregido[i])
}


rm(niveles)
rm(niveles_corregido)


#datasetB
fin_foto_mes<-date(ym(datasetB$foto_mes[1]))+months(1) - days(1)
datasetB$Master_Finiciomora_h<-datasetB$Master_Finiciomora
zz<-datasetB[,"Master_Finiciomora"]
niveles<-levels(factor(zz[[1]]))
#summary(factor(zz[[1]]))
niveles_corregido<-rep(0,length(niveles))
for (i in 1:length(niveles)){
  niveles_corregido[i]<-correccion_habiles(fin_foto_mes,as.numeric(niveles[i]))
  datasetB$Master_Finiciomora_h[datasetB$Master_Finiciomora_h == as.numeric(niveles[i])] <- as.numeric(niveles_corregido[i])
}
rm(nivelesr)
rm(nivelesr_corregido)

zr<-datasetB[,"Master_fultimo_cierre"]
nivelesz<-levels(factor(zr[[1]]))
#summary(factor(zr[[1]]))
nivelesr<-nivelesz[(as.numeric(nivelesz)<120) &(as.numeric(nivelesz)>0)]
nivelesr_corregido<-rep(0,length(nivelesr))

datasetB$Master_fultimo_cierre_h<-datasetB$Master_fultimo_cierre
for (i in 1:length(nivelesr)){
  nivelesr_corregido[i]<-correccion_habiles(fin_foto_mes,as.numeric(nivelesr[i]))
  datasetB$Master_fultimo_cierre_h[datasetB$Master_fultimo_cierre_h == as.numeric(nivelesr[i])] <- as.numeric(nivelesr_corregido[i])
}

#datasetA$Master_Finiciomora_ok<-ifelse(!is.na(datasetA$Master_Finiciomora),datasetA$Master_Finiciomora+5,NA)

#datasetB$Master_Finiciomora_ok<-ifelse((!is.na(datasetB$Master_Finiciomora)&&(datasetB$Master_Finiciomora<5)),0,datasetB$Master_Finiciomora)

#datasetB$Master_Finiciomora_ok<-datasetB$Master_Finiciomora

datasetA[ , mv_Finiciomora  := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]
datasetB[ , mv_Finiciomora  := pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ]

campos_buenos<-c("Master_Finiciomora_h")  # revisar
campos_buenos<-c("mv_Finiciomora")  # revisar

campos_buenos<-c("Master_Finiciomora")  # revisar
#campos_buenos<-c("mactivos_margen")

datasetA$Master_fultimo_cierre<-datasetA$Master_fultimo_cierre+5

campos<-c("numero_de_cliente","Master_Finiciomora","Master_fultimo_cierre")
df1<-datasetA[,campos,with=FALSE]
df2<-datasetB[,campos,with=FALSE]
tabla<-merge(df1, df2, by='numero_de_cliente', all.x = T)
tabla<-merge(df1, df2, by='numero_de_cliente', all.y = T)

zz<-(data.table(tabla,datasetA$clase_ternaria))
xx<-zz[zz[,V2]=='BAJA+1']
xx[is.na(xx[,Master_Finiciomora.x])!=TRUE]

tabla[is.na(tabla)]<-0 # reemplazo con ceros si algún cliente no esta en 202011
nombre<-c(paste0("cuotas_",campos[2]))


#campos_buenos<-c("Master_fultimo_cierre")
#pdf("./work/data_delta_01.pdf")
for( campo in  campos_buenos )
{
  cat( campo, "  " )
  
  distA  <- quantile(  datasetA[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  distB  <- quantile(  datasetB[ , get(campo) ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  #print(summary(datasetA[ , get(campo) ]))
  #print(summary(datasetB[ , get(campo) ]))
  
  a1  <- pmin( distA[[1]], distB[[1]] )
  a2  <- pmax( distA[[2]], distB[[2]] )
  
  densidadA  <- density( datasetA[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
  densidadB  <- density( datasetB[ , get(campo) ] , kernel="gaussian", na.rm=TRUE)
  
  plot(densidadA, 
       col="blue",
       xlim= c( a1, a2),
       main= paste0("Densidades    ",  campo), )
  
  lines(densidadB, col="red", lty=2)
  
  legend(  "topright",  
           legend=c("A", "B"),
           col=c("blue", "red"), lty=c(1,2))
  
  
  tbl  <- datasetA[ , c("numero_de_cliente", campo),   with=FALSE ]
  tbl[  datasetB, on="numero_de_cliente",  futuro := get(paste0("i.",campo)) ]
  tbl[ , delta :=  futuro - get(campo)  ]
  
 # plot(sort(tbl[,delta]))
  drift  <- density( tbl[ !is.na(delta), delta ] , kernel="gaussian", na.rm=TRUE)
  
  qdrift  <- quantile(  tbl[ , delta ] , prob= c(0.05, 0.95), na.rm=TRUE )
  
  a1  <- qdrift[[1]]
  a2  <- qdrift[[2]]
  
  
  plot(drift, 
       col="blue",
       main= paste0("DELTA    ",  campo),
       xlim= c(a1, a2),
       lwd= 3
  )
  
  abline( v=0, col="darkgreen" )
  
  legend(  "topright",  
           legend=c("A", "B"),
           col=c("blue", "darkgreen"), lty=c(1,2))
  
 
  
}

# suma que da super correlacionada
#plot(datasetA$mrentabilidad,datasetA$mactivos_margen+datasetA$mpasivos_margen,col=factor(datasetA$clase_ternaria))

#plot(datasetA$chomebanking_transacciones,datasetA$cmobile_app_trx)
#z<-data.frame(datasetA$catm_trx,datasetA$matm,datasetA$catm_trx_other,datasetA$matm_other)

#hist(ifelse(datasetB$catm_trx_other==0, 0, datasetB$matm_other/datasetB$catm_trx_other),breaks = 50)

#z<-data.frame(datasetB$catm_trx,datasetB$matm,ifelse(datasetB$catm_trx==0, 0, datasetB$matm/datasetB$catm_trx),datasetB$catm_trx_other,datasetB$matm_other,ifelse(datasetB$catm_trx_other==0, 0, datasetB$matm_other/datasetB$catm_trx_other))
#colnames(z)<-c("catm_trx","matm","divm","catm_trx_o","matm_o","div_o")

#z1<-data.frame(datasetA$catm_trx,datasetA$matm,ifelse(datasetA$catm_trx==0, 0, datasetA$matm/datasetA$catm_trx),datasetA$catm_trx_other,datasetA$matm_other,ifelse(datasetA$catm_trx_other==0, 0, datasetA$matm_other/datasetA$catm_trx_other))
#colnames(z1)<-c("catm_trx","matm","divm","catm_trx_o","matm_o","div_o")

#z1<-data.frame(datasetB$Master_Finiciomora,datasetB$Master_fultimo_cierre,datasetB$Master_status,datasetB$Master_delinquency,datasetB$Master_mconsumospesos,datasetB$mttarjeta_master_debitos_automaticos)
#z<-data.frame(datasetA$Master_Finiciomora,datasetA$Master_fultimo_cierre,datasetA$Master_status,datasetA$Master_delinquency,datasetA$Master_mconsumospesos,datasetA$ctarjeta_master_debitos_automaticos)

library(dplyr)
df1<-data.table(datasetA$numero_de_cliente,rank(datasetA$mactivos_margen,ties.method="min"))

df1<-data.table(datasetA$numero_de_cliente,datasetA$mactivos_margen)
colnames(df1)<-c("numero_de_cliente","mactivos_margenA")
df1[, tipoA := rep("A",length(datasetA$numero_de_cliente))]

df2<-data.table(datasetB$numero_de_cliente,rank(datasetB$mactivos_margen,ties.method="min"))
df2<-data.table(datasetB$numero_de_cliente,datasetB$mactivos_margen)
colnames(df2)<-c("numero_de_cliente","mactivos_margenB")
df2[, tipoB := rep("B",length(datasetB$numero_de_cliente))]

library(plyr)
tablita<-join(df1, df2, type = "inner", by="numero_de_cliente")


colA = c("mactivos_margenA", "mactivos_margenB")
colB = c("tipoA","tipoB")

campo=c(colA,colB)
sub_tablita<-tablita[,campo, with=FALSE] 
DT.m1 = melt(sub_tablita, measure = list(colA, colB))
colnames(DT.m1)<-c('variable','mactivos_margen','tipo')

DT.m1$tipo<-factor(DT.m1$tipo)
aov.df<-aov(mactivos_margen~tipo,data=DT.m1[,c('mactivos_margen','tipo')])
summary(aov.df)
shapiro.test(residuals(aov.df))
library(nortest)
ad.test(residuals(aov.df))
#el test F rechaza la igualdad de medias a nivel 0:05.
library(car)
leveneTest(mactivos_margen~tipo ,data=DT.m1[,c('mactivos_margen','tipo')])
# no se cumple la homocedasticidad

kruskal.test(mactivos_margen~tipo,data=DT.m1[,c('mactivos_margen','tipo')])

TukeyHSD(aov.df,conf.level=0.95)
# Real iza las comparaciones mú l t i p l e s a p o s t e r i

library("ggplot2")
ggplot(DT.m1[,c('mactivos_margen','tipo')],aes(sample=residuals(aov.df)))+
  stat_qq(alpha=0.5,color="royalblue")+
  xlab("Valoresteóricos")+
  ylab("Valoresdelamuestra")

ggplot(DT.m1[,c('mactivos_margen','tipo')], aes_string(x="mactivos_margen"))+
  geom_histogram(aes(color =factor(tipo),fill=factor(tipo)),position = "identity", bins = 50, alpha = 0.6) +
  scale_fill_brewer(palette="Dark2")#+
  #scale_color_brewer(palette = "Dark2")

ggplot(DT.m1[,c('mactivos_margen','tipo')],aes_string(x="tipo",y="mactivos_margen",fill=as.factor(DT.m1$tipo)))+
  geom_boxplot()+
  xlab("")+
  scale_fill_brewer(palette="Dark2",name="mactivos_margen")+
  theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+
  theme_bw()

    

data<-tablita[,c(2,3)]
vector_medias = colMeans(data) 
vector_medianas = apply(data, 2, median)
matriz_var_cov = cov(data)

maha1 = mahalanobis(data,vector_medias,matriz_var_cov)
top_maha1 <- head(data[order(maha1,decreasing = TRUE),],10)

plot(tablita$mactivos_margen.A,tablita$mactivos_margen.B)



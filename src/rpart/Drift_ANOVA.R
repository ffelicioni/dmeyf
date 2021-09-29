#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
library(car)
library(nortest)

#cargo los datasets que voy a comparar
setwd("C:/Users/Flavia/Documents/DataScience/dmeyf") 


#dataset1  <- fread( "./datasetsOri/paquete_premium_202009.csv" )
#dataset2  <- fread( "./datasetsOri/paquete_premium_202011.csv" )

dataset1  <- fread( "./datasets/paquete_premium_202009_ext.csv" )
dataset2  <- fread( "./datasets/paquete_premium_202011_ext.csv" )

resumen_aov<-data.table(variable=character(),media_A=numeric(), media_B=numeric(), var_A=numeric(), var_B=numeric(),pv_aov=numeric(),pv_ad_residuo=numeric(),pv_lev_residuo=numeric(),pv_kw=numeric())

#campos_buenos<-c("Master_Finiciomora")

campos_buenos<-c("mpayroll","mpasivos_margen","mcuentas_saldo","Visa_msaldototal","mactivos_margen","ctarjeta_debito_transacciones","mcuenta_corriente","Visa_msaldopesos","mrentabilidad_annual","ctarjeta_visa_transacciones","ctrx_quarter","Visa_fechaalta","mcaja_ahorro","Visa_mpagominimo","cproductos","Visa_status","mtarjeta_visa_consumo","cpayroll_trx","mprestamos_personales","cliente_edad","mforex_sell")


for( jj in  seq(1:length(campos_buenos)))
{
  campo<-campos_buenos[jj]
  
  df1<-dataset1[,c("numero_de_cliente",campo),with=FALSE]
  df2<-dataset2[,c("numero_de_cliente",campo),with=FALSE] 
  tabla<-merge(df1, df2, by='numero_de_cliente',all = T)
  colnames(tabla)<-c("numero_de_cliente","A","B")
  
  colA = c(colnames(tabla)[2], colnames(tabla)[3])
  Dm = melt(tabla, measure.vars = list(colA))
  colnames(Dm)<-c("numero_de_cliente",'tipo','valor')
  Dm[,tipo:=factor(Dm[,tipo])]
  
  aov.df<-aov(valor~tipo,data=na.omit(Dm[,c('valor','tipo')],cols = "valor"))
  #summary(aov.df)
  p_value_aov<-summary(aov.df)[[1]][["Pr(>F)"]][1]
  #shapiro.test(residuals(aov.df)) solo se puede aplicar si la muestra es menor a 5000
  #ad.test(residuals(aov.df))
  zz<-ad.test(residuals(aov.df))
  p_value_ad<-zz$p.value
  zz<-leveneTest(valor~tipo,data=na.omit(Dm[,c('valor','tipo')],cols = "valor"))
  p_value_lev<-zz$`Pr(>F)`[1]
  
  zz<-kruskal.test(valor~tipo,data=na.omit(Dm[,c('valor','tipo')],cols = "valor"))
  p_value_kw<-zz$p.value
  #TukeyHSD(aov.df,conf.level=0.95)
  
  medias<-Dm[,.(tipo.mean=mean(valor,na.rm=TRUE)),by=tipo]
  var<-Dm[,.(tipo.var=var(valor,na.rm=TRUE)),by=tipo]
  row<-list(campo,round(medias$tipo.mean[1],2),round(medias$tipo.mean[2],2),var$tipo.var[1],var$tipo.var[2],round(p_value_aov,2),round(p_value_ad,2),round(p_value_lev,2),round(p_value_kw,2))
  resumen_aov<-rbindlist(list(resumen_aov, row))
  #tabla[A==1,.N]
  #tabla[B==1,.N]
  #dataset1[ cforex == 1 & clase_ternaria == "BAJA+2",sum(cforex==1)] 
}

fwrite(resumen_aov, "./work/resumen_aov.csv")
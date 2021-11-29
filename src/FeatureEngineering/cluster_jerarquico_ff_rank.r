require("data.table")
require("randomForest")


#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

setwd( "~/buckets/b1/" )

#leo el dataset , aqui se puede usar algun super dataset con Feature Engineering
dataset  <- fread( "datasets/paquete_premium_1_meses_rank.csv.gz", stringsAsFactors= TRUE)
#gc()


require("data.table")
require("lightgbm")


#setwd("C:/Users/Flavia/Documents/DataScience/dmeyf/")

#cargo el dataset donde voy a entrenar
#dataset  <- fread("./datasets/paquete_premium_1_meses_rank.csv.gz",stringsAsFactors= TRUE)

dataset  <- fread("./datasets/paquete_premium_1_meses_rank.csv.gz",stringsAsFactors= TRUE)
dataset<-dataset[foto_mes>=202001  & foto_mes<=202011,] 
gc()


#quito los nulos para que se pueda ejecutar randomForest,  Dios que algoritmo prehistorico ...
dataset  <- na.roughfix( dataset )
gc()


campos_buenos  <- c( "ctrx_quarter", "cpayroll_trx", "mcaja_ahorro_rankM", "mtarjeta_visa_consumo_rankM", "ctarjeta_visa_transacciones",
                     "mcuentas_saldo_rankM", "mrentabilidad_annual_rankM", "mprestamos_personales_rankM", "mactivos_margen_rankM", "mpayroll_rankM",
                     "Visa_mpagominimo", "cliente_edad", "chomebanking_transacciones", "Visa_msaldopesos_rankM",
                     "Visa_Fvencimiento", "mrentabilidad_rankM", "Visa_msaldototal_rankM", "Master_Fvencimiento", "mcuenta_corriente_rankM",
                     "Visa_mpagospesos_rankM", "Visa_fechaalta", "mcomisiones_mantenimiento_rankM", "Visa_mfinanciacion_limite_rankM",
                     "mtransferencias_recibidas_rankM", "cliente_antiguedad", "Visa_mconsumospesos_rankM", "Master_mfinanciacion_limite_rankM",
                     "mcaja_ahorro_dolares_rankM", "cproductos", "mcomisiones_otras_rankM", "thomebanking", "mcuenta_debitos_automaticos_rankM",
                     "mcomisiones_rankM", "Visa_cconsumos", "ccomisiones_otras", "Master_status", "mtransferencias_emitidas_rankM",
                     "mpagomiscuentas_rankM","Visa_mlimitecompra","Master_mlimitecompra")


#dataset1  <- na.roughfix( dataset1 )

#Ahora, a esperar mucho con este algoritmo del pasado que NO correr en paralelo, patetico
modelo  <- randomForest( x= dataset[ , campos_buenos, with=FALSE ], 
                         y= NULL, 
                         ntree= 1000, #se puede aumentar a 10000
                         proximity= TRUE, 
                         oob.prox = TRUE )

#genero los clusters jerarquicos
hclust.rf  <- hclust( as.dist ( 1.0 - modelo$proximity),  #distancia = 1.0 - proximidad
                      method= "ward.D2" )


pdf( paste0( paste0("./work/cluster_jerarquico.pdf" ) ))
plot( hclust.rf )
dev.off()


h <- 20
distintos <- 0

while(  h>0  &  !( distintos >=6 & distintos <=7 ) )
{
  h <- h - 1 
  rf.cluster  <- cutree( hclust.rf, h)
  
  dataset[  , cluster2 := NULL ]
  dataset[  , cluster2 := rf.cluster ]
  
  distintos  <- nrow( dataset[  , .N,  cluster2 ] )
  cat( distintos, " " )
}

#en  dataset,  la columna  cluster2  tiene el numero de cluster
#sacar estadicas por cluster

dataset[  , .N,  cluster2 ]  #tamaño de los clusters

#ahora a mano veo las variables
dataset[  , mean(ctrx_quarter),  cluster2 ]  #media de la variable  ctrx_quarter

dataset[  , mean(cproductos),  cluster2 ]  #media de la variable  ctrx_quarter

dataset[  , median(cpayroll_trx),  cluster2 ]  #media de la variable  ctrx_quarter


dataset[  , mean(Master_mlimitecompra_rankM),  cluster2 ]  #media de la variable  ctrx_quarter
dataset[  , mean(Master_mlimitecompra),  cluster2 ]  #media de la variable  ctrx_quarter

#datos_variables%>%scale%>%dist()%>%hclust(method="ward.D2")%>%as.dendrogram()->dend
#Aplica el criterio de Ward a las variables estandarizadas
#par(mar=c(1,1,0.1,4))#Establece márgenes
#dend%>%
#  set("branches_k_color",value=c("blue","red","purple","darkgreen","cyan","gray","black"),k=7)%>%
#Personaliza las ramas
#  set("labels_col",value=c("blue","red","purple","darkgreen"),k=4)%>%
#  set("labels_cex",0.95)%>%#set("labels",datos$position)%>%
#  plot(axes=FALSE,horiz=TRUE)+
#  theme_bw()

require("ggpubr")
install.packages("factoextra")
library()

require(RColorBrewer)

colores_cl<-brewer.pal(n = 7, name = 'Dark2')

fviz_cluster(cluster2, data = dataset[ , campos_buenos, with=FALSE ],
             palette = colores_cl, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)

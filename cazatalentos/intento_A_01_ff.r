#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

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
  GLOBAL_jugadores  <<-  ( c( (501:599 ) / 1000 , 0.7 )) # sample
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
#------------------------------------------------------------------------------

particionar  <- function( data, division, agrupa="", campo="fold", start=1)
{
  bloque  <- unlist( mapply(  function(x,y) { rep( y, x )} ,   division,  seq( from=start, length.out=length(division) )  ) )  
  
  data[ , (campo) :=  rep( bloque, ceiling(.N/length(bloque)))[1:.N], by= agrupa ]
}



#Estrategia
#En la primer ronda se hace tirar 90 tiros libres a cada uno de los 100 jugadores ( se gastan 90000 tiros )
#Se eligen a la mejor mitad  ( se descarta a la peor mitad )
#En la segunda ronda, a la mejor mitad de la anterior ronda se los hace tirar 400 tiros a cada uno
#Se elige el mejor jugador de la segunda ronda

set.seed( 100003 )
set.seed( 129136 )

#inicializo el juego
gimnasio_init()

#Esta el la planilla del cazatalentos
planilla_cazatalentos  <- data.table(  "id"=1:100 )


#Ronda 1  ------------------------------------------------------
#tiran los 100 jugadores es decir 1:100  90 tiros libres cada uno

N1<-90
#lo saco haciendo 0.7*N1-1.282*sqrt(0.7*N1) para que sea mayor a 0.5 y quede por encima de la mediana
N<-N1*100


ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,

ctiros<-c(90,150-90,210-150,260-210,320-260,370-320,415-370)

ctiros<-c(90,150-90,217-150,273-217,325-273,412-325) # 375-320,

planilla_cazatalentos[ ids_juegan1,  tiros1 := ctiros[1]]  #registro en la planilla que tiran 90 tiros
#Hago que tiren
resultado1  <- gimnasio_tirar( ids_juegan1, ctiros[1])
planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla
planilla_cazatalentos[ ids_juegan1,acumulados := aciertos1 ]  #registro en la planilla
planilla_cazatalentos[ ids_juegan1,  c(paste0('p',1)) := resultado1/N1 ]  #registro en la planilla
planilla_cazatalentos[ids_juegan1, tiros_acum:=N1]
planilla_cazatalentos[ids_juegan1, media_foto1:=mean(p1)]
planilla_cazatalentos[ids_juegan1, c(paste0('s',1)):=sqrt(acumulados)/tiros_acum] 

qfolds=5
divi  <- rep( 1, qfolds )

particionar(planilla_cazatalentos,divi) 

#ids_jueganx<-list()
#for (i in 1:qfolds ) {
#  ids_jueganf<-planilla_cazatalentos[ fold==i, id]
#  mediana  <- planilla_cazatalentos[ ids_jueganf, median(acumulados) ]
#  ids  <- planilla_cazatalentos[ ids_jueganf ][ acumulados >= mediana, id ]
#  ids_jueganx<-c(unlist(ids_jueganx),ids)
#}
#ids_juegan2<-sort(unlist(ids_jueganx))

#print(planilla_cazatalentos[fold,])
#mediana  <- planilla_cazatalentos[ ids_juegan1, median(acumulados) ]
#ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ acumulados >= mediana, id ]

#Rondas -------------------------------------------------------

for (i in 2:2){
  
  torneo_anterior<-c(paste0('aciertos',i))
#los mejores 40 jugadores tiran 400 tiros cada uno
  mediana  <- planilla_cazatalentos[ ids_juegan1, median(acumulados) ]
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][acumulados >= mediana, id ]

  print(length(ids_juegan2))
  #ids_juegan2<-1:100 
  N2<-ctiros[i]


  planilla_cazatalentos[ ids_juegan2,  c(paste0('tiros',i)) := N2 ]  #registro en la planilla que tiran 400 tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, N2)
  #torneo_actual<-c(paste0('aciertos',i+1))
  planilla_cazatalentos[ ids_juegan2,  c(paste0('aciertos',i)) := resultado2 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan2,  acumulados := acumulados+resultado2 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan2,  tiros_acum := tiros_acum+N2 ]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan2,  c(paste0('p',i)) := acumulados/tiros_acum ]  #registro en la planilla
  planilla_cazatalentos[ids_juegan2, c(paste0('s',i)):=sqrt(acumulados)/tiros_acum]
  planilla_cazatalentos[ids_juegan2, c(paste0('media_foto',i)):=mean(resultado2/N2)]
  
#planilla_cazatalentos[,p2:=aciertos2/tiros2]
#planilla_cazatalentos[,s2:=sqrt(aciertos2/tiros2)/tiros2]

#planilla_cazatalentos[,p3:=(aciertos2+aciertos1)/(tiros2+tiros1)]

  
  N1<-N2
  ids_juegan1<-ids_juegan2
  print(ids_juegan1)
  N<-N+length(ids_juegan1)*N2
  
  
}

planilla_cazatalentos[ids_juegan2, inferior:=p4-1.2901*s4]
planilla_cazatalentos[ids_juegan2, superior:=p4+1.2901*s4]
df1<-planilla_cazatalentos[ids_juegan2,c("id","p1","p2","p3","p4"),with=FALSE]#,"s1","s2","s3","s4","s5"),with=FALSE]

df1<-planilla_cazatalentos[ids_juegan2,c("id","inferior","superior"),with=FALSE]#,"s1","s2","s3","s4","s5"),with=FALSE]

#df1<-planilla_cazatalentos[ids_juegan2,c("id","p1","p2","p3","p4"),with=FALSE]#,"s1","s2","s3","s4","s5"),with=FALSE]
#df2<-planilla_cazatalentos[,c("numero_de_cliente",campo),with=FALSE] 
#tabla<-merge(df1, df2, by='numero_de_cliente',all = T)
n<-dim(df1)



Dm = melt(df1,id.vars="id")
#Dm[,iteracion:=rep(seq(1:n[1]),n[2]-1)]
Dm<-Dm[, !"variable"]
Dm[,id:=as.factor(id)]

#summary(modelo)$fstatistic

model<-aov(value~ id, data=Dm)
zz<-pairwise.t.test(Dm$value, Dm$id, p.adjust.method="holm")
id_test<-as.numeric(rownames(zz$p.value))
dg<-data.table(zz$p.value)
minimo<-apply(dg, 1, FUN = min, na.rm = TRUE)


#-----------------------------------------------
Dm.aov <- aov(value ~ id, data = Dm)
summary(Dm.aov)

Dm.mc <-TukeyHSD(Dm.aov, which = "id",conf.level = 0.9)
Dm.mc <-TukeyHSD(Dm.aov, which = "id",linfct = mcp(id = "holm"),conf.level = 0.9)
#TukeyHSD(anthox1, "Harvest", conf.level = 0.95)
#Dm.mc <- glht(Dm.mc,linfct = mcp(id = "Dunnett"), alternative = "less")
Dm.mc$id
summary(Dm, test = adjusted(type = "Dunnet")) #es menos exigente que dunnet
summary(Dm, test = adjusted(type = "bonferroni")) #es menos exigente que dunnet

Dm.mc<-glht(Dm.aov, linfct = mcp(id = "Tukey"),alternative="greater")
Dm.mc<-glht(Dm.aov, linfct = mcp(id = "holm"),alternative="greater")
summary(Dm.mc)
recovery.ci <- confint(Dm.aov, level = 0.90)
recovery.ci
#plot(recovery.ci, main = "", ylim = c(0.5, 3.5),xlab = "id")



summary(Dm.aov)
#recovery.mc <- glht(recovery.aov,linfct = mcp(id = "Dunnett"), alternative = "greater")
#recovery.mc <- glht(recovery.aov,linfct = mcp(id = "Dunnett"), alternative = "less")
#summary(recovery.mc)

#summary(recovery.mc, test = adjusted(type = "bonferroni"))

# 
df1[,id:=factor(id)]

immer.aov <- aov((p1 + p2 + p3 + p4 + p5)/5 ~ id, data = df1)
model.tables(immer.aov, type = "means")$tables
#immer.mc <- glht(immer.aov, linfct = mcp(id = "Tukey"),alternative = "greater")
#immer.mc <- glht(immer.aov, linfct = mcp(id = "Dunnet"),alternative = "greater")
#summary(immer.mc)


immer.ci <- confint(immer.aov)
immer.ci

#summary(immer.mc, test = adjusted(type = "free"))

summary(immer.ci, test = adjusted(type = "holm"))

#print(id_test[which.min(minimo)])

library("multcomp")
#LSD.test(Dm.aov,"id", p.adj="bonferroni", alpha = 0.05, console=TRUE)
glht(Dm.aov, linfct = mcp(id = "Tukey"),alternative = c( "greater")) #rhs = 0


#El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
pos_mejor <-  planilla_cazatalentos[ , which.max(acumulados) ]
id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

print(id_mejor)
#Finalmente, la hora de la verdadero_mejor
#Termino el juego
veredicto  <- gimnasio_veredicto( id_mejor )

veredicto


#El veredicto da que la estrategia seguida por el cazatalentos fue exitosa para este caso
#Le acerto al verdadero_mejor

#En el siguiente script veremos de hacer una Estimacion Montecarlo
#De 10000 veces que el entrenador sigue esta estrategia, cuantas realmente le acierta


#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#ideas principales, en cada ronda se acumulan los aciertos de los que siguen en carrera
#la probabilida del primer tiro tiene que superar al 50% para que super a la mediana,
#el segundo tiro deberia superar a 75% ya que es la mediana de la mediana (si se dio el evento ell segundo tiro p>1-(1-p(1))*.5)
#sigue así hasta que hayan tirado los ultimos los 415 tiros y con suerte el hganador llego hasta ahí


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
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
  position_mejor <-  which(GLOBAL_jugadores %in% c(0.7))
  #print('campeon')
  #print(position_mejor)
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


Estrategia_B  <- function()
{
  #Estrategia
  #Se juegan varias rondas
  #En cada ronda, los jugadores que participan, tiran x tiros
  #De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  #Se elige el mejor jugador de la septima ronda

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )

  ctiros<-c(85,61,61,61,61,61,30) 
  #ctiros<-c(85)
  
  ctiros<-c(75,61,61,61,61,61,30) 
  
  ctiros<-c(85,66,100,61,61,61,30) 
  ctiros<-c(90,66,120,61,61,61,30) 
  ctiros<-c(90,66,260,61,61,61,30) 
  
  ctiros<-c(85,66,350,61,61,61,30) 
  #ctiros<-c(50,50,50,50,50,50,50,70) 
  #ctiros<-c(115,61,61,61,61,61,30) 
  
  ctiros<-c(60,60,60,60,61,61,30) 
  ctiros<-c(90,60,60,60,60,60,30)  #156660 .952 
  
  ctiros<-c(90,50,60,60,60,60,30)  #156660 .952 
  
  ctiros<-c(85,62,55,60,60,60,30)  # 85,62,55,60,60,60,60
  
  ids_juegan_ini  <- 1:100   #los jugadores que participan en la ronda,
  ids_juegan_ini1  <- 1:5   #los jugadores que participan en la ronda,
  ids_juegan_ini2  <- 6:10   #los jugadores que participan en la ronda,
  ids_juegan_ini3  <- 11:15   #los jugadores que participan en la ronda,
  ids_juegan_ini4  <- 16:20   #los jugadores que participan en la ronda,
  ids_juegan_ini5  <- 21:25   #los jugadores que participan en la ronda,
  ids_juegan_ini6  <- 26:30   #los jugadores que participan en la ronda,
  ids_juegan_ini7  <- 31:35   #los jugadores que participan en la ronda,
  ids_juegan_ini8  <- 36:40   #los jugadores que participan en la ronda,
  ids_juegan_ini9  <- 41:45   #los jugadores que participan en la ronda,
  ids_juegan_ini10 <- 46:50   #los jugadores que participan en la ronda,
  ids_juegan_ini11  <- 51:55   #los jugadores que participan en la ronda,
  ids_juegan_ini12  <- 56:60   #los jugadores que participan en la ronda,
  ids_juegan_ini13  <- 61:65   #los jugadores que participan en la ronda,
  ids_juegan_ini14  <- 66:70   #los jugadores que participan en la ronda,
  ids_juegan_ini15  <- 71:75   #los jugadores que participan en la ronda,
  ids_juegan_ini16  <- 76:80   #los jugadores que participan en la ronda,
  ids_juegan_ini17  <- 81:85   #los jugadores que participan en la ronda,
  ids_juegan_ini18  <- 86:90   #los jugadores que participan en la ronda,
  ids_juegan_ini19  <- 91:95   #los jugadores que participan en la ronda,
  ids_juegan_ini20  <- 96:100   #los jugadores que participan en la ronda,

  #ids_juegan_ini1  <- 1:10   #los jugadores que participan en la ronda,
  #ids_juegan_ini2  <- 11:20   #los jugadores que participan en la ronda,
  #ids_juegan_ini3  <- 21:30   #los jugadores que participan en la ronda,
  #ids_juegan_ini4  <- 31:40   #los jugadores que participan en la ronda,
  #ids_juegan_ini5  <- 41:50   #los jugadores que participan en la ronda,
  #ids_juegan_ini6  <- 51:60   #los jugadores que participan en la ronda,
  #ids_juegan_ini7  <- 61:70   #los jugadores que participan en la ronda,
  #ids_juegan_ini8  <- 71:80   #los jugadores que participan en la ronda,
  #ids_juegan_ini9  <- 81:90   #los jugadores que participan en la ronda,
  #ids_juegan_ini10 <- 91:100   #los jugadores que participan en la ronda,
  
  
  planilla_cazatalentos[ ids_juegan_ini1,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini1,  acumulados1 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini2,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini2,  acumulados2 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini3,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini3,  acumulados3 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini4,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini4,  acumulados4 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini5,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini5,  acumulados5 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini6,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini6,  acumulados6 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini7,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini7,  acumulados7 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini8,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini8,  acumulados8 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini9,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini9,  acumulados9 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini10,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini10,  acumulados10 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini11,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini11,  acumulados11 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini12,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini12,  acumulados12 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini13,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini13,  acumulados13 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini14,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini14,  acumulados14 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini15,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini15,  acumulados15 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini16,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini16,  acumulados16 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini17,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini17,  acumulados17 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini18,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini18,  acumulados18 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini19,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini19,  acumulados19 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini20,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini20,  acumulados20 := 0]  #registro en la planilla
  
  planilla_cazatalentos[ ids_juegan_ini,  acumulados := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini,  prob := 1]
  
  usa_grupo<-rep(T,20)
  ronda_final<-F
  
  #t=c(0.718,0.677,0.815,1.026,1.1374,1.2384,1.282)
  #t<-c(.0178,0.6745,0.6745,0.6745,0.6745,0.6745,1.282)
  for (i in 1:3){
    
    #print(paste0('ronda',i))
    maximos<-list()
    desvios<-list()
    #planilla_cazatalentos<-planilla_cazatalentos[,fold:=]
    #indices_part1<-ids_juegan_ini1[seq(1,floor(length(ids_juegan_ini)/2))]
    #indices_part2<-ids_juegan_ini2[seq(floor(length(ids_juegan_ini)/2)+1,length(ids_juegan_ini))]
    resultado  <- gimnasio_tirar( ids_juegan_ini, ctiros[i])
    planilla_cazatalentos[ ids_juegan_ini,  acumulados := acumulados+resultado]
    planilla_cazatalentos[ ids_juegan_ini,  tiros_acum := tiros_acum+ctiros[i]]  
    planilla_cazatalentos[ ids_juegan_ini,  c(paste0('aciertos',i))  := resultado ]  
    planilla_cazatalentos[ ids_juegan_ini, aciertos  := resultado ] 
    planilla_cazatalentos[ ids_juegan_ini,  prob := prob*resultado/ctiros[i]]
    
    
    if (usa_grupo[1]==T){
      resultado1<-planilla_cazatalentos[ ids_juegan_ini1, aciertos ] 
      planilla_cazatalentos[ ids_juegan_ini1,  acumulados1 := acumulados1+resultado1] 
      
      mediana1  <- planilla_cazatalentos[ ids_juegan_ini1, median(acumulados1) ]
      mediana1<-planilla_cazatalentos[ ids_juegan_ini1,  median(prob)]
      ids_juegan_fin1  <- planilla_cazatalentos[ ids_juegan_ini1][ acumulados1 >= mediana1, id ]
      ids_juegan_fin1  <- planilla_cazatalentos[ ids_juegan_ini1][ prob >= mediana1, id ]
      ids_juegan_ini1<-ids_juegan_fin1
      
      #data<-planilla_cazatalentos[ ids_juegan_ini1,  acumulados1]
      data<-planilla_cazatalentos[ ids_juegan_ini1,  prob]
      #print(data)
      #colnames(data)<-c('id','medianas')
      #data <- data[with(data,order(-medianas)),]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos1<-mean(data[1]) 
      } else
      {
        media_pos1<-data
      }
    }
    
    if (usa_grupo[2]==T){
      
      #
      resultado2<-planilla_cazatalentos[ ids_juegan_ini2, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini2,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini2,  c(paste0('aciertos',i))  := resultado2 ]  
      #planilla_cazatalentos[ ids_juegan_ini2,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini2,  acumulados2 := acumulados2+resultado2] 
      #planilla_cazatalentos[ ids_juegan_ini2,  acumulados := acumulados+resultado2] 
      #planilla_cazatalentos[ ids_juegan_ini2,  c(paste0('p',i)) := acumulados2/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini2,  c(paste0('s',i)):=sqrt(acumulados2)/tiros_acum]
      
      mediana2  <- planilla_cazatalentos[ ids_juegan_ini2, median(acumulados2) ]
      mediana2<-planilla_cazatalentos[ ids_juegan_ini2,  median(prob)]
      ids_juegan_fin2  <- planilla_cazatalentos[ ids_juegan_ini2][ acumulados2 >= mediana2, id ]
      ids_juegan_fin2  <- planilla_cazatalentos[ ids_juegan_ini2][ prob >= mediana2, id ]
      ids_juegan_ini2<-ids_juegan_fin2
      
      data<-planilla_cazatalentos[ ids_juegan_ini2,  acumulados2]
      data<-planilla_cazatalentos[ ids_juegan_ini2,  prob]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos2<-mean(data[1]) 
      } else
      {
        media_pos2<-data
      }
      #media_pos2<-planilla_cazatalentos[ ids_juegan_ini2,  mean(acumulados/ tiros_acum)]
      
      #indice_max2<-planilla_cazatalentos[ ,  which.max(acumulados2/tiros_acum)]
      #id_max2  <-  planilla_cazatalentos[ indice_max2, id ]
      #max_actual2<-planilla_cazatalentos[ id_max2, acumulados2/tiros_acum] 
      #print(id_max2)
      #print(max_actual2)
     # desvio_actual2<-planilla_cazatalentos[ id_max2, sqrt(acumulados2)/tiros_acum] 
      
      #maximos<-c(unlist(maximos),media_pos2)
      #media2<-c(unlist(media2),media_pos2)
      #desvios<-c(unlist(desvios),desvio_actual2)
    }
    
    if (usa_grupo[3]==T){
      resultado3<-planilla_cazatalentos[ ids_juegan_ini3, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini3,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini3,  c(paste0('aciertos',i))  := resultado3 ]  
      #planilla_cazatalentos[ ids_juegan_ini3,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini3,  acumulados3 := acumulados3+resultado3] 
      #planilla_cazatalentos[ ids_juegan_ini3,  acumulados := acumulados+resultado] 
      #planilla_cazatalentos[ ids_juegan_ini3,  c(paste0('p',i)) := acumulados3/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini3,  c(paste0('s',i)):=sqrt(acumulados3)/tiros_acum]
      
      mediana3  <- planilla_cazatalentos[ ids_juegan_ini3, median(acumulados3) ]
      mediana3<-planilla_cazatalentos[ ids_juegan_ini3,  median(prob)]
      ids_juegan_fin3  <- planilla_cazatalentos[ ids_juegan_ini3][ acumulados3 >= mediana3, id ]
      ids_juegan_fin3  <- planilla_cazatalentos[ ids_juegan_ini3][ prob >= mediana3, id ]
      
      ids_juegan_ini3<-ids_juegan_fin3
      #data<-planilla_cazatalentos[ ids_juegan_ini3,  acumulados3]
      data<-planilla_cazatalentos[ ids_juegan_ini3,  prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos3<-mean(data[1]) 
      } else
      {
        media_pos3<-data
      }
    }
    
    
    if (usa_grupo[4]==T){
      resultado4<-planilla_cazatalentos[ ids_juegan_ini4, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini4,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini4,  c(paste0('aciertos',i))  := resultado4 ]  
      #planilla_cazatalentos[ ids_juegan_ini4,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini4,  acumulados4 := acumulados4+resultado4] 
      #planilla_cazatalentos[ ids_juegan_ini4,  acumulados := acumulados+resultado] 
      #planilla_cazatalentos[ ids_juegan_ini4,  c(paste0('p',i)) := acumulados4/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini4,  c(paste0('s',i)):=sqrt(acumulados4)/tiros_acum]
      
      mediana4  <- planilla_cazatalentos[ ids_juegan_ini4, median(acumulados4) ]
      mediana4<-planilla_cazatalentos[ ids_juegan_ini4,  median(prob)]
      ids_juegan_fin4  <- planilla_cazatalentos[ ids_juegan_ini4][ acumulados4 >= mediana4, id ]
      ids_juegan_fin4  <- planilla_cazatalentos[ ids_juegan_ini4][ prob >= mediana4, id ]
      ids_juegan_ini4<-ids_juegan_fin4
      #data<-planilla_cazatalentos[ ids_juegan_ini4,  acumulados4]
      data<-planilla_cazatalentos[ ids_juegan_ini4, prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos4<-mean(data[1]) 
      } else
      {
        media_pos4<-data
      }
    }
    

    if (usa_grupo[5]==T){
      resultado5<-planilla_cazatalentos[ ids_juegan_ini5, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini5,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini5,  c(paste0('aciertos',i))  := resultado5 ]  
      #planilla_cazatalentos[ ids_juegan_ini5,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini5,  acumulados5 := acumulados5+resultado5] 
      #planilla_cazatalentos[ ids_juegan_ini5,  acumulados := acumulados+resultado] 
      #planilla_cazatalentos[ ids_juegan_ini5,  c(paste0('p',i)) := acumulados5/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini5,  c(paste0('s',i)):=sqrt(acumulados5)/tiros_acum]
      
      mediana5  <- planilla_cazatalentos[ ids_juegan_ini5, median(acumulados5) ]
      mediana5<-planilla_cazatalentos[ ids_juegan_ini5,  median(prob)]
      ids_juegan_fin5  <- planilla_cazatalentos[ ids_juegan_ini5][ acumulados5 >= mediana5, id ]
      ids_juegan_fin5  <- planilla_cazatalentos[ ids_juegan_ini5][ prob >= mediana5, id ]
      ids_juegan_ini5<-ids_juegan_fin5
      
      #data<-planilla_cazatalentos[ ids_juegan_ini5,  acumulados5]
      data<-planilla_cazatalentos[ ids_juegan_ini5,  prob]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos5<-mean(data[1]) 
      } else
      {
        media_pos5<-data
      }
    }
    
    
    if (usa_grupo[6]==T){
      resultado6<-planilla_cazatalentos[ ids_juegan_ini6, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini6,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini6,  c(paste0('aciertos',i))  := resultado6 ]  
      #planilla_cazatalentos[ ids_juegan_ini6,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini6,  acumulados6 := acumulados6+resultado6] 
      #planilla_cazatalentos[ ids_juegan_ini6,  acumulados := acumulados+resultado] 
      #planilla_cazatalentos[ ids_juegan_ini6,  c(paste0('p',i)) := acumulados6/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini6,  c(paste0('s',i)):=sqrt(acumulados6)/tiros_acum]
      
      mediana6  <- planilla_cazatalentos[ ids_juegan_ini6, median(acumulados6) ]
      mediana6<-planilla_cazatalentos[ ids_juegan_ini6,  median(prob)]
      ids_juegan_fin6  <- planilla_cazatalentos[ ids_juegan_ini6][ acumulados6 >= mediana6, id ]
      ids_juegan_fin6  <- planilla_cazatalentos[ ids_juegan_ini6][ prob >= mediana6, id ]
      ids_juegan_ini6<-ids_juegan_fin6
      #data<-planilla_cazatalentos[ ids_juegan_ini6,  acumulados6]
      data<-planilla_cazatalentos[ ids_juegan_ini6,  prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos6<-mean(data[1]) 
      } else
      {
        media_pos6<-data
      }
    }
    
    if (usa_grupo[7]==T){
      resultado7<-planilla_cazatalentos[ ids_juegan_ini7, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini7,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini7,  c(paste0('aciertos',i))  := resultado7 ]  
      #planilla_cazatalentos[ ids_juegan_ini7,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini7,  acumulados7 := acumulados7+resultado7] 
      #planilla_cazatalentos[ ids_juegan_ini7,  acumulados := acumulados+resultado] 
      #planilla_cazatalentos[ ids_juegan_ini7,  c(paste0('p',i)) := acumulados7/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini7,  c(paste0('s',i)):=sqrt(acumulados7)/tiros_acum]
      
      mediana7  <- planilla_cazatalentos[ ids_juegan_ini7, median(acumulados7) ]
      mediana7<-planilla_cazatalentos[ ids_juegan_ini7,  median(prob)]
      ids_juegan_fin7  <- planilla_cazatalentos[ ids_juegan_ini7][ acumulados7 >= mediana7, id ]
      ids_juegan_fin7  <- planilla_cazatalentos[ ids_juegan_ini7][ prob >= mediana7, id ]
      ids_juegan_ini7<-ids_juegan_fin7
      
      #data<-planilla_cazatalentos[ ids_juegan_ini7,  acumulados7]
      data<-planilla_cazatalentos[ ids_juegan_ini7,   prob]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos7<-mean(data[1]) 
      } else
      {
        media_pos7<-data
      }
    }
    
    
    if (usa_grupo[8]==T){
      #resultado  <- gimnasio_tirar( ids_juegan_ini8, ctiros[i])
      resultado8<-planilla_cazatalentos[ ids_juegan_ini8, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini8,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini8,  c(paste0('aciertos',i))  := resultado8 ]  
      #planilla_cazatalentos[ ids_juegan_ini8,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini8,  acumulados8 := acumulados8+resultado8] 
      #planilla_cazatalentos[ ids_juegan_ini8,  acumulados := acumulados+resultado] 
      #planilla_cazatalentos[ ids_juegan_ini8,  c(paste0('p',i)) := acumulados8/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini8,  c(paste0('s',i)):=sqrt(acumulados8)/tiros_acum]
      
      mediana8  <- planilla_cazatalentos[ ids_juegan_ini8, median(acumulados8) ]
      mediana8<-planilla_cazatalentos[ ids_juegan_ini8,  median(prob)]
      ids_juegan_fin8  <- planilla_cazatalentos[ ids_juegan_ini8][ acumulados8 >= mediana8, id ]
      ids_juegan_fin8  <- planilla_cazatalentos[ ids_juegan_ini8][ prob >= mediana8, id ]
      ids_juegan_ini8<-ids_juegan_fin8
      #data<-planilla_cazatalentos[ ids_juegan_ini8,  acumulados8]
      data<-planilla_cazatalentos[ ids_juegan_ini8,  prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos8<-mean(data[1]) 
      } else
      {
        media_pos8<-data
      }
    }
    
    if (usa_grupo[9]==T) {
      #resultado  <- gimnasio_tirar( ids_juegan_ini9, ctiros[i])
      resultado9<-planilla_cazatalentos[ ids_juegan_ini9, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini9,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini9,  c(paste0('aciertos',i))  := resultado9 ]  
      #planilla_cazatalentos[ ids_juegan_ini9,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini9,  acumulados9 := acumulados9+resultado9] 
      #planilla_cazatalentos[ ids_juegan_ini9,  acumulados := acumulados+resultado] 
      #planilla_cazatalentos[ ids_juegan_ini9,  c(paste0('p',i)) := acumulados9/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini9,  c(paste0('s',i)):=sqrt(acumulados9)/tiros_acum]
      
      mediana9  <- planilla_cazatalentos[ ids_juegan_ini9, median(acumulados9) ]
      mediana9<-planilla_cazatalentos[ ids_juegan_ini9,  median(prob)]
      ids_juegan_fin9  <- planilla_cazatalentos[ ids_juegan_ini9][ acumulados9 >= mediana9, id ]
      ids_juegan_fin9  <- planilla_cazatalentos[ ids_juegan_ini9][ prob >= mediana9, id ]
      ids_juegan_ini9<-ids_juegan_fin9
      #data<-planilla_cazatalentos[ ids_juegan_ini9,  acumulados9]
      data<-planilla_cazatalentos[ ids_juegan_ini9,  prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos9<-mean(data[1]) 
      } else
      {
        media_pos9<-data
      }
    }
    
    if (usa_grupo[10]==T){
      #resultado  <- gimnasio_tirar( ids_juegan_ini10, ctiros[i])
      resultado10<-planilla_cazatalentos[ ids_juegan_ini10, aciertos ]
      #planilla_cazatalentos[ ids_juegan_ini10,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      planilla_cazatalentos[ ids_juegan_ini10,  c(paste0('aciertos',i))  := resultado10 ]  
      #planilla_cazatalentos[ ids_juegan_ini10,  tiros_acum := tiros_acum+ctiros[i]]  
      planilla_cazatalentos[ ids_juegan_ini10,  acumulados10 := acumulados10+resultado10] 
      #planilla_cazatalentos[ ids_juegan_ini10,  acumulados := acumulados+resultado] 
      #planilla_cazatalentos[ ids_juegan_ini10,  c(paste0('p',i)) := acumulados10/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_ini10,  c(paste0('s',i)):=sqrt(acumulados10)/tiros_acum]
      
      mediana10  <- planilla_cazatalentos[ ids_juegan_ini10, median(acumulados10) ]
      mediana10<-planilla_cazatalentos[ ids_juegan_ini10,  median(prob)]
      ids_juegan_fin10  <- planilla_cazatalentos[ ids_juegan_ini10][ acumulados10 >= mediana10, id ]
      ids_juegan_fin10  <- planilla_cazatalentos[ ids_juegan_ini10][ prob >= mediana10, id ]
      ids_juegan_ini10<-ids_juegan_fin10
      #data<-planilla_cazatalentos[ ids_juegan_ini10,  acumulados10]
      data<-planilla_cazatalentos[ ids_juegan_ini10,  prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos10<-mean(data[1]) 
      } else
      {
        media_pos10<-data
      }
    }
    
    if (usa_grupo[11]==T){
      resultado11<-planilla_cazatalentos[ ids_juegan_ini11, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini11,  c(paste0('aciertos',i))  := resultado11 ]  
      planilla_cazatalentos[ ids_juegan_ini11,  acumulados11 := acumulados11+resultado11] 
      planilla_cazatalentos[ ids_juegan_ini11,  acumulados := acumulados+resultado11] 
      
      mediana11  <- planilla_cazatalentos[ ids_juegan_ini11, median(acumulados11) ]
      mediana11<-planilla_cazatalentos[ ids_juegan_ini11,  median(prob)]
      ids_juegan_fin11  <- planilla_cazatalentos[ ids_juegan_ini11][ acumulados11 >= mediana11, id ]
      ids_juegan_fin11  <- planilla_cazatalentos[ ids_juegan_ini11][ prob >= mediana11, id ]
      ids_juegan_ini11<-ids_juegan_fin11
      #data<-planilla_cazatalentos[ ids_juegan_ini11,  acumulados11]
      data<-planilla_cazatalentos[ ids_juegan_ini11,  prob]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos11<-mean(data[1]) 
      } else
      {
        media_pos11<-data
      }
    }
    
    if (usa_grupo[12]==T){
      resultado12<-planilla_cazatalentos[ ids_juegan_ini12, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini12,  c(paste0('aciertos',i))  := resultado12 ]  
      planilla_cazatalentos[ ids_juegan_ini12,  acumulados12 := acumulados12+resultado12] 
      
      mediana12  <- planilla_cazatalentos[ ids_juegan_ini12, median(acumulados12) ]
      mediana12<-planilla_cazatalentos[ ids_juegan_ini12,  median(prob)]
      ids_juegan_fin12  <- planilla_cazatalentos[ ids_juegan_ini12][ acumulados12 >= mediana12, id ]
      ids_juegan_fin12  <- planilla_cazatalentos[ ids_juegan_ini12][ prob >= mediana12, id ]
      ids_juegan_ini12<-ids_juegan_fin12
      #data<-planilla_cazatalentos[ ids_juegan_ini12,  acumulados12]
      data<-planilla_cazatalentos[ ids_juegan_ini12,  prob]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos12<-mean(data[1]) 
      } else
      {
        media_pos12<-data
      }
    }
    
    if (usa_grupo[13]==T){
      resultado13<-planilla_cazatalentos[ ids_juegan_ini13, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini13,  c(paste0('aciertos',i))  := resultado13 ]  
      planilla_cazatalentos[ ids_juegan_ini13,  acumulados13 := acumulados13+resultado13] 
      
      mediana13  <- planilla_cazatalentos[ ids_juegan_ini13, median(acumulados13) ]
      mediana13<-planilla_cazatalentos[ ids_juegan_ini13,  median(prob)]
      ids_juegan_fin13  <- planilla_cazatalentos[ ids_juegan_ini13][ acumulados13 >= mediana13, id ]
      ids_juegan_fin13  <- planilla_cazatalentos[ ids_juegan_ini13][ prob >= mediana13, id ]
      ids_juegan_ini13<-ids_juegan_fin13
      #data<-planilla_cazatalentos[ ids_juegan_ini13,  acumulados13]
      data<-planilla_cazatalentos[ ids_juegan_ini13,  prob]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos13<-mean(data[1]) 
      } else
      {
        media_pos13<-data
      }
    }
    
    if (usa_grupo[14]==T){
      resultado14<-planilla_cazatalentos[ ids_juegan_ini14, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini14,  c(paste0('aciertos',i))  := resultado14 ]  
      planilla_cazatalentos[ ids_juegan_ini14,  acumulados14 := acumulados14+resultado14] 
      
      mediana14  <- planilla_cazatalentos[ ids_juegan_ini14, median(acumulados14) ]
      mediana14<-planilla_cazatalentos[ ids_juegan_ini14,  median(prob)]
      ids_juegan_fin14  <- planilla_cazatalentos[ ids_juegan_ini14][ acumulados14 >= mediana14, id ]
      ids_juegan_fin14  <- planilla_cazatalentos[ ids_juegan_ini14][ prob >= mediana14, id ]
      ids_juegan_ini14<-ids_juegan_fin14
      #data<-planilla_cazatalentos[ ids_juegan_ini14,  acumulados14]
      data<-planilla_cazatalentos[ ids_juegan_ini14,  prob]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos14<-mean(data[1]) 
      } else
      {
        media_pos14<-data
      }
    }
    
    if (usa_grupo[15]==T){
      resultado15<-planilla_cazatalentos[ ids_juegan_ini15, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini15,  c(paste0('aciertos',i))  := resultado15 ]  
      planilla_cazatalentos[ ids_juegan_ini15,  acumulados15 := acumulados15+resultado15] 
      
      mediana15  <- planilla_cazatalentos[ ids_juegan_ini15, median(acumulados15) ]
      mediana15<-planilla_cazatalentos[ ids_juegan_ini15,  median(prob)]
      ids_juegan_fin15  <- planilla_cazatalentos[ ids_juegan_ini15][ acumulados15 >= mediana15, id ]
      ids_juegan_fin15  <- planilla_cazatalentos[ ids_juegan_ini15][ prob>= mediana15, id ]
      ids_juegan_ini15<-ids_juegan_fin15
      #data<-planilla_cazatalentos[ ids_juegan_ini15,  acumulados15]
      data<-planilla_cazatalentos[ ids_juegan_ini15,  prob]
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos15<-mean(data[1]) 
      } else
      {
        media_pos15<-data
      }
    }
    
    if (usa_grupo[16]==T){
      resultado16<-planilla_cazatalentos[ ids_juegan_ini16, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini16,  c(paste0('aciertos',i))  := resultado16 ]  
      planilla_cazatalentos[ ids_juegan_ini16,  acumulados16 := acumulados16+resultado16] 
      
      mediana16  <- planilla_cazatalentos[ ids_juegan_ini16, median(acumulados16) ]
      mediana16<-planilla_cazatalentos[ ids_juegan_ini16,  median(prob)]
      ids_juegan_fin16  <- planilla_cazatalentos[ ids_juegan_ini16][ acumulados16 >= mediana16, id ]
      ids_juegan_fin16  <- planilla_cazatalentos[ ids_juegan_ini16][ prob >= mediana16, id ]
      ids_juegan_ini16<-ids_juegan_fin16
      #data<-planilla_cazatalentos[ ids_juegan_ini16,  acumulados16]
      data<-planilla_cazatalentos[ ids_juegan_ini16,  prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos16<-mean(data[1]) 
      } else
      {
        media_pos16<-data
      }
    }
    
    if (usa_grupo[17]==T){
      resultado17<-planilla_cazatalentos[ ids_juegan_ini17, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini17,  c(paste0('aciertos',i))  := resultado17 ]  
      planilla_cazatalentos[ ids_juegan_ini17,  acumulados17 := acumulados17+resultado17] 
      
      mediana17  <- planilla_cazatalentos[ ids_juegan_ini17, median(acumulados17) ]
      mediana17<-planilla_cazatalentos[ ids_juegan_ini17,  median(prob)]
      ids_juegan_fin17  <- planilla_cazatalentos[ ids_juegan_ini17][ acumulados17 >= mediana17, id ]
      ids_juegan_fin17  <- planilla_cazatalentos[ ids_juegan_ini17][ prob >= mediana17, id ]
      ids_juegan_ini17<-ids_juegan_fin17
      #data<-planilla_cazatalentos[ ids_juegan_ini17,  acumulados17]
      data<-planilla_cazatalentos[ ids_juegan_ini17,prob]
     
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos17<-mean(data[1]) 
      } else
      {
        media_pos17<-data
      }
    }
    
    
    if (usa_grupo[18]==T){
      resultado18<-planilla_cazatalentos[ ids_juegan_ini18, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini18,  c(paste0('aciertos',i))  := resultado18 ]  
      planilla_cazatalentos[ ids_juegan_ini18,  acumulados18 := acumulados18+resultado18] 
      
      mediana18  <- planilla_cazatalentos[ ids_juegan_ini18, median(acumulados18) ]
      mediana18<-planilla_cazatalentos[ ids_juegan_ini18,  median(prob)]
      ids_juegan_fin18  <- planilla_cazatalentos[ ids_juegan_ini18][ acumulados18 >= mediana18, id ]
      ids_juegan_fin18  <- planilla_cazatalentos[ ids_juegan_ini18][ prob >= mediana18, id ]
      ids_juegan_ini18<-ids_juegan_fin18
      #data<-planilla_cazatalentos[ ids_juegan_ini18,  acumulados18]
      data<-planilla_cazatalentos[ ids_juegan_ini18,  prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos18<-mean(data[1]) 
      } else
      {
        media_pos18<-data
      }
    }
    
    if (usa_grupo[19]==T){
      resultado19<-planilla_cazatalentos[ ids_juegan_ini19, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini19,  c(paste0('aciertos',i))  := resultado19 ]  
      planilla_cazatalentos[ ids_juegan_ini19,  acumulados19 := acumulados19+resultado19] 
      
      mediana19  <- planilla_cazatalentos[ ids_juegan_ini19, median(acumulados19) ]
      mediana19<-planilla_cazatalentos[ ids_juegan_ini19,  median(prob)]
      ids_juegan_fin19  <- planilla_cazatalentos[ ids_juegan_ini19][ acumulados19 >= mediana19, id ]
      ids_juegan_fin19  <- planilla_cazatalentos[ ids_juegan_ini19][ prob >= mediana19, id ]
      ids_juegan_ini19<-ids_juegan_fin19
      #data<-planilla_cazatalentos[ ids_juegan_ini19,  acumulados19]
      data<-planilla_cazatalentos[ ids_juegan_ini19,  prob]
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos19<-mean(data[1]) 
      } else
      {
        media_pos19<-data
      }
    }
    
    if (usa_grupo[20]==T){
      resultado20<-planilla_cazatalentos[ ids_juegan_ini20, aciertos ]
      planilla_cazatalentos[ ids_juegan_ini20,  c(paste0('aciertos',i))  := resultado20 ]  
      planilla_cazatalentos[ ids_juegan_ini20,  acumulados20 := acumulados20+resultado20] 
      
      data<-planilla_cazatalentos[ ids_juegan_ini20,  prob]
      
      mediana20  <- planilla_cazatalentos[ ids_juegan_ini20, median(acumulados20) ]
      mediana20<-planilla_cazatalentos[ ids_juegan_ini20,  median(prob)]
      ids_juegan_fin20  <- planilla_cazatalentos[ ids_juegan_ini20][ acumulados20 >= mediana20, id ]
      ids_juegan_fin20  <- planilla_cazatalentos[ ids_juegan_ini20][ prob >= mediana20, id ]
      ids_juegan_ini20<-ids_juegan_fin20
      #data<-planilla_cazatalentos[ ids_juegan_ini20,  acumulados20]
      
      
      if (length(data)>=2){
        data<-sort(data,decreasing = TRUE)
        media_pos20<-mean(data[1]) 
      } else
      {
        media_pos20<-data
      }
    }
    
    if (ronda_final==T){
      resultadof<-planilla_cazatalentos[ ids_juegan_ini, aciertos ]
      #resultadof  <- gimnasio_tirar( ids_juegan_final, ctiros[i])
      #planilla_cazatalentos[ ids_juegan_final,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
      #planilla_cazatalentos[ ids_juegan_final,  c(paste0('aciertos',i))  := resultadof ]  
      #planilla_cazatalentos[ ids_juegan_final,  tiros_acum := tiros_acum+ctiros[i]]  
      #planilla_cazatalentos[ ids_juegan_final,  acumulados10 := acumulados10+resultado] 
      planilla_cazatalentos[ ids_juegan_ini,  acumulados := acumulados+resultadof] 
      #planilla_cazatalentos[ ids_juegan_final,  c(paste0('p',i)) := acumulados10/tiros_acum]  
      #planilla_cazatalentos[ ids_juegan_final,  c(paste0('s',i)):=sqrt(acumulados10)/tiros_acum]
      
      medianaf  <- planilla_cazatalentos[ ids_juegan_ini, median(acumulados) ]
      ids_juegan_final  <- planilla_cazatalentos[ ids_juegan_ini][ acumulados >= medianaf, id ]
      ids_juegan_ini<-ids_juegan_final
      #print(ids_juegan_ini)
    }
    #print(i)
    #print(mediana1)
    #print(planilla_cazatalentos[ ids_juegan_ini1, mean(acumulados1/tiros_acum) ])
    #print(mediana2)
    #print(planilla_cazatalentos[ ids_juegan_ini2, mean(acumulados2/tiros_acum) ])
    
    #media1<-c(unlist(media1),planilla_cazatalentos[ ids_juegan_ini1, mean(acumulados1/tiros_acum) ])
    #media2<-c(unlist(media2),planilla_cazatalentos[ ids_juegan_ini2, mean(acumulados2/tiros_acum) ])
    #if 
    
    #ids_final<-c(ids_juegan_ini1,ids_juegan_ini2,ids_juegan_ini3,ids_juegan_ini4,ids_juegan_ini5,ids_juegan_ini6,ids_juegan_ini7,ids_juegan_ini8,ids_juegan_ini9,ids_juegan_ini10)
    if (ronda_final==F){
      ids_final<-c(ids_juegan_ini1,ids_juegan_ini2,ids_juegan_ini3,ids_juegan_ini4,ids_juegan_ini5,ids_juegan_ini6,ids_juegan_ini7,ids_juegan_ini8,ids_juegan_ini9,ids_juegan_ini10,ids_juegan_ini11,ids_juegan_ini12,ids_juegan_ini13,ids_juegan_ini14,ids_juegan_ini15,ids_juegan_ini16,ids_juegan_ini17,ids_juegan_ini18,ids_juegan_ini19,ids_juegan_ini20)
      ids_juegan_ini<-ids_final
    }
    #if (i==4){
    #print(ids_juegan_ini)
    #}
    #print(media_pre1)
    #print(media1)
    
    #print(media_pre2)
    #print(media2)
    
    #
    
    if (i==2) {
      
      medianas<-c(media_pos1,media_pos2,media_pos3,media_pos4,media_pos5,media_pos6,media_pos7,media_pos8,media_pos9,media_pos10,media_pos11,media_pos12,media_pos13,media_pos14,media_pos15,media_pos16,media_pos17,media_pos18,media_pos19,media_pos20)
      #print(medianas)
      
      rango<-c("1-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90","91-95","96-100")
      grupo<-1:20
      ids<-c('ids_juegan_ini1','ids_juegan_ini2','ids_juegan_ini3','ids_juegan_ini4','ids_juegan_ini5','ids_juegan_ini6','ids_juegan_ini7','ids_juegan_ini8','ids_juegan_ini9','ids_juegan_ini10','ids_juegan_ini11','ids_juegan_ini12','ids_juegan_ini13','ids_juegan_ini14','ids_juegan_ini15','ids_juegan_ini16','ids_juegan_ini17','ids_juegan_ini18','ids_juegan_ini19','ids_juegan_ini20')
      
      data<-data.table(rango,medianas,grupo,ids)
      colnames(data)<-c('rango','medianas','grupo','ids')
      data <- data[with(data,order(-medianas)),]
      unicos<-unique(data[,medianas])
      #print(mean(medianas))
      #th<-unicos[6]
      th<-unicos[length(unicos)]
      
      grupos_continuan<-data[medianas>=th]
      #print(grupos_continuan)
      
      data<-data.table(ids_juegan_ini,planilla_cazatalentos[ids_juegan_ini,prob])
      colnames(data)<-c('id','prob')
      data <- data[with(data,order(-prob)),]
      #print(data)
      
      lista<-grupos_continuan[,grupo]
      listaid<-grupos_continuan[,ids]
      usa_grupo<-rep(F,20)
      usa_grupo<-rep(T,20)
      ids_final<-list()
      for (jj in 1:length(lista)){
        #ids_final<-c(unlist(ids_final),unlist(get(listaid[jj])))
        #usa_grupo[lista[jj]]<-T
      }
      #print(length(ids_final))
      
      ronda_final<-c(F)
      
      #ids_final<-c(ids_juegan_ini1,ids_juegan_ini2,ids_juegan_ini3,ids_juegan_ini4,ids_juegan_ini5,ids_juegan_ini6,ids_juegan_ini7,ids_juegan_ini8,ids_juegan_ini9,ids_juegan_ini10,ids_juegan_ini11,ids_juegan_ini12,ids_juegan_ini13,ids_juegan_ini14,ids_juegan_ini15,ids_juegan_ini16,ids_juegan_ini17,ids_juegan_ini18,ids_juegan_ini19,ids_juegan_ini20)
      #ids_juegan_ini<-ids_final
      
      #print(usa_grupo)
        
    }
    #  indice_max<-which.max(maximos)
    #  indice_min<-which.min(maximos)
    #    if (media1[3]>media2[3]){
    #      usa_grupo[2]<-F
    #    } else {
    #      usa_grupo[1]<-F
    #    }  
    #  } 
    #}
    #media2<-c(unlist(media2),planilla_cazatalentos[ ids_juegan_ini2, mean(acumulados2/tiros_acum) ])
    #print(media1)
    #print(media2)
    #print(length(ids_juegan_ini1))
    #print(length(ids_juegan_ini2))
  }
  
  
  
  
  
  #data<-data.table(ids_juegan_ini,planilla_cazatalentos[ids_juegan_ini,acumulados],planilla_cazatalentos[ids_juegan_ini,aciertos1/85],planilla_cazatalentos[ids_juegan_ini,aciertos2/60],planilla_cazatalentos[ids_juegan_ini,aciertos3/60],planilla_cazatalentos[ids_juegan_ini,prob])
  #colnames(data)<-c('id','acumulados','aciertos1','aciertos2','aciertos3','media')
  #data <- data[with(data,order(-media)),]
  #print(data)
  
  #pos_mejor <-  data[ , which.max(acumulados) ]
  #id_mejor  <-  data[ pos_mejor, id ]
  #print(id_mejor)
  
  pos_mejor <-  planilla_cazatalentos[ , which.max(acumulados) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  #print(id_mejor)
  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )
  
  return( veredicto )
}
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia A

set.seed( 100003 )  #debe ir una sola vez, ANTES de los experimentos
#set.seed(123456)

tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )

for( experimento  in  1:10000 )
{
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy
  
  veredicto  <- Estrategia_B()
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
}

cat("\n")

tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]

tiros_total 
tasa_eleccion_correcta




#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#ideas principales, en cada ronda se acumulan los aciertos de los que siguen en carrera
#la estimadoabilida del primer tiro tiene que superar al 50% para que super a la mediana,
#el segundo tiro deberia superar a 75% ya que es la mediana de la mediana (si se dio el evento ell segundo tiro p>1-(1-p(1))*.5)
#sigue así hasta que hayan tirado los ultimos los 415 tiros y con suerte el hganador llego hasta ahí


#limpio la memoria
rm( list=ls() )
gc()

require("data.table")

ftirar <- function( estimado, qty )
{
  return(  sum( runif(qty) < estimado ) )
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


comparacion_muestras <- function(poblacion) {
     muestra_a <- sample(poblacion, size = 1)
     muestra_b <- sample(poblacion, size = 49)
     t.test(muestra_a, muestra_b, alternative = "greater", var.equal = TRUE)$p.value #"two.sided"
}

estBetaParams <- function(mu, var) {
   alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
   beta <- alpha * (1 / mu - 1)
   return(params = list(alpha = alpha, beta = beta))
}

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

  ctiros<-c(90,60,70,70,100,65,65) #en 4 tiros .9441, en 5 tiros .9671, en 6 tiros .9718
  
  #ctiros<-c(90,62,65,70,95,100,65) #en 6 tiros .9706
  
  #ctiros<-c(85,90,65,70,95,100,65) #en 6 tiros .9706
  
  #ctiros<-c(90,90,90,90,90,100,65) #en 6 tiros .9706
  
  ctiros<-c(85,62,55,60,60,60,60) #en 4 tiros .9441, en 5 tiros .9671, en 6 tiros .9718
  
  ctiros<-c(85,62,55,55,55,55,50) 
  
  
  ids_juegan_ini  <- 1:100   #los jugadores que participan en la ronda,
  
  planilla_cazatalentos[ ids_juegan_ini,  tiros_acum := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini,  acumulados := 0]  #registro en la planilla
  planilla_cazatalentos[ ids_juegan_ini,  estimado := 1]  #registro en la planilla
  
  th<-0.599
  #th<-1
  #ths<-c(1,1-0.634,1-.401,1-0.222,1-.1225,1-0.005852)
  pcorte<-c(0.5,0.25,.125,0.0625,0.0312,0.0156,0.01)
  #pcorte<-c(0.25,.125,0.0625,0.0312,0.0156,0.01)
  ths<-list()
  alpha<-0.01
  
  for (i in 1:4){
    
    m<-length(ids_juegan_ini)
    #print(m)
    
    if (m>1){
    #print(paste0('ronda',i))
    planilla_cazatalentos[ ids_juegan_ini,  c(paste0('tiros',i))  := ctiros[i] ]  #registro en la planilla que tiran x tiros
    resultado  <- gimnasio_tirar( ids_juegan_ini, ctiros[i])
    
    planilla_cazatalentos[ ids_juegan_ini,  c(paste0('aciertos',i))  := resultado ]  
    planilla_cazatalentos[ ids_juegan_ini,  tiros_acum := tiros_acum+ctiros[i]]  
    planilla_cazatalentos[ ids_juegan_ini,  acumulados := acumulados+resultado]  
    planilla_cazatalentos[ ids_juegan_ini,  c(paste0('p',i)) := acumulados/tiros_acum]  #para calcular la tasa de aciertos si uno quisiera
    
    planilla_cazatalentos[ ids_juegan_ini,  c(paste0('p',i)) := acumulados/tiros_acum]  #para calcular la tasa de aciertos si uno quisiera
    planilla_cazatalentos[ ids_juegan_ini,  c(paste0('s',i)):=sqrt(acumulados)/tiros_acum] #para calcular el desvio si uno quisiera
 
    
    mu<-planilla_cazatalentos[ ids_juegan_ini,  mean(acumulados/tiros_acum)]  #para calcular la tasa de aciertos si uno quisiera
    
    var<-planilla_cazatalentos[ ids_juegan_ini,  var(acumulados/tiros_acum)]  #para calcular la tasa de aciertos si uno quisiera
    
    m<-estBetaParams(mu,var)
    
    #print(m)
#    print(planilla_cazatalentos[ids_juegan_ini,acumulados/tiros_acum])
    #m1 <- MASS::fitdistr(planilla_cazatalentos[ids_juegan_ini,acumulados/tiros_acum], dbeta, start = list(shape1 = 1, shape2 = 10))
    
    #print(m1)
    
    #alpha0 <- m1$estimate[1]
    #beta0 <- m1$estimate[2]
    
    
    planilla_cazatalentos[ ids_juegan_ini,  estimado := (acumulados+m$alpha)/(tiros_acum+m$alpha+m$beta)]  #para calcular la tasa de aciertos si uno quisiera
    
    planilla_cazatalentos[ ids_juegan_ini,  alphaj := (acumulados+m$alpha)]  #para calcular la tasa de aciertos si uno quisiera
    planilla_cazatalentos[ ids_juegan_ini,  betaj := (tiros_acum-acumulados+m$beta)]  #para calcular la tasa de aciertos si uno quisiera
    
    #mediana  <- planilla_cazatalentos[ ids_juegan_ini, median(acumulados) ]
    
    mediana  <- planilla_cazatalentos[ ids_juegan_ini, median(estimado) ]
    planilla_cazatalentos[ ids_juegan_ini,  pepj := pbeta(mediana, alphaj, betaj)]  #para calcular la tasa de aciertos si uno quisiera
    #planilla_cazatalentos[ ids_juegan_ini,  qvaluej := cummean(pepj)]  #para calcular la tasa de aciertos si uno quisiera
    
    #print(mediana)
    
    #if (i>5){
    #  mediana<-((mediana+1)/(mediana+2))
    #}
    
    #ths<-c(unlist(ths),1-(1-alpha)^m)
    #ths<-c(unlist(ths),1-(1-alpha)^m)
    #print(ths)
    
    #ids_juegan_fin  <- planilla_cazatalentos[ ids_juegan_ini][ acumulados >= mediana, id ]
    ids_juegan_fin  <- planilla_cazatalentos[ ids_juegan_ini][ estimado >= mediana, id ]
    ids_juegan_ini<-ids_juegan_fin
    
    #planilla_cazatalentos <- planilla_cazatalentos[with(planilla_cazatalentos,order(-estimado)),]
    #planilla_cazatalentos[ ids_juegan_ini, qvaluej := cummean(pepj)]  #para calcular la tasa de aciertos si uno quisiera
    
    
    #planilla_cazatalentos[, qvaluej := cummean(pepj)]  #para calcular la tasa de aciertos si uno quisiera
    
    #planilla_cazatalentos[, qvaluej := qvalue(pepj)]  #para calcular la tasa de aciertos si uno quisiera
    
    #ids_juegan_x<-data[,qvalue<pcorte[i]]
    
    #print(poblacion)
    #p_values <- replicate(n = 1000, expr = comparacion_muestras(poblacion))
    
    #print(p_values)
    #print(sum(p_values < 0.05))
    
    #print(planilla_cazatalentos[ ids_juegan_ini] )
    #th<-planilla_cazatalentos[ ids_juegan_ini,  median(estimado)] 
    #print(th)
    th<-0.599^i
    
    #if (i>0){
    #  ids_juegan_fin  <- planilla_cazatalentos[ ids_juegan_ini][ estimado >= th, id ]
    #  ids_juegan_ini<-ids_juegan_fin
      #print(length(ids_juegan_ini))
    #}
    #print(pcorte[i])
    if (i>0) {
      data<-data.table(ids_juegan_ini,planilla_cazatalentos[ids_juegan_ini,estimado],planilla_cazatalentos[ids_juegan_ini,acumulados/tiros_acum],planilla_cazatalentos[ids_juegan_ini,alphaj],planilla_cazatalentos[ids_juegan_ini,betaj],planilla_cazatalentos[ids_juegan_ini,pepj])
      colnames(data)<-c('id','estimado','acumulados','alpha','beta','pep')
      data <- data[with(data,order(-estimado)),]
      data[ , qvalue := cummean(pep)]  #para calcular la tasa de aciertos si uno quisiera
      #ids_juegan_x<-data[,qvalue<pcorte[i]]
      data <- data[with(data,order(id)),]
      
      planilla_cazatalentos[ids_juegan_ini,qvalor:=data[,qvalue]]
      
      #print(length(ids_juegan_ini))
      if (i<=5){
      ids_juegan_fin  <- planilla_cazatalentos[ ids_juegan_ini][ qvalor <= pcorte[i], id ]
      ids_juegan_ini<-ids_juegan_fin
      #print(data)
      #print(length(ids_juegan_ini))
      }
      #print(data)
      #print(p.adjust(data[,pep],"holm"))
      #ids_juegan_ini<-ids_juegan_x
      #print(data)
    }
    
    }
    
    
    #print(mediana/sum(ctiros[1:i]))
    #media  <- planilla_cazatalentos[ ids_juegan_ini, mean(acumulados/tiros_acum) ]
    #desvio<-planilla_cazatalentos[ ids_juegan_ini, sd(acumulados/tiros_acum) ]
    #print(media)
    #print(desvio)
    #print(length(ids_juegan_ini))
    #print(resultado/ctiros[i])
    
    #if (i==2){
    #    data<-data.table(ids_juegan_ini,planilla_cazatalentos[ids_juegan_ini,estimado])
    #    colnames(data)<-c('id','estimado')
    #    data <- data[with(data,order(-estimado)),]
    #    print(data)
    #}

  }
  

  pos_mejor <-  planilla_cazatalentos[ , which.max(acumulados) ]
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]

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




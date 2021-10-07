x=c(85,146,207,268,329,390,420)
y1=c(49,39,38,41,43,46,24)  #15 termina decidiendo esto
y2=c(64,38,36,40,38,35,18)  #72 en 4 tiros cree esto
df1<-data.table(cbind(x,y1,y2))
ajuste1<-lm(x~y1,data = df1)
summary(ajuste1)
ajuste2<-lm(x~y2,data = df1)
summary(ajuste2)
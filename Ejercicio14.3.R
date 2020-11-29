library(ggplot2)

d=c()
df1=data.frame(NA)
df=data.frame(NA)
for( i in 0:10){
  rta=ppois(i,lambda = 2)
  d=c(d,rta)
}

for(i in 1:length(d)){
  df1=data.frame(r=c(df1$r,d[i]))
}
d=c(df1$r)
df=data.frame(r=c(0,d))
print(df)
contador=c(rep(0,length(df$r-1)))
nro=20
for(i in 1:nro){
  n=runif(1)
  for(o in 1:length(df$r)){
    if(n>df$r[o] & n<df$r[o+1]){
      contador[o]=contador[o]+1
    }
  }
}

graf=data.frame(cant=c(contador/nro),esperado=c(0:11))

ggplot(data=graf,aes(x=esperado,y=cant))+geom_bar(fill = "#FF6133",width=0.3,stat="identity")+labs(title="Ejercicio 14.3",x="Resultado",y="Probabilidad")

  
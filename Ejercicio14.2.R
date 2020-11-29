library(ggplot2)
x=0
y=0
z=0
w=0
c=20

for(i in 1:c){
  n=runif(1)
  if(n<0.3){
    x=x+1
  }else if(n>=0.3 & n<0.7){
    y=y+1
  }else if(n>=0.7 & n<0.9){
    z=z+1
  }else{
    w=w+1
  }
}
df <- data.frame(res=c("Peinarse", "Peinarse + Corte","Corte","Permanente"), cant=c(x/c,y/c,z/c,w/c))
df$res <- factor(df$res, levels = df$res)

print(df)
ggplot(data=df,aes(x=res,y=cant))+geom_bar(fill = "#FF6133",width=0.3,stat="identity")+labs(title="Ejercicio 14.2",x="Resultado",y="Probabilidad")

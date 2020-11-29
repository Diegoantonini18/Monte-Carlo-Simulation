library(ggplot2)

cara=0
cruz=0
n=10

for (i in 1:n){
  if(runif(1)>0.5){
    cara=cara+1
  }else{
    cruz=cruz+1
  }
}  

df <- data.frame(res=c("cara", "cruz"), cant=c(cara,cruz))
print(df)


ggplot(data=df,aes(x=res,y=cant))+geom_bar(fill = "#FF6133",width=0.3,stat="identity")+labs(title="Ejercicio 14.1",x="Resultado",y="Cantidad")  





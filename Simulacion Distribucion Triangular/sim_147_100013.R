#### Alumno: Diego Antonini
#### Padrón: 100013

library(ggplot2)

sim <- function(r1,r2) {
  a = 1
  b = 4
  m = 3
  h= 2 /(b-a)
  A1= (m-a)*h/2
  A2= (b-m)*h/2
  if(r1 < (m-a)/(b-a)){
    x=a+(m-a)*sqrt(r2)
  }else{
    x=b-(b-m)*sqrt(1-r2)
  }
  out <- x
  return(out)
}

##### DATOS PARA GRAFICAR
a = 1
b = 4
m = 3
h= 2 /(b-a)
A1= (m-a)*h/2
A2= (b-m)*h/2

##### FUNCION DE DENSIDAD
dfa=data.frame(x=c(a,m,b),y=c(0,h,0))
ggplot(data=dfa,aes(x=x,y=y))+geom_line(color="firebrick2",size=1)+
 labs(title="Funcion de densidad",caption="Diego Antonini 100013",x="x",y="f(x)",subtitle="")+ 
    expand_limits(x = 0, y = 0)+ scale_y_continuous(limits=c(0:1),expand=c(0,0),breaks=c(0,0.5,h,1),labels=c(0,0.5,expression(frac(2,"b-a")),1))+theme_bw()+
      theme(axis.line = element_line(color = "black", size=1, lineend = 'square'))+
        geom_segment(x = 0, y =h , xend = 3,yend=h, linetype = "dotted", color = "black",size=0.8)+
          geom_segment(x = 3, y =0 , xend =3,yend=h, linetype = "dotted", color = "black",size=0.8)+
              scale_x_continuous(expand = c(0, 0),limits = c(0,5) )+
                annotate("text",x=2.5,y=0.25,label="A1",size=5.2)+
                  annotate("text",x=3.30,y=0.25,label="A2",size=5.2)
                
        

#### FUNCION DE DISTRIBUCION
dfb=data.frame(x=c(0,a,m,b),y=c(0,(m-a)/(b-a),1,1))
ggplot(data=dfb,aes(x=x,y=y))+geom_step(color="#FF6133",size=1.2)+ expand_limits(x=0,y=0)+
  labs(title = "Función de distribución",subtitle="",caption="Diego Antonini 100013",x="x",y="F(x)")+
    geom_segment(x = 0, y =(m-a)/(b-a) , xend = 1, yend = (m-a)/(b-a), linetype = "dotted", color = "black",size=0.8)+theme_bw()+
      theme(axis.line = element_line(color = "black", size=1, lineend = 'square'))+
        scale_x_continuous(expand = c(0, 0),limits = c(0,4) )+
          scale_y_continuous(limits = c(0,1.1),breaks =c(0,(m-a)/(b-a),1),labels=c("0",expression(frac("m-a","b-a")),"1"))

### SIMULANDO 5 VALORES
for(i in 1:5){
  r1=runif(1)
  r2=runif(1)
  print(sim(r1,r2))
}           
sim(0.9,0.3)








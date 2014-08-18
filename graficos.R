#Grafico del Ashenfelter's dip

#Carga libreráa de gráficos
library(ggplot2)
library(reshape2)

dip<-melt(medias, measure.vars=c("Participantes", "No Participantes", "No Part. Apareados", "No Part. No Apareados"))
theme_set(theme_bw())

ash<-ggplot(dip, aes(x=Periodo, y=value, group=variable, colour=variable))+geom_line()+geom_point()
ash<-ash+theme(legend.position="bottom", legend.direction="horizontal")+theme(legend.title=element_blank())
ash<-ash+labs(color="Legend text")+labs(x="Período", y="Ingreso Medio", colour="variable")
ash<-ash+scale_x_discrete(breaks=seq(1, 10))+ggtitle("Ingreso promedio por grupos (Coeficientes Aleatorios)")
ash

#Para graficar la distribucion de theta_i necesito un nuevo data frame
categoria<-rep(0,n)
categoria<-replace(categoria, D_i==1, "Participantes")
categoria<-replace(categoria, D_i==0, "No Participantes")
categoria<-replace(categoria, D_i==0 & dat$partc==1, "No Part. Apareados")
categoria<-replace(categoria, D_i==0 & dat$partc==0, "No Part. No Apareados")
dens<-data.frame(id=seq(1,n,1),categoria,theta_i)

dens1<-melt(dens, id="id")
dist<-ggplot(dens1, aes(x=theta_i, fill=categoria)) + geom_density(alpha=.2) 
dist<-dist+labs(color="Legend text")+labs(x=expression(theta[i]), y="Densidad", colour="variable")
dist<-dist+theme(legend.title=element_blank())+theme(legend.position="bottom", legend.direction="horizontal")
dist<-dist+ggtitle("Distribución de theta en el caso de coeficientes aleatorios")
dist

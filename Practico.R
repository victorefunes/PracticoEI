#Parametros del modelo
n<-1000
beta<-rep(1000,n)
r<-0.1
rho<- 0.78
phi<-1
mean_alpha<-100
sd_alpha<-0 #(=0 para el modelo de coeficientes comunes, =300 para coef. aleatorios)
mean_theta<-0
sd_theta<-300
mean_epsilon<-0
sd_epsilon<-280
mean_v<-0
sd_v<-200
sd_z<-300

#Generar variables aleatorias
alpha_i<-rnorm(n, mean_alpha, sd_alpha)
theta_i<-rnorm(n, mean_theta, sd_theta)
V_i<-rnorm(n, mean_v, sd_v)
Epsilon_it<-replicate(10, rnorm(n, mean_epsilon, sd_epsilon)) 

#Generar data frame para guardar los datos
dat<-data.frame(matrix(0, ncol=10, nrow=n))

#Nombres de las 10 columnas de la matriz de ingresos
y<-rep(0,10)
for(i in 1:10){
  y[i]<-paste("y_",i, sep="")
}
names(dat)<-y

U_it<-matrix(0, nrow=n, ncol=10)
U_it[,1]=Epsilon_it[,1]

for(k in 2:10){
  U_it[,k]=rho*U_it[,k-1]+Epsilon_it[,k]
}

#Período incial
#Nadie participa antes de la implementación del programa
D_i<-rep(0, n)
dat[,1]<-beta+alpha_i*D_i+theta_i+U_it[,1]

#Períodos 2 a 5: se generan los ingresos según la ley de movimiento
for(k in 2:5){
  dat[,k]<-beta+alpha_i*D_i+theta_i+U_it[,k]
}

#Período 6: implementación del programa
#Función de costos: c_i=Z_i*Phi_i+V_i
#phi=1; V_i~N(0,200); Z_i~N(mu_z, 300)

#comando while - itera hasta que se cumpla la condición
mu<-700 #Coeficientes comunes
#mu<-4000 #Coeficientes aleatorios
ntreated<-0
y<-beta+alpha_i*D_i+theta_i+U_it[,6]
pb <- winProgressBar(title="Progreso", label="0% terminado", min=0, max=100, initial=0)

while(ntreated!=100){
  Z_i<-rnorm(n, mu, sd_z)
  c_i<-Z_i*phi+V_i
  D_i<-as.numeric(I((alpha_i/r-y-c_i)>0))
  ntreated<-as.numeric(table(D_i)[2])
  #print(paste("Media de u: ", mu))
  #print(paste("Numero de participantes: ", ntreated))  
  #si participantes < 1000 reduce la media de u, caso contrario la aumenta
  if(ntreated<100){
    mu=mu-1
  }else{
    mu=mu+1
  }
  info <- sprintf("%d%% terminado", round((i/100)*100))
  setWinProgressBar(pb, i/(100)*100, label=info)
}
close(pb)

#Reemplazo valores para el período 6 (0 para los que participan-D_i=1-)
dat[,6]<-(1-D_i)*(beta+alpha_i+theta_i+U_it[,6])


#Períodos 7 a 10: se generan los ingresos según la ley de movimiento
for(k in 7:10){
  dat[,k]<-beta+alpha_i*D_i+theta_i+U_it[,k]
}

#Primeros 6 individuos
head(dat)

#Agrego la columa de valores de alfa y D_i al data frame
dat<-cbind(dat, alpha_i, D_i)

#Carga la librería para hacer matching
library(MatchIt)
match<-matchit(D_i~y_4, method="nearest", distance="logit", data=dat, replace=TRUE)
summary(match, interactions = TRUE)

#Obtengo el conjunto de datos apareados (weights=1)
m.data<-match.data(match)

#Obtengo las medias y desviaciones estándar de los alfas para los grupos
#apareados y no apareados 
mean_alpha_i<-mean(dat$alpha_i)       #E(alpha_i)
w1<-subset(m.data, weights==1)
mean_alpha_i_Tr<-mean(w1$alpha_i) #E(alpha_i/D_i=1)
rm(w1)

tapply(m.data$alpha_i, m.data$weights, mean)

library(dplyr)
dat<-cbind(dat, match$weights, D_i)
names(dat)[names(dat)=="match$weights"] <- "weight"

#Cuadro de las medias por cada grupo
medias<-data.frame(matrix(0, ncol=4, nrow=10))
names(medias)<-c("Participantes", "No Participantes", "No Part. Apareados", "No Part. No Apareados")

partc<-as.numeric(I(dat$weight!=0))
dat<-cbind(dat, partc)

for(j in 1:10){
  medias[j,1]<-aggregate(dat[,j]~partc+D_i, dat, mean )[3,3]
  medias[j,2]<-aggregate(dat[,j]~D_i, dat, mean )[1,2]
  medias[j,3]<-aggregate(dat[,j]~partc+D_i, dat, mean )[2,3]
  medias[j,4]<-aggregate(dat[,j]~partc+D_i, dat, mean )[1,3]
}
Periodo<-seq(1,10,1)
medias<-cbind(Periodo, medias)

#generar graficos de las variables
#source("C:/Users/VictorF/Documents/PracticoEI/graficos.r")

###Estimador Cross-section 

matched<-subset(dat, partc==1)

#Periodo k+1
library(sandwich)
library(lmtest)

#Grupo no apareado
regc1<-lm(y_7~D_i, data=dat)
alfa_cs1<-summary(regc1)$coefficients[2,1]
regc1$newse<-vcovHC(regc1, type="HC1")
se_cs1<-coeftest(regc1,regc1$newse)[2,2]

#Grupo apareado
regc1m<-lm(y_7~D_i, data=matched)
alfa_cs1m<-summary(regc1m)$coefficients[2,1]
regc1m$newse<-vcovHC(regc1m, type="HC1")
se_cs1m<-coeftest(regc1m,regc1m$newse)[2,2]

#Periodo k+2
#Grupo no apareado
regc2<-lm(y_8~D_i, data=dat)
alfa_cs2<-summary(regc2)$coefficients[2,1]
regc2$newse<-vcovHC(regc2, type="HC1")
se_cs2<-coeftest(regc2,regc2$newse)[2,2]

#Grupo apareado
regc2m<-lm(y_8~D_i, data=matched)
alfa_cs2m<-summary(regc2m)$coefficients[2,1]
regc2m$newse<-vcovHC(regc2m, type="HC1")
se_cs2m<-coeftest(regc2m,regc2m$newse)[2,2]

#Periodo k+3
#Grupo no apareado
regc3<-lm(y_9~D_i, data=dat)
alfa_cs3<-summary(regc3)$coefficients[2,1]
regc3$newse<-vcovHC(regc3, type="HC1")
se_cs3<-coeftest(regc3,regc1$newse)[2,2]

#Grupo apareado
regc3m<-lm(y_9~D_i, data=matched)
alfa_cs3m<-summary(regc3m)$coefficients[2,1]
regc3m$newse<-vcovHC(regc3m, type="HC1")
se_cs3m<-coeftest(regc3m,regc3m$newse)[2,2]

#Periodo k+4
#Grupo no apareado
regc4<-lm(y_10~D_i, data=dat)
alfa_cs4<-summary(regc4)$coefficients[2,1]
regc4$newse<-vcovHC(regc4, type="HC1")
se_cs4<-coeftest(regc4,regc4$newse)[2,2]

#Grupo apareado
regc4m<-lm(y_10~D_i, data=matched)
alfa_cs4m<-summary(regc4m)$coefficients[2,1]
regc4m$newse<-vcovHC(regc4m, type="HC1")
se_cs4m<-coeftest(regc4m,regc4m$newse)[2,2]

##Estimador de diferencias en diferencias

library(lfe)
id<-seq(1,n,1)
dat<-cbind(id,dat)
matched<-subset(dat, partc==1)

#Periodo k+3 y k-1
#Grupo no apareado
Yit_31<-dat$y_9-dat$y_5
regdd1<-felm(Yit_31~D_i, clustervar=id, data=dat)
alfa_dd1<-summary(regdd1)$coefficients[2,1]
se_dd1<-summary(regdd1)$coefficients[2,2]

#Grupo apareado
Yit_31<-matched$y_9-matched$y_5
regdd1m<-felm(Yit_31~D_i, clustervar=matched$id, data=matched)
alfa_dd1m<-summary(regdd1m)$coefficients[2,1]
se_dd1m<-summary(regdd1m)$coefficients[2,2]

#Periodo k+3 y k-3
#Grupo no apareado
Yit_33<-dat$y_9-dat$y_3
regdd3<-felm(Yit_33~D_i, clustervar=id, data=dat)
alfa_dd3<-summary(regdd3)$coefficients[2,1]
se_dd3<-summary(regdd3)$coefficients[2,2]

#Grupo apareado
Yit_33<-matched$y_9-matched$y_3
regdd3m<-felm(Yit_33~D_i, clustervar=matched$id, data=matched)
alfa_dd3m<-summary(regdd3m)$coefficients[2,1]
se_dd3m<-summary(regdd3m)$coefficients[2,2]

#Periodo k+3 y k-5
#Grupo no apareado
Yit_35<-dat$y_9-dat$y_1
regdd5<-felm(Yit_35~D_i, clustervar=id, data=dat)
alfa_dd5<-summary(regdd5)$coefficients[2,1]
se_dd5<-summary(regdd5)$coefficients[2,2]

#Grupo apareado
Yit_35<-matched$y_9-matched$y_1
regdd5m<-felm(Yit_35~D_i, clustervar=matched$id, data=matched)
alfa_dd5m<-summary(regdd5m)$coefficients[2,1]
se_dd5m<-summary(regdd5m)$coefficients[2,2]

##Estimador de Variables instrumentales

#Cargar libreria para estimar VI
library(AER)

#Primera etapa
reg1e<-lm(D_i~Z_i)
corDZ<-summary(reg1e)$coefficients[2,1]
z<- summary(reg1e)$coefficients[2,1]/summary(reg1e)$coefficients[2,2]
p<-2*dnorm(-abs(z))

dat<-cbind(dat,Z_i)
matched<-subset(dat, partc==1)

#Periodo k+1
#Grupo no apareado
regiv1<-ivreg(y_7~D_i|Z_i, data=dat)
alfa_iv1<-summary(regiv1)$coefficients[2,1]
se_iv1<-summary(regiv1)$coefficients[2,2]

#Grupo apareado
regiv1m<-ivreg(y_7~D_i|Z_i, data=matched)
alfa_iv1m<-summary(regiv1m)$coefficients[2,1]
se_iv1m<-summary(regiv1m)$coefficients[2,2]

#Periodo k+2
#Grupo no apareado
regiv2<-ivreg(y_8~D_i|Z_i, data=dat)
alfa_iv2<-summary(regiv2)$coefficients[2,1]
se_iv2<-summary(regiv2)$coefficients[2,2]

#Grupo apareado
regiv2m<-ivreg(y_8~D_i|Z_i, data=matched)
alfa_iv2m<-summary(regiv2m)$coefficients[2,1]
se_iv2m<-summary(regiv2m)$coefficients[2,2]

#Periodo k+3
#Grupo no apareado
regiv3<-ivreg(y_9~D_i|Z_i, data=dat)
alfa_iv3<-summary(regiv3)$coefficients[2,1]
se_iv3<-summary(regiv3)$coefficients[2,2]

#Grupo apareado
regiv3m<-ivreg(y_9~D_i|Z_i, data=matched)
alfa_iv3m<-summary(regiv3m)$coefficients[2,1]
se_iv3m<-summary(regiv3m)$coefficients[2,2]

#Periodo k+4
#Grupo no apareado
regiv4<-ivreg(y_10~D_i|Z_i, data=dat)
alfa_iv4<-summary(regiv4)$coefficients[2,1]
se_iv4<-summary(regiv4)$coefficients[2,2]

#Grupo apareado
regiv4m<-ivreg(y_10~D_i|Z_i, data=matched)
alfa_iv4m<-summary(regiv4m)$coefficients[2,1]
se_iv4m<-summary(regiv4m)$coefficients[2,2]


#rm(list=ls())
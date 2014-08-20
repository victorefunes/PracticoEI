mat<-data.frame(matrix(0, ncol=49, nrow=100))
names(mat)<-c("alfa_cs1", "alfa_cs2", "alfa_cs3", "alfa_cs4", "alfa_dd1",
               "alfa_dd3", "alfa_dd5","alfa_iv1","alfa_iv2", "alfa_iv3", 
               "alfa_iv4", "se_cs1", "se_cs2", "se_cs3", "se_cs4","se_dd1",
               "se_dd3", "se_dd", "se_iv1", "se_iv2", "se_iv3", "se_iv4",
               "mean_alpha_i", "mean_alpha_i_Tr", "mu", "corDZ", "p",
               "alfa_cs1m", "alfa_cs2m", "alfa_cs3m", "alfa_cs4m", "alfa_dd1m",
               "alfa_dd3m", "alfa_dd5m", "alfa_iv1m", "alfa_iv2m", "alfa_iv3m",
               "alfa_iv4m", "se_cs1m", "se_cs2m", "se_cs3m", "se_cs4m", "se_dd1m",
               "se_dd3m", "se_dd5m", "se_iv1m", "se_iv2m", "se_iv3m", "se_iv4m")

for(w in 1:100){
  source('C:/Users/VictorF/Documents/PracticoEI/Practico.R', local=TRUE)
  print(paste("Simulacion: ", w, ""))
  mat[w,1]<-alfa_cs1
  mat[w,2]<-alfa_cs2
  mat[w,3]<-alfa_cs3
  mat[w,4]<-alfa_cs4
  mat[w,5]<-alfa_dd1
  mat[w,6]<-alfa_dd3
  mat[w,7]<-alfa_dd5
  mat[w,8]<-alfa_iv1
  mat[w,9]<-alfa_iv2
  mat[w,10]<-alfa_iv3
  mat[w,11]<-alfa_iv4
  mat[w,12]<-se_cs1
  mat[w,13]<-se_cs2
  mat[w,14]<-se_cs3
  mat[w,15]<-se_cs4
  mat[w,16]<-se_dd1
  mat[w,17]<-se_dd3
  mat[w,18]<-se_dd5
  mat[w,19]<-se_iv1
  mat[w,20]<-se_iv2
  mat[w,21]<-se_iv3
  mat[w,22]<-se_iv4
  mat[w,23]<-mean_alpha_i
  mat[w,24]<-mean_alpha_i_Tr
  mat[w,25]<-mu
  mat[w,26]<-corDZ
  mat[w,27]<-p
  mat[w,28]<-alfa_cs1m
  mat[w,29]<-alfa_cs2m
  mat[w,30]<-alfa_cs3m
  mat[w,31]<-alfa_cs4m
  mat[w,32]<-alfa_dd1m
  mat[w,33]<-alfa_dd3m
  mat[w,34]<-alfa_dd5m
  mat[w,35]<-alfa_iv1m
  mat[w,36]<-alfa_iv2m
  mat[w,37]<-alfa_iv3m
  mat[w,38]<-alfa_iv4m
  mat[w,39]<-se_cs1m
  mat[w,40]<-se_cs2m
  mat[w,41]<-se_cs3m
  mat[w,42]<-se_cs4m
  mat[w,43]<-se_dd1m
  mat[w,44]<-se_dd3m
  mat[w,45]<-se_dd5m
  mat[w,46]<-se_iv1m
  mat[w,47]<-se_iv2m
  mat[w,48]<-se_iv3m
  mat[w,49]<-se_iv4m
}

write.table(mat, file="C:/Users/VictorF/Documents/PracticoEI/montecarlo-comunes.csv", col.names=TRUE)

means<-apply(mat, 2, mean)
sds<-apply(mat, 2, sd)

mat_al<-read.csv("C:/Users/VictorF/Documents/PracticoEI/montecarlo-aleatorios.csv", sep="")
mat_com<-read.csv("C:/Users/VictorF/Documents/PracticoEI/montecarlo-comunes.csv", sep="")

#Coeficientes aleatorios
sesgo_al<-data.frame(matrix(0, ncol=22, nrow=100))
names(sesgo_al)<-c("alfa_cs1", "alfa_cs2", "alfa_cs3", "alfa_cs4", "alfa_dd1",
                   "alfa_dd3", "alfa_dd5", "alfa_iv1", "alfa_iv2", "alfa_iv3",
                   "alfa_iv4", "alfa_cs1m", "alfa_cs2m", "alfa_cs3m", "alfa_cs4m",
                   "alfa_dd1m", "alfa_dd3m", "alfa_dd5m", "alfa_iv1m", "alfa_iv2m",
                   "alfa_iv3m", "alfa_iv4m")

#Sesgo con relación a E(alpha)
for(i in 1:11){
  sesgo_al[,i]<-mat_al[,i]-mat_al[,23]
}
for(i in 1:11){
  sesgo_al[,i+11]<-mat_al[,i+27]-mat_al[,23]
}
alpha_s1<-apply(sesgo_al, 2, mean)
se_s1   <-apply(sesgo_al, 2, sd)

#Sesgo con relación a E(alpha/D=1)
for(i in 1:11){
  sesgo_al[,i]<-mat_al[,i]-mat_al[,24]
}
for(i in 1:11){
  sesgo_al[,i+11]<-mat_al[,i+27]-mat_al[,24]
}
alpha_s2<-apply(sesgo_al, 2, mean)
se_s2   <-apply(sesgo_al, 2, sd)

#Coeficientes comunes
sesgo_com<-data.frame(matrix(0, ncol=22, nrow=100))
names(sesgo_com)<-c("alfa_cs1", "alfa_cs2", "alfa_cs3", "alfa_cs4", "alfa_dd1",
                    "alfa_dd3", "alfa_dd5", "alfa_iv1", "alfa_iv2", "alfa_iv3",
                    "alfa_iv4", "alfa_cs1m", "alfa_cs2m", "alfa_cs3m", "alfa_cs4m",
                    "alfa_dd1m", "alfa_dd3m", "alfa_dd5m", "alfa_iv1m", "alfa_iv2m",
                    "alfa_iv3m", "alfa_iv4m")

#Sesgo con relación a E(alpha)
for(i in 1:11){
  sesgo_com[,i]<-mat_com[,i]-mat_com[,23]
}
for(i in 1:11){
  sesgo_com[,i+11]<-mat_com[,i+27]-mat_com[,23]
}
alpha_c1<-apply(sesgo_com, 2, mean)
se_c1   <-apply(sesgo_al, 2, sd)

#Sesgo con relación a E(alpha/D=1)
for(i in 1:11){
  sesgo_com[,i]<-mat_com[,i]-mat_com[,24]
}
for(i in 1:11){
  sesgo_com[,i+11]<-mat_com[,i+27]-mat_com[,24]
}
alpha_c2<-apply(sesgo_com, 2, mean)
se_c2   <-apply(sesgo_al, 2, sd)

T10<-data.frame(matrix(0, ncol=4, nrow=22))
T10[1,1]<-alpha_s2[1]
T10[2,1]<-se_s2[1]
T10[3,1]<-alpha_s2[2]
T10[4,1]<-se_s2[2]
T10[5,1]<-alpha_s2[3]
T10[6,1]<-se_s2[3]
T10[7,1]<-alpha_s2[4]
T10[8,1]<-se_s2[4]
T10[9,1]<-alpha_s2[5]
T10[10,1]<-se_s2[5]
T10[11,1]<-alpha_s2[6]
T10[12,1]<-se_s2[6]
T10[13,1]<-alpha_s2[7]
T10[14,1]<-se_s2[7]
T10[15,1]<-alpha_s2[8]
T10[16,1]<-se_s2[8]
T10[17,1]<-alpha_s2[9]
T10[18,1]<-se_s2[9]
T10[19,1]<-alpha_s2[10]
T10[20,1]<-se_s2[10]
T10[21,1]<-alpha_s2[11]
T10[22,1]<-se_s2[11]

T10[1,2]<-alpha_s1[1]
T10[2,2]<-se_s1[1]
T10[3,2]<-alpha_s1[2]
T10[4,2]<-se_s1[2]
T10[5,2]<-alpha_s1[3]
T10[6,2]<-se_s1[3]
T10[7,2]<-alpha_s1[4]
T10[8,2]<-se_s1[4]
T10[9,2]<-alpha_s1[5]
T10[10,2]<-se_s1[5]
T10[11,2]<-alpha_s1[6]
T10[12,2]<-se_s1[6]
T10[13,2]<-alpha_s1[7]
T10[14,2]<-se_s1[7]
T10[15,2]<-alpha_s1[8]
T10[16,2]<-se_s1[8]
T10[17,2]<-alpha_s1[9]
T10[18,2]<-se_s1[9]
T10[19,2]<-alpha_s1[10]
T10[20,2]<-se_s1[10]
T10[21,2]<-alpha_s1[11]
T10[22,2]<-se_s1[11]

T10[1,3]<-alpha_c2[1]
T10[2,3]<-se_c2[1]
T10[3,3]<-alpha_c2[2]
T10[4,3]<-se_c2[2]
T10[5,3]<-alpha_c2[3]
T10[6,3]<-se_c2[3]
T10[7,3]<-alpha_c2[4]
T10[8,3]<-se_c2[4]
T10[9,3]<-alpha_c2[5]
T10[10,3]<-se_c2[5]
T10[11,3]<-alpha_c2[6]
T10[12,3]<-se_c2[6]
T10[13,3]<-alpha_c2[7]
T10[14,3]<-se_c2[7]
T10[15,3]<-alpha_c2[8]
T10[16,3]<-se_c2[8]
T10[17,3]<-alpha_c2[9]
T10[18,3]<-se_c2[9]
T10[19,3]<-alpha_c2[10]
T10[20,3]<-se_c2[10]
T10[21,3]<-alpha_c2[11]
T10[22,3]<-se_c2[11]

T10[1,4]<-alpha_c1[12]
T10[2,4]<-se_c1[12]
T10[3,4]<-alpha_c1[13]
T10[4,4]<-se_c1[13]
T10[5,4]<-alpha_c1[14]
T10[6,4]<-se_c1[14]
T10[7,4]<-alpha_c1[15]
T10[8,4]<-se_c1[15]
T10[9,4]<-alpha_c1[16]
T10[10,4]<-se_c1[16]
T10[11,4]<-alpha_c1[17]
T10[12,4]<-se_c1[17]
T10[13,4]<-alpha_c1[18]
T10[14,4]<-se_c1[18]
T10[15,4]<-alpha_c1[19]
T10[16,4]<-se_c1[19]
T10[17,4]<-alpha_c1[20]
T10[18,4]<-se_c1[20]
T10[19,4]<-alpha_c1[21]
T10[20,4]<-se_c1[21]
T10[21,4]<-alpha_c1[22]
T10[22,4]<-se_c1[22]
T10<-round(T10, digits=2)
names(T10)<-c("E(alpha/D=1)=612,34", "E(alpha)=100,2", "E(alpha)=100", "E(alpha)=100")
rownames(T10)<-c("Cross section k+1 media", "Cross section k+1 sd", 
                 "Cross section k+2 media", "Cross section k+2 sd", 
                 "Cross section k+3 media", "Cross section k+3 sd", 
                 "Cross section k+4 media", "Cross section k+4 sd",
                 "DiD (-1,3) media", "DiD (-1,3) sd", 
                 "DiD (-3,3) media", "DiD (-3,3) sd", 
                 "DiD (-5,3) media", "DiD (-5,3) sd",
                 "VI k+1 media", "VI k+1 sd", "VI k+2 media", "VI k+2 sd", 
                 "VI k+3 media", "VI k+3 sd", "VI k+4 media", "VI k+4 sd")

mean(mat_al$corDZ)
mean(mat_al$p)

mean(mat_com$corDZ)
mean(mat_com$p)


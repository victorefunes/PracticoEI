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

sesgo<-rep(0,11)
for(i in 1:11){
  sesgo[i]<-means[i]-means[23]
}
sesgo

sesgo_Tr<-rep(0,11)
for(i in 1:11){
  sesgo_Tr[i]<-means[i]-means[24]
}
sesgo_Tr


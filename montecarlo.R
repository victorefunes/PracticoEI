mat1<-data.frame(matrix(0, ncol=27, nrow=100))
names(mat1)<-c("alfa_cs1", "alfa_cs2", "alfa_cs3", "alfa_cs4", "alfa_dd1",
               "alfa_dd3", "alfa_dd5","alfa_iv1","alfa_iv2", "alfa_iv3", 
               "alfa_iv4", "se_cs1", "se_cs2", "se_cs3", "se_cs4","se_dd1",
               "se_dd3", "se_dd", "se_iv1", "se_iv2", "se_iv3", "se_iv4",
               "mean_alpha_i", "mean_alpha_i_Tr", "mu", "corDZ", "p")

for(w in 1:100){
  source('C:/Users/VictorF/Documents/PracticoEI/Practico.R', local=TRUE)
  print(paste("Simulaci?n: ", w, ""))
  mat1[w,1]<-alfa_cs1
  mat1[w,2]<-alfa_cs2
  mat1[w,3]<-alfa_cs3
  mat1[w,4]<-alfa_cs4
  mat1[w,5]<-alfa_dd1
  mat1[w,6]<-alfa_dd3
  mat1[w,7]<-alfa_dd5
  mat1[w,8]<-alfa_iv1
  mat1[w,9]<-alfa_iv2
  mat1[w,10]<-alfa_iv3
  mat1[w,11]<-alfa_iv4
  mat1[w,12]<-se_cs1
  mat1[w,13]<-se_cs2
  mat1[w,14]<-se_cs3
  mat1[w,15]<-se_cs4
  mat1[w,16]<-se_dd1
  mat1[w,17]<-se_dd3
  mat1[w,18]<-se_dd5
  mat1[w,19]<-se_iv1
  mat1[w,20]<-se_iv2
  mat1[w,21]<-se_iv3
  mat1[w,22]<-se_iv4
  mat1[w,23]<-mean_alpha_i
  mat1[w,24]<-mean_alpha_i_Tr
  mat1[w,25]<-mu
  mat1[w,26]<-corDZ
  mat1[w,27]<-p
}

mat2<-data.frame(matrix(0, ncol=22, nrow=100))
names(mat2)<-c("alfa_cs1m", "alfa_cs2m", "alfa_cs3m", "alfa_cs4m", "alfa_dd1m",
               "alfa_dd3m", "alfa_dd5m", "alfa_iv1m", "alfa_iv2m", "alfa_iv3m",
               "alfa_iv4m", "se_cs1m", "se_cs2m", "se_cs3m", "se_cs4m", "se_dd1m",
               "se_dd3m", "se_dd5m", "se_iv1m", "se_iv2m", "se_iv3m", "se_iv4m")

for(w in 1:100){
  source('C:/Users/VictorF/Documents/PracticoEI/Practico.R', local=TRUE)
  print(paste("Simulaci?n: ", w, ""))
  mat2[w,1]<-alfa_cs1m
  mat2[w,2]<-alfa_cs2m
  mat2[w,3]<-alfa_cs3m
  mat2[w,4]<-alfa_cs4m
  mat2[w,5]<-alfa_dd1m
  mat2[w,6]<-alfa_dd3m
  mat2[w,7]<-alfa_dd5m
  mat2[w,8]<-alfa_iv1m
  mat2[w,9]<-alfa_iv2m
  mat2[w,10]<-alfa_iv3m
  mat2[w,11]<-alfa_iv4m
  mat2[w,12]<-se_cs1m
  mat2[w,13]<-se_cs2m
  mat2[w,14]<-se_cs3m
  mat2[w,15]<-se_cs4m
  mat2[w,16]<-se_dd1m
  mat2[w,17]<-se_dd3m
  mat2[w,18]<-se_dd5m
  mat2[w,19]<-se_iv1m
  mat2[w,20]<-se_iv2m
  mat2[w,21]<-se_iv3m
  mat2[w,22]<-se_iv4m
}

means1<-apply(mat1, 2, mean)
sds1<-apply(mat1, 2, sd)

means2<-apply(mat2, 2, mean)
sds2<-apply(mat2, 2, sd)

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

"alfa_cs1m", "alfa_cs2m", "alfa_cs3m", 
"alfa_cs4m", "alfa_dd1m", "alfa_dd3m", "alfa_dd5m",
"alfa_iv1m", "alfa_iv2m", "alfa_iv3m", "alfa_iv4m",

"se_cs1m", "se_cs2m", "se_cs3m", "se_cs4m", "se_dd1m",
"se_dd3m", "se_dd5m", "se_iv1m", "se_iv2m", "se_iv3m", "se_iv4m",
)
  
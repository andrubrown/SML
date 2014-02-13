#############
#%: Demo R for Big Memory
#%: Instuctions: Just run this in blocks. Be sure to understand this demo line-by-line.
#%: Written by: Kevin Lin
#%: NOTE: You might have trouble running bigmemory on your (new) R version. If so, install R-2.15.3
##########
setwd("C:/Users/UikosPC/Dropbox/GitHub/SML")
library(bigmemory)
library(biganalytics)
library(biglm)
#%: set the option to name columns and load the data in
options(bigmemory.allow.dimnames=TRUE)
df.mat = read.big.matrix("DemoData_BigMemory.csv",type="double")
colnames(df.mat) = c("col1","col2","col3","col4","col5")
df.mat2 = as.matrix(df.mat)

####################################
#%: we'll just run some simple functions on df.mat to let you know how to manipulate a big.matrix object.
head(df.mat)
summary(df.mat)
max(df.mat)
colrange(df.mat)
tmp = df.mat[,1]
mean(tmp)

######################################
#%: let's see how the mwhich function is faster than the which function
vec.sorted = sort(df.mat2[,1])

for(i in 1:100){
  which(df.mat2[,1]<=vec.sorted[i])
  if(i%%10==0) cat('*')
}

for(i in 1:100){
  mwhich(df.mat,1,vec.sorted[i],"le")
  if(i%%10==0)cat('*')
}

#%: now we see how to automatically save our results
idx_list = list()
for(i in 1:100){
  idx_list[[i]] = mwhich(df.mat,1,vec.sorted[i],"le")
  if(i%%10==0) {
    save(idx_list,file="idx_list.RData")
    cat('*')
  }
}

#%: or we can just used the sorted list to be even faster
idx_list2 = list()
vec.orderidx = order(df.mat[,1])
for(i in 1:100){
  idx_list2[[i]] = vec.orderidx[1:i]
}
sort(idx_list[[10]])
sort(idx_list2[[10]])

#%: we can do compare mwhich and which more scientifically by using the function system.time
#%: note we can run mwhich on normal matrices as well
system.time(for(i in 1:100){
  which(df.mat2<=vec.sorted[i])
})
system.time(for(i in 1:100){
  mwhich(df.mat,1,vec.sorted[i],"le")
})
system.time(for(i in 1:100){
  mwhich(df.mat2,1,vec.sorted[i],"le")
})

dummyfunc3<-function(i){
 mwhich(df.mat,1,i,"le") 
}
system.time(sapply(df.mat[1:100,1],dummyfunc3))

dummyfunc1<-function(){
  idx_list = list()
  for(i in 1:100){
    idx_list[[i]] = mwhich(df.mat,1,vec.sorted[i],"le")
  }
}

dummyfunc2<-function(){
  idx_list2 = list()
  vec.orderidx = order(df.mat[,1])
  for(i in 1:100){
    idx_list2[[i]] = vec.orderidx[1:i]
  }
} 
system.time(dummyfunc1())
system.time(dummyfunc2())

#####################################
#%: now let's try running a linear regression. 
##%: we use biglm.big.matrix when we do a linear regression on a big.matrix
##%: we use lm when we do linear regression on a data frame
biglm.res = biglm.big.matrix(col1 ~ col2 + col3 + col4 + col5, data=df.mat)
vec.coef = coef(biglm.res)
vec.predict = df.mat[,2:5]%*%vec.coef[2:5] + vec.coef[1]
num.dif = sum(abs(vec.predict-df.mat[,1]))

#%: we can also compare the difference in time between biglm and lm
df.mat3 = data.frame(df.mat2)
system.time(biglm.big.matrix(col1 ~ col2 + col3 + col4 + col5, data=df.mat))
system.time(lm(col1 ~ col2 + col3 + col4 + col5, data=df.mat3))

#################################
#%: try the load function
rm(list=ls())
load("idx_list.RData")


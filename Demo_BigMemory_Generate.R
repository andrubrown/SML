setwd("C:/Users/UikosPC/Dropbox/GitHub/SML")
NUM = 1000000
COL = 5
MEAN = c(1,5,6,2,3)
set.seed(1)
x = matrix(rnorm(NUM*COL,MEAN,1),ncol=COL,nrow=NUM,byrow=TRUE)
x = round(x,digits=2)
write.table(x,file="DemoData_BigMemory.csv",sep=",",row.names=FALSE,col.names=FALSE)
load_mnist <- function() {
  load_image_file <- function(filename) {
    ret = list()
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    ret$n = readBin(f,'integer',n=1,size=4,endian='big')
    nrow = readBin(f,'integer',n=1,size=4,endian='big')
    ncol = readBin(f,'integer',n=1,size=4,endian='big')
    x = readBin(f,'integer',n=ret$n*nrow*ncol,size=1,signed=F)
    ret$x = matrix(x, ncol=nrow*ncol, byrow=T)
    close(f)
    ret
  }
  load_label_file <- function(filename) {
    f = file(filename,'rb')
    readBin(f,'integer',n=1,size=4,endian='big')
    n = readBin(f,'integer',n=1,size=4,endian='big')
    y = readBin(f,'integer',n=n,size=1,signed=F)
    close(f)
    y
  }
  train <<- load_image_file('train-images.idx3-ubyte')
  test <<- load_image_file('t10k-images.idx3-ubyte')
  
  train$y <<- load_label_file('train-labels.idx1-ubyte')
  test$y <<- load_label_file('t10k-labels.idx1-ubyte')  
}


show_digit <- function(arr784, col=gray(12:1/12), ...) {
  image(matrix(arr784, nrow=28)[,28:1], col=col, ...)
}

show_digitsmall  <- function(arr196, col=gray(12:1/12), ...) {
  image(matrix(arr196, nrow=14)[,14:1], col=col, ...)
}


###############################################################
#load in the data and look at it
setwd("C:/Users/UikosPC/Dropbox/GitHub/SML")
load_mnist()
names(train)

train.num = train$n
train.images = train$x
train.labels = train$y
dim(train.images)

head(train.labels)
png("exampledigit.png",width=200,height=200,units="px")
show_digit(train.images[1,],asp=TRUE)
dev.off()

tmp = train.images[1,]
tmp[1:14] = 255
png("exampledigit2.png",width=200,height=200,units="px")
show_digit(tmp,asp=TRUE)
dev.off()

######################
#compress our images
compressImg <- function(full){
  compressFour <- function(j){
    pixelvec = rep(NA,4)
    pixelvec[1] = full[2*j-1+floor((j-1)/14)*28];
    pixelvec[2] = full[2*j+floor((j-1)/14)*28];
    pixelvec[3] = full[2*j-1+28+floor((j-1)/14)*28];
    pixelvec[4] = full[2*j+28+floor((j-1)/14)*28];
    return(mean(pixelvec))
  }
  
  compress = unlist(lapply(1:196,compressFour))
  return(compress)
}


compress.train.images = matrix(NA,nrow=dim(train.images)[1],ncol=14*14)
for(i in 1:10){
  compress.train.images[((i-1)*6000+1):(i*6000),] = t(apply(train.images[((i-1)*6000+1):(i*6000),],1,compressImg))
  cat('*')
  save(compress.train.images,file="compress.train.images.RData")
}

####################################
#display as a table
plotTable <- function(numCol,vec.labels,mat.images){
  vec.uniq = unique(vec.labels)
  par(mfrow=c(length(vec.uniq),numCol),pty="s",mar = c(0.1,0.1,0.1,0.1))
  for(i in 1:length(vec.uniq)){
    tmpidx = which(vec.labels==vec.uniq[i])
    for(j in 1:numCol){
      show_digitsmall(mat.images[tmpidx[j],],asp=TRUE)
    }
  }
}

png("exampletable.png",width=1200,height=800,units="px")
plotTable(20,train.labels, compress.train.images)
dev.off()

######################################################
#implement the log functions
log.sum <- function(v) {
  log.sum.pair <- function(x,y)
  {
    if ((y == -Inf) && (x == -Inf))
    { return(-Inf); }
    if (y < x) return(x+log1p(exp(y-x)))
    else return(y+log1p(exp(x-y)));
  }
  
  r <- v[1];
  for (i in 2:length(v))
    r <- log.sum.pair(r, v[i]);
  return(r);
}

calc.gamma <- function(mat.images){
  inv.sigma = list(0)  
  tmpvalues = rep(NA,num.class)
  
  for(j in 1:num.class){
    inv.sigma[[j]] = solve(sigma[[j]])
    tmpvalues[j] = log(eta[j]) - (14*14)/2*log(2*pi) - .5*log(det(sigma[[j]])) 
  }
  
  calc.gamma_indiv <- function(i){
    tmpvec = rep(NA,num.class)
    for(j in 1:num.class){
      tmpvec[j] = tmpvalues[j] - .5*(mat.images[i,]-mu[j,])%*%inv.sigma[[j]]%*%(mat.images[i,]-mu[j,])
    }
    
    return(tmpvec)
  }
  
  tmpmat.log = t(sapply(1:dim(mat.images)[1],calc.gamma_indiv))  
  tmpvec.logsum = apply(tmpmat.log,1,log.sum)
  
  for(j in 1:num.class){
    tmpmat.log[,j] = tmpmat.log[,j] - tmpvec.logsum
  }
  
  tmpmat.log = exp(tmpmat.log)
  
  return(list(gamma = tmpmat.log, obj=sum(tmpvec.logsum)))
}

calc.Sigma <- function(j,mat.images){
  tmpmat = matrix(0,ncol=14*14,nrow=14*14)
  for(i in 1:dim(mat.images)[1]){
    tmpmat = tmpmat + gamma[i,j]*(mat.images[i,]-mu[j,])%*%t(mat.images[i,]-mu[j,])
  }
  tmpmat = tmpmat + diag(14*14)
  tmpmat = tmpmat/sum(gamma[,j])
  return(tmpmat)
}

#################
#compute confusion matrix and the predictions based on largest gamma for each image
calc.accuracy <- function(){
  vec.pred = apply(gamma,1,which.max)
  mat.confusion = matrix(0,ncol=num.class,nrow=num.class)
  vec.truth = labels123
  vec.uniq = unique(labels123)
  for(i in 1:length(vec.uniq)){
    tmpidx = which(labels123==vec.uniq[i])
    vec.truth[tmpidx] = i
  }
  
  for(i in 1:length(labels123)){
    mat.confusion[vec.pred[i],vec.truth[i]] = mat.confusion[vec.pred[i],vec.truth[i]] + 1
  }
  
  return(list(mat=mat.confusion,pred=vec.pred))
}



#######################################################3

#EM algorithm
#take all images associated with 1,2,3
set.seed(1)
tmpidx = c(which(train.labels==1),which(train.labels==2),which(train.labels==3))
tmpidx = sample(tmpidx)[1:1000]
labels123 = train.labels[tmpidx]
images123 = compress.train.images[tmpidx,]
num.class = 3

#initialize the parameters
eta = rep(1/num.class,num.class)
mu = matrix(NA,ncol=14*14,nrow=num.class)
sigma = list(0)
for(j in 1:num.class){
  mu[j,] = images123[j,]
  sigma[[j]] = 25*diag(14*14)
}
gamma = matrix(0,ncol=num.class,nrow=length(labels123))

num.EPS = 0.0001
num.iter = 1
num.MAXITER = 100
vec.obj = rep(NA,num.MAXITER)

while(TRUE){
  #E-step. calculate the gammas
  #calculate the loglikelihood while we calculate the gammas
  eres = calc.gamma(images123)
  vec.obj[num.iter] = eres$obj
  gamma = eres$gamma
  print(paste("Obj: ",eres$obj))
  if(num.iter > 1 && vec.obj[num.iter]-vec.obj[num.iter-1] < num.EPS) break()
  
  #M-step. calculate the parameters
  for(j in 1:num.class){
    eta[j] = 1/length(labels123)*sum(gamma[,j])
    mu[j,] = t(gamma[,j]%*%images123)/sum(gamma[,j])
    sigma[[j]] = calc.Sigma(j,images123)
  }
  
  res = calc.accuracy()
  print(res$mat)
  
  num.iter = num.iter+1
  if(num.iter > num.MAXITER) break()
  cat('*')
}

png("em_1.png",height=300,width=900,units="px")
par(mfrow=c(1,3))
for(i in 1:3){
  show_digitsmall(mu[i,],asp=TRUE)
}
dev.off()


##############################
#different initializations
#one initialization method (random index)
set.seed(1)
eta = rep(1/num.class,num.class)
mu = matrix(NA,ncol=14*14,nrow=num.class)
sigma = list(0)
for(j in 1:num.class){
  mu[j,] = images123[floor(runif(1,max=length(labels123))),]
  sigma[[j]] = 25*diag(14*14)
}
gamma = matrix(0,ncol=num.class,nrow=length(labels123))


#another initialization method (averaging)
vec.diag = rep(NA,14*14)
for(i in 1:(14*14)){
  vec.diag[i] = sd(images123[,i])
}
vec.diag = vec.diag^2/4
vec.diag = vec.diag + 0.05
mat.var = matrix(0,ncol=14*14,nrow=14*14)
diag(mat.var) = vec.diag


int.third = floor(length(labels123)/3)
mu = matrix(NA,ncol=14*14,nrow=num.class)
for(i in 1:3){
  mu[i,] = apply(images123[(1+(i-1)*int.third):(i*int.third),],2,mean)
}

eta = rep(1/num.class,num.class)
sigma = list(0)
for(j in 1:num.class){
  sigma[[j]] = mat.var
}
gamma = matrix(0,ncol=num.class,nrow=length(labels123))


###########################################################
res = calc.accuracy()
vec.pred = res$pred
png("resulttable.png",height=300,width=1200,units="px")
plotTable(20,vec.pred,images123)
dev.off()





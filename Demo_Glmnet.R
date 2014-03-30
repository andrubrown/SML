setwd("C:/Users/UikosPC/Dropbox/GitHub/SML")
library(survival)
library(glmnet)
data(cancer)
head(cancer)

cancer2 = cancer
#remove NA
tmp = apply(is.na(cancer2),1,sum)
cancer2 = cancer2[which(tmp==0),]
#regularize numerical data
numElem = c(2,4,6:10)
for(i in 1:length(numElem)){
  cancer2[,numElem[i]] = (cancer2[,numElem[i]]-mean(cancer2[,numElem[i]]))/sd(cancer2[,numElem[i]])
}

smallTrain = cancer2[1:150,]
smallTest = cancer2[151:167,]

# tmp = apply(is.na(smallTrain),1,sum)
# smallTrain = smallTrain[which(tmp==0),]
# tmp = apply(is.na(smallTest),1,sum)
# smallTest = smallTest[which(tmp==0),]

train.x = smallTrain[,-c(1:2)]
train.y = smallTrain[,2]
test.x = smallTest[,-c(1:2)]
test.y = smallTest[,2]

train.x = data.matrix(train.x)
train.y = as.numeric(train.y)
test.x = data.matrix(test.x)
test.y = as.numeric(test.y)

dataset = rbind(train.x,test.x)
len = dim(train.x)[1]

#Convert all the categorical features into several indicator features
#Add the appropriate column labels to the final data matrix
name = colnames(dataset)
catElem = c(1,3)
for (i in 1:length(catElem)){
  tmp = dataset[,catElem[i]]
  elemUniq = unique(tmp)
  for (j in 1:length(elemUniq)){
    indic = rep(0,dim(dataset)[1])
    indic[tmp==elemUniq[j]]=1
    
    tmpName = paste(name[catElem[i]],elemUniq[j],sep="")
    name = c(name,tmpName)
    
    dataset = cbind(dataset,indic)
  }
}
dataset=dataset[,-catElem]
name=name[-catElem]
colnames(dataset)=name
train.x = dataset[1:len,]
test.x = dataset[(len+1):nrow(dataset),]

train = data.frame(train.x,train.y)
test = data.frame(test.x,test.y)

res = glmnet(x=train.x,y=train.y,family="gaussian",alpha=1)
plot(res,label=TRUE,lwd=2,cex=2,main="Regularization Path for Lasso")

###################################################

#Run cross validation
cv.model = cv.glmnet(x=train.x,y=train.y,family="gaussian",alpha=1,nfolds=5)
plot(cv.model,main="Cross Validation Plot")

#we now want to see what value of lambda minimzed our cross-validation error
#actually, we use the largest lambda that is within 1 standard devation from
#  the smallest lambda.
lambda = cv.model$lambda.min
#this is model we picked that (roughly) minimized the error via cross validation on our training set
glmnet.coeff = coef(cv.model,s="lambda.min")

#to visualize this on the graph, we first find the L1-norm of these coefficients
l1norm = sum(abs(glmnet.coeff[-1]))

png("lassowithlambda.png",height=600,width=600,units="px")
plot(res,label=TRUE,lwd=2,cex=2,main="Regularization Path for Lasso")
liney = c(-1000:1000)
linex = rep(l1norm,length(liney))
lines(x=linex,y=liney,col="red",lty=4,lwd=3)
dev.off()


preds= predict(cv.model, test.x, type="response",s = "lambda.min")
test.error = (mean(abs(preds - test.y)))

#####################################

train2 = train
#compare the the normal MLE  regression we learned before. 
#We skip the cross-validation here for no reason in particular
mle.model = lm(train.y ~ ., data = train2)
#this is the coefficients we would've gotten using the normal MLE model
mle.coeff = mle.model$coefficients
# compute test error with the MLE
# We get warnings when running this but that's alright (because some of our coefficients are NA)
# this happened since most of our columns are indicators
# http://stats.stackexchange.com/questions/25804/why-would-r-return-na-as-a-lm-coefficient
mle.test.error = mean(abs(predict(mle.model, test ,type="response") - test.y))

######################################
#######################################

cancer2 = cancer
#remove NA
tmp = apply(is.na(cancer2),1,sum)
cancer2 = cancer2[which(tmp==0),]

train.x = cancer2[,c("pat.karno","wt.loss")]
train.y = cancer2[,2]
train.x = data.matrix(train.x)

train.x[,1] = (train.x[,1]-mean(train.x[,1]))/sd(train.x[,1])
train.x[,2] = (train.x[,2]-mean(train.x[,2]))/sd(train.x[,2])
train.y = (train.y-mean(train.y))/sd(train.y)

#calculate the mle coefficients
train = data.frame(cbind(train.x,train.y))
mle.model = lm(train.y ~ ., data = train)
summary(mle.model)
mlecoef = coef(mle.model)[2:3]
targetl1norm = sum(abs(mlecoef))/3

res = glmnet(x=train.x,y=train.y,family="gaussian",alpha=1)
#plot(res)
#coef(res)
#res$lambda

#find the corresponding lasso coefficients there is only one coefficient
lambda = res$lambda[3]
beta = as.matrix(res$beta)
l1normall = apply(abs(beta),2,sum)
idx = max(which(l1normall<=targetl1norm))
# tmp = res$df
# idx = max(which(tmp==1))
l1norm = sum(abs(beta[,idx]))
lassocoef = beta[,idx]

beta1 = seq(-.1,.2,length.out=100)
beta2 = seq(-.1,.2,length.out=100)

#http://stackoverflow.com/questions/15601359/an-error-in-r-when-i-try-to-apply-outer-function
MSEfunc <- function(x,y){ 
  return(apply((matrix(rep(train.y,times=length(x)),
                       ncol=length(train.y),nrow=length(x),
                       byrow=TRUE)
                -(x%*%t(train.x[,1])+y%*%t(train.x[,2])))^2,1,sum))
}
z = outer(beta1,beta2,MSEfunc)
maxz = max(z)
minz = min(z)
tmp = seq(minz,maxz,length.out=386)
seqz = tmp[c(2,6,15,31,56,92,141,205,286,386)]
#to make our plot look nicer, find the level corresponding to our lasso curve
tmpidxx = which(abs(beta1-lassocoef[1])==min(abs(beta1-lassocoef[1])))[1]
tmpidxy = which(abs(beta2-lassocoef[2])==min(abs(beta2-lassocoef[2])))[1]
tmpz = z[tmpidxx,tmpidxy]
tmpidx = which(abs(seqz-tmpz) == min(abs(seqz-tmpz)))[1]
seqz[tmpidx] = tmpz
vec.col=rep("black",length(seqz))
vec.col[tmpidx]="red"
vec.lwd=rep(1,length(seqz))
vec.lwd[tmpidx]=2

png("demo_lasso_geom.png",width=600,height=600,units="px")
contour(beta1,beta2,z,levels=seqz,lwd=vec.lwd,col=vec.col,
        main="Contour Plot for Cancer Data, Lasso Regression",
        xlab="Coefficent Value for Physican's Karnofsky Perf. Score", 
        ylab="Coefficnet Value for Weight Loss",asp=1,
        labcex=1)

#add our other stuff
lines(x=c(-100,100),y=c(0,0),lwd=3)
lines(x=c(0,0),y=c(-100,100),lwd=3)
points(mlecoef[1],mlecoef[2],pch=16,col="red",cex=2)
text(mlecoef[1],mlecoef[2]+0.01,"MLE Coef")
points(lassocoef[1],lassocoef[2],pch=16,col="blue",cex=2)
text(lassocoef[1]+.01,lassocoef[2]+.01,"Lasso Coef")
polyx = c(l1norm,0,-l1norm,0)
polyy = c(0,l1norm,0,-l1norm)
polygon(polyx,polyy,col=rgb(0.2,0.2,0.2,0.5),border="blue",lwd=2)
dev.off()


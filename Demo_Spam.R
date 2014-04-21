setwd("C:/Users/Uikos/Dropbox/GitHub/SML")
top = "C:/Users/Uikos/Dropbox/GitHub/SML"
Directories = c("easy_ham","spam")
dirs = paste(top, Directories, sep ="/")

source("readRawEmail.R")
mail = readAllMessages(dirs = dirs)


#############################################
list_textBody = numeric(0)
vec.spam = rep(NA,length(mail))
for(i in 1:length(mail)){
  tmpmail = mail[[i]]
  tmp = tmpmail$body
  tmp2 = paste(tmp$text,collapse="")
  tmp3 = gsub("\\b([[:punct:]|[:digit:]])*[a-zA-Z]*([[:punct:]|[:digit:]])+[a-zA-Z]*
              ([[:punct:]|[:digit:]])*"," ",tmp2)
  tmp4 = gsub("[^A-Za-z]"," ",tmp3)
  list_textBody[[i]] = tmp4
  vec.spam[i] = tmpmail$spam
}
numHam = sum(!vec.spam)

library(tm)

corp_textBody <- Corpus(VectorSource(list_textBody))
cntr <- list(removePunctuation = TRUE, stemming = TRUE, wordLengths = c(3, 20))

res = as.matrix(TermDocumentMatrix(corp_textBody, control = cntr))
res2 = res>0


###############################################
#split data into testing and training
set.seed(1)
testingidx = sample(1:ncol(res),100)
trainingidx = 1:ncol(res)
trainingidx = trainingidx[-testingidx]

#compute sufficient statistics
#for each word: indicator how many documents use this word, how many spam documents use this word
computeSufficient &lt;- function(vec){
  spamIndic = sum(vec[trainingidx]%*%vec.spam[trainingidx])
  totalIndic = sum(vec[trainingidx])
  return(c(spamIndic,totalIndic))
}
mat.suffStat = apply(res2,1,computeSufficient)
mat.suffStat = t(mat.suffStat)
numSpam = sum(vec.spam)

#from the sufficient statistics, calculate the estimated parameters, 2 for each word, each type
n1 = numSpam
n0 = dim(res2)[2]-numSpam
vec.wplus = mat.suffStat[,1]
vec.w2plus = mat.suffStat[,2]-mat.suffStat[,1]

vec.theta = vec.wplus/n1
vec.theta2 = vec.w2plus/n0
eta = n1/(n1+n0)

#prediction
vec.truth = vec.spam[testingidx]
vec.pred = rep(NA,length(vec.truth))
eps = 0.000001
vec.theta[vec.theta<eps] = eps
vec.theta[vec.theta>(1-eps)] = 1-eps
vec.theta2[vec.theta2<eps] = eps
vec.theta2[vec.theta2>(1-eps)] = 1-eps
for(i in 1:length(vec.truth)){
  tmp = res2[testingidx[i],]
  logprobspam = log(eta)+res2[,testingidx[i]]%*%log(vec.theta)+(1-res2[,testingidx[i]])%*%log(1-vec.theta)
  logprobham = log(1-eta)+res2[,testingidx[i]]%*%log(vec.theta2)+(1-res2[,testingidx[i]])%*%log(1-vec.theta2)
  if(logprobspam>logprobham){
    vec.pred[i] = 1
  } else {
    vec.pred[i] = 0
  }
}
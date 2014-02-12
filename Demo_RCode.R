#############
#%: Demo R Code
#%: Note: The plots created by this demo might be meaningless. This is simply a demo to
##%: demostrate basic functions of R.
#%: Instuctions: Just run this in blocks. Be sure to understand this demo line-by-line.
#%: Written by: Kevin Lin
##########
#%: set working directory and load in the data
#%: more info about the data can be found on http://vincentarelbundock.github.io/Rdatasets/doc/COUNT/lbw.html
setwd("C:/Users/UikosPC/Dropbox/GitHub/SML")
DATA = read.csv("lbw.csv",header=TRUE,sep=",")
?read.csv #check how to use this function
DATA = read.csv(url("http://vincentarelbundock.github.io/Rdatasets/csv/COUNT/lbw.csv"),header=TRUE,sep=",") #you can also just read it straight from the internet

#%: do some simple assessment of the data
head(DATA) #view data
dim(DATA) #check the dimension of the data
colnames(DATA) #see the column names of the data
unique(DATA[,5])[1:10] #check 10 unique ages. The next line does the same exact thing
unique(DATA$age)[1:10]
max(DATA[,11]) #see the maximum of column 11 of the dataset
summary(DATA) #see the entire summary of the dataset
sum(is.na(DATA)) #see how many missing values there are
sort(DATA$age) #a function for sorting
tmp = order(DATA$age) #a function for returning the index for sorting
df.data2 = DATA[tmp,] #if we wanted to sort the data by age
DATA[sample(1:dim(DATA)[1],10),] #display 10 randomly sampled rows of the data

#################
#%: construct a histogram of "last weight", one for each race We present two ways to do this.
#%: The first way is the "brute-force" way. 
tmpwhite = which(DATA$race == 1)
hist(DATA$lwt[tmpwhite],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (White)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")
tmpblack = which(DATA$race == 2)
hist(DATA$lwt[tmpblack],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (Black)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")
tmpother = which(DATA$race == 3)
hist(DATA$lwt[tmpother],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (Other)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")

#%: equivalently, we could do this
hist(DATA$lwt[DATA$race==1],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (White)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")


#%: We can plot all 3 graphs in one window
par(mfrow=c(3,1)) #this allows us to plot 3 graphs in a column. this changes the R environment setting
hist(DATA$lwt[DATA$race==1],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (White)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")
hist(DATA$lwt[DATA$race==2],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (Black)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")
hist(DATA$lwt[DATA$race==3],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (Other)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")
par(mfrow=c(1,1)) #remember to always reset this after you're done.
#%: Note: if you get the error "figure margins too large", you need to resize your Plots window (make it bigger)

#%: We can save this graph automatically to our harddisk also.
#%: this will avoid any problems with figure margins automatically.
png("hist.png",height=600,width=600,units="px")
par(mfrow=c(3,1)) #this allows us to plot 3 graphs in a column. this changes the R environment setting
hist(DATA$lwt[DATA$race==1],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (White)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")
hist(DATA$lwt[DATA$race==2],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (Black)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")
hist(DATA$lwt[DATA$race==3],breaks=20,freq=F,main="Histogram of Weight in the Last Menstrual Period (Other)",xlab="Weight in the Last Menstrual Period (lbs)",ylab="Density")
par(mfrow=c(1,1)) #remember to always reset this after you're done.
dev.off()

#%: We can also stack all histograms ontop of each other. We set a transparency to the colors.
vec.raceNames = c("White","Black","Other")
hist(DATA$lwt[DATA$race==1],breaks=20,freq=F,ylim=c(0,.03),xlim=c(80,250),col = rgb(1,0,0,.8), main="Histogram of Weight in the Last Menstrual Period (lbs)",xlab="Weight in the Last Menstrual Period",ylab="Density")
hist(DATA$lwt[DATA$race==2],breaks=20,freq=F,add=T,col = rgb(0,1,0,.6))
hist(DATA$lwt[DATA$race==3],breaks=20,freq=F,add=T,col = rgb(0,0,1,.4))
legend("topright", vec.raceNames, cex=0.6, bty="n", fill=vec.color)


#%: Let's automate this thing some more. Here's what I personally would've done to get that graph.
#%: It might take you a while to understand this code. For the purposes of Homework 1, you don't
##%: do NOT need to understand this code, but this shows how to automate things nicely.
#%: The important part is to realize what functions R has.
vec.raceNames = c("White","Black","Other")
vec.lwt_list = rep(NA,3)
for(i in 1:3){
  vec.lwt_list[i] = paste("vec.lwt_",i,sep="")
  assign(vec.lwt_list[i],DATA$lwt[DATA$race==i])
}
png("hist2.png",height=600,width=600,units="px")
plot(NA,xlim=c(min(DATA$lwt),max(DATA$lwt)),ylim=c(0,.03),main="Histogram of Weight in the Last Menstrual Period By Race",xlab="Weight in Last Menstrual Period (lbs)",ylab="Density")
vec.color = c(rgb(1,0,0,.8),rgb(0,1,0,.6),rgb(0,0,1,.4))
for(i in 1:3){
  hist(get(vec.lwt_list[i]),col=vec.color[i],freq=F,breaks=20,add=T,lty=i)
}
legend("topright", vec.raceNames, cex=0.6, bty="n", fill=vec.color)
dev.off()

#################

#%: Do a linear regression where we regression all the numeric (quantitative) columns 
##%: without missing values onto the Total Reciepts column
#%: Note: This linear regression is a bit meaningless, but this is just a demo.
lm.res = lm(lwt ~ bwt + ftv + age, data = DATA)
names(lm.res) #view what's stored in the linear regression results
summary(lm.res) #view the linear regression results
lm.coef = coef(lm.res) #how to extract the coefficients

#%: we show how to do matrix multiplication here
tmpmat = as.matrix(DATA[,c(11,10,5)]) #convert data frame to matrix
vec.pred = lm.coef[1]+tmpmat%*%lm.coef[2:4] #perform matrix multiplication
vec.res = DATA$lwt - vec.pred

#%: plot the predicted lwt's against the observed lwt's
##%: to be fancy, let the size of each dot represent another dimension
maxx = max(DATA$bwt)
minx = min(DATA$bwt)
maxy = max(DATA$lwt)
miny = min(DATA$lwt)
tmpsize = (DATA$ftv+1)/1.25
png("plot.png",height=600,width=600,units="px")
plot(NA,xlim=c(minx,maxx),ylim=c(miny,maxy),main="Plot of Actual LWT against Predicted LWT\n with BWT and FTV shown",ylab="LWT (lbs)",xlab="Age (Years)")
points(x=DATA$bwt,y=DATA$lwt,col=rgb(0,0,0,.75),pch=20,cex=tmpsize)
points(x=DATA$bwt,y=vec.pred,col=rgb(1,0,0,.75),pch=20,cex=tmpsize)
legend("topleft", c("Actual","Predicted"), cex=1.2, bty="n", fill=c("black","red"))
dev.off()

#%: check to see that our calculated residuals match those found using
##%: the lm function
tmpres = unlist(lm.res[2]) #converting a list into a vector
tmp = sum(abs(vec.res)) - sum(abs(tmpres))

####################
#%: Lastly, we show how to define a function and how to use the `apply' function
##%: this example has no meaning at all, we are simply showing how this works
randomfunc <- function(idx){
  if(DATA$race[idx]==1){
    return(DATA$lwt[idx]+DATA$bwt[idx])
  } else{
    return(DATA$bwt[idx])
  }
}
tmplist = lapply(DATA$X,randomfunc) #apply the "randomfunc" function to the entire data set
tmp = unlist(tmplist) #unlist the results

#%: another useful function is "apply"
tmp = apply(DATA,1,sum) #for no good reason at all, sum all the numbers in each row
?apply #be sure to understand how apply works

#######################
#%: Dummy examples to show what can go wrong in R
dummyfunc1 <- function(){
  for(j in 1:5){
    i = i+1
    print(i)
  }
  print(j)
}

i = 0
dummyfunc1()
i
#j
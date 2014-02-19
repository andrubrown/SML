###########################
# Author: Kevin Lin
# Title: Time and String Tutorial
# Purpose: Demonstrate POSIX and simple string manipulation
##############################

setwd("C:/Users/UikosPC/Dropbox/GitHub/SML")
DATA = read.csv("USDCNH_Ticks_10.02.2014-10.02.2014.csv")
head(DATA)

#%: explore what factor types are
tmptime = DATA[,1]
tmptime[1]
levels(tmptime)[1:10]
c(tmptime[1],tmptime[2]) #notice this gives you integers
c(as.character(tmptime[1]),as.character(tmptime[2])) #notice this gives you strings

#####################
#%: convert all the date and times into POSIX objects
strsplit(as.character(tmptime[1]),split=".")
strsplit(as.character(tmptime[1]),split="\\.")
strsplit(as.character(tmptime[1]),split="0")
strsplit(as.character(tmptime[1]),split="0+")
strsplit(as.character(tmptime[1]),split="")
strsplit(as.character(tmptime[1]),split="[[:punct:]]")
strsplit(as.character(tmptime[1]),split="[[:punct:][:space:]]")
splitfunc <- function(factorvar){
  return(strsplit(as.character(factorvar),split="[[:punct:][:space:]]")[[1]][1:6])
}
tmp = unlist(lapply(tmptime,splitfunc))
tmptime2 = matrix(as.numeric(tmp),ncol=6,byrow=TRUE)

#%: the following two ways are equivalent
##%: method 1: using "sep" in paste
pastefunc <- function(stringvect){
  paste(stringvect[1],stringvect[2],stringvect[3],stringvect[4],
        stringvect[5],stringvect[6],sep="|")
}
tmpdatetime = unlist(apply(tmptime2,1,pastefunc))

##%: method 2: using "collapse" in paste
tmpdatetime = apply(tmptime2,1,paste,collapse="|")

vec.time = as.POSIXct(tmpdatetime,format="%m|%d|%Y|%H|%M|%S")
attr(vec.time,"tzone") <- "EST"
#################################################

#%: try some functions that work on POSIXct functions
vec.time[100]
cut.POSIXt(vec.time[100],"hours") #truncate the date by hours
tmpisodate = ISOdate(2011,11,3,2,40,30,tz="EST")
difftime(vec.time[1],tmpisodate)
difftime(vec.time[1],tmpisodate,units="hours")
tmpisodate2 = Sys.time()
tmpdiff = unlist(lapply(vec.time,FUN=difftime,time2=tmpisodate2,units="weeks"))

#%: extract all years. all three methods do the same thing
##%: method 1: using cut.POSIXt
tmpisodate = ISOdate(2014,10,1,0,0,0,tz="EST")
tmpday = as.POSIXct(unlist(lapply(vec.time,FUN=cut.POSIXt,breaks="day")),tz="EST")
tmpidx1 = which((tmpday == tmpisodate)==TRUE)

##%: method 2: splitting strings
splitday <- function(posixvar){
  return(strsplit(as.character(posixvar),split="[[:punct:]
                  [:space:]]")[[1]][1:3])
}
tmp = unlist(lapply(vec.time,splitday))
tmp2 = matrix(as.numeric(tmp),ncol=3,byrow=TRUE)
tmpidx2 = which(((tmp2[,1]==2014 & tmp2[,2]==10) & tmp2[,3]==1)==TRUE)

##%: method 3: using grep
tmpstring = as.character(vec.time)
tmpidx3 = grep("2014-10-01",tmpstring)

##############################################
##%: Now we demonstrate how to plot a time series
tmpisodate = vec.time[1]
vec.dif = unlist(lapply(vec.time,difftime,time2=tmpisodate,units="mins"))
plot(vec.dif,DATA[,2],x="Time Elapsed (Minutes)",y="Ask Price ($)",title="Plot of Ask Prices")

#%: let's plot what would NOT be good. 
##%: What's NOT good is to completely ignore the time information and uniformally
##%: space out the datapoints
png("timeseries.png",width=600,height=600,units="px")
max.x = max(vec.dif)
num.len = dim(DATA)[1]
plot(c(1:num.len)*max.x/num.len,DATA[,2],col=rgb(1,0,0,.5),xlab="Time Elapsed (Minutes)",ylab="Ask Price ($)",main="Plot of Ask Prices")
points(vec.dif,DATA[,2])
legend("bottomright", c("Accurately Timed Points","Inaccurate Uniformly Spaced Points"), cex=1.2, bty="n", fill=c("black","red"))
dev.off()

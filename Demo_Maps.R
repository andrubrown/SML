setwd("C:/Users/Uikos/Dropbox/GitHub/SML")
load("elect00.rda")
#head(elect00)

#####################
#%: load in the maps package and 
library(maps)
par(mfrow=c(2,2))
map()
map('state','texas')
map('world','italy')
map('italy')
par(mfrow=c(1,1))

data(world.cities)
head(world.cities)
vec.pop = world.cities[,3]
def.par = par(no.readonly = TRUE)
png("map_hist.png",width=800,height=400,units="px")
par(mfrow=c(1,2))
par(mar=c(5,5,5,5))
hist(vec.pop,xlab="size") #see how skewed the distribution of the population is
vec.plotsize = log(vec.pop)/10 #use the log function to make the distribution sizes more reasonable
hist(vec.plotsize,xlab="log-size")
par(def.par) #reset graphical settings
dev.off()
png("map2.png",width=1200,height=600,units="px")
map()
map.axes()
title("Plot of City Sizes",cex.main = 2)
points(x=world.cities[,5],y=world.cities[,4],col=rgb(1,0,0,.25),pch=16,cex=vec.plotsize)
dev.off()

###########################################
#extract the matrix of state county
county.names = map('county', namesonly=T, plot=F)
state.names = map('state', namesonly=T, plot=F)
num.county = length(county.names)

#we briefly show a function "match.map" which 
## the following way is NOT good
randfunc <- function(stringvect){
  return(paste(stringvect[2],stringvect[3],sep=","))
}
tmpname = apply(elect00,1,randfunc)
tmpidx = match.map("county",tmpname)
sum(is.na(tmpidx)) #we see that most of the indices shown here are NA, so something went wrong
county.names[is.na(tmpidx)]

## debugging what went wrong above
## we're going to inspect the representation of "west viriginia"
tmp1 = county.names[2977]
tmp1 = as.character(unlist(tmp1))
tmp1 = unlist(strsplit(as.character(tmp1),split=","))[1]
tmp1 = tolower(tmp1)
tmp1 = unlist(strsplit(tmp1,split=""))
tmp2 = as.character(elect00[3010,2])
tmp2 = tolower(tmp2)
tmp2 = unlist(strsplit(tmp2,split=""))
tmp1 == tmp2

## the following way IS good (only for windows it seems though. not good for macs)
randfunc2 <- function(stringvect){
  tmp = paste(stringvect[2],stringvect[3],sep=",")
  return(iconv(tmp, to='ASCII//TRANSLIT'))
}
tmpname = apply(elect00,1,randfunc2)
tmpidx = match.map("county",tmpname)
sum(is.na(tmpidx))
county.names[is.na(tmpidx)]


tmpidx2 = match.map("state",iconv(as.character(unique(elect00[,2])), to='ASCII//TRANSLIT'))
sum(is.na(tmpidx2)) #see that all the states got matched


#######
#the following versions work for both windows and macs
randfunc2 <- function(stringvect){
  tmp = paste(stringvect[2],stringvect[3],sep=",")
  tmp2 = charToRaw(tmp)
  tmpidx = which(tmp2=="a0")
  if(length(tmpidx)>0){
    tmp2[tmpidx] = charToRaw(" ")
  }
  tmp = rawToChar(tmp2)
  return(tmp)
}
tmpname = apply(elect00,1,randfunc2)
tmpidx = match.map("county",tmpname)
sum(is.na(tmpidx))


randfunc3 <- function(stringvect){
  tmp2 = charToRaw(stringvect)
  tmpidx = which(tmp2=="a0")
  if(length(tmpidx)>0){
    tmp2[tmpidx] = charToRaw(" ")
  }
  tmp = rawToChar(tmp2)
  return(tmp)
}
tmpidx2 = match.map("state",unlist(lapply(as.character(elect00[,2]),randfunc3)))
sum(is.na(tmpidx2)) #see that all the states got matched

############################################

#%: let's make our lives easier by defining a function that we can then iterate over 
##%: all the elements
parsing <-function(levelname){
  return(unlist(strsplit(as.character(levelname),',')))
}
mat.stateCounty = matrix(unlist(lapply(county.names,parsing)),nrow=length(county.names),ncol=2,byrow=TRUE)
mat.stateCounty = data.frame(mat.stateCounty)
colnames(mat.stateCounty) = c("State","County")

#%: do a simple check see if the county names are all unique. we will see they are not
num.county == length(unique(mat.stateCounty[,2]))

#%: preprocess all the strings to set them to lowercase letters and with spaces and periods removed
formatstring <-function(stringname){
  tmp = as.character(stringname)
  tmp2 = charToRaw(tmp)
  tmpidx = which(tmp2=="a0")
  if(length(tmpidx)>0){
    tmp2[tmpidx] = charToRaw(" ")
  }
  tmp = rawToChar(tmp2)
  tmp = gsub("[[:punct:]]","",tmp)
  tmp = tolower(tmp)
  tmp = gsub(" ","",tmp)
  tmp = gsub("county","",tmp)
  tmp = gsub("city","",tmp)
  return(tmp)
}
tmpstate = unlist(lapply(elect00$STATE,formatstring))
tmpcounty = unlist(lapply(elect00$COUNTY,formatstring))
mat.stateCounty[,1] = unlist(lapply(mat.stateCounty[,1],formatstring))
mat.stateCounty[,2] = unlist(lapply(mat.stateCounty[,2],formatstring))

#%: set up a translation matrix
tmpmapping = rep(NA,num.county)
mat.translateIdx = numeric(0) #this vector will store the indices that we need to manually check

for(i in 1:num.county){
  tmpidx1 = which(tmpstate == mat.stateCounty[i,1])
  
  if(mat.stateCounty[i,1]=="districtofcolumbia"){
    tmpidx = tmpidx1
  } else {  
    tmpidx2 = which(tmpcounty == mat.stateCounty[i,2])
    if(length(tmpidx2)!=0){
      tmpidx = intersect(tmpidx1,tmpidx2)
      if(length(tmpidx)>=2) tmpidx = min(tmpidx)
      tmpmapping[i] = tmpidx
    } else {
      mat.translateIdx = c(mat.translateIdx,i)
      print(paste("Idx: ",i,"// state: ",mat.stateCounty[i,1],"// county: ",mat.stateCounty[i,2]))
    }
  }
}

mat.translateName = c("dade","okaloosa","okaloosa","ebatonrouge","stmartin","stmartin",
                      "lewisclark","yellowstone","currituck","currituck","currituck",
                      "galveston","galveston","accomack","accomack",
                      "pierce","pierce","sanjuan","sanjuan","sanjuan","boulder")

tmpvec = rep(NA,num.county)
for(i in 1:num.county){
  tmpidx1 = which(tmpstate == mat.stateCounty[i,1])
  
  tmp = which(mat.translateIdx==i)
  if (length(tmp)>0){
    tmpidx2 = which(tmpcounty == mat.translateName[tmp])
    tmpidx = intersect(tmpidx1,tmpidx2)
  } else if(mat.stateCounty[i,1]=="districtofcolumbia"){
   tmpidx = tmpidx1
  } else {
    tmpidx2 = which(tmpcounty == mat.stateCounty[i,2])
    tmpidx = intersect(tmpidx1,tmpidx2)
  }
  if(length(tmpidx)>=2) tmpidx = min(tmpidx)
  tmpvec[i] = elect00[tmpidx,9]
}

#######################################
#%: create a vector of colors, one color for each state, where each color ranges
##%: between blue (democrat, gore) and red (republican, bush)

max(tmpvec)
min(tmpvec)
mean(tmpvec)
sd(tmpvec)
summary(tmpvec)

num.maxvote = max(tmpvec)
numvoteToColor <- function(numvote){
  tmp = 10^(log(numvote)/log(num.maxvote))/10
  return(rgb(1,0,0,tmp))
}
vec.countyColProp = unlist(lapply(tmpvec,numvoteToColor))
map("county", fill = TRUE, col = vec.countyColProp,resolution = 0)
map("state",add=TRUE,lwd=2)
title("Number of Votes for Bush in the US 2000 Election by County",cex.main = 2)
map.axes()

png("countycolorProp.png",height=800,width=1400,units="px")
map("county", fill = TRUE, col = vec.countyColProp,xlim=c(-125,-65),bg=rgb(.9,.9,.9))
map("state",add=TRUE,lwd=2)
map.axes()
title("Number of Votes for Bush in the US 2000 Election by County",cex.main = 2)
colfunc <- colorRampPalette(c("red", "white"))
legend_image <- as.raster(matrix(colfunc(20), ncol=1))
tmp = seq(0,1,length.out=5)
tmp2 = exp(log(10*tmp)*log(num.maxvote)/log(10))
tmp2 = round(tmp2)
text(x=-66.75, y = seq(26,35,l=5), labels = tmp2)
text(x=-69,y=36,labels="No. of Votes Legend",cex=1.5)
rasterImage(legend_image, -70,26,-68,35)
dev.off()

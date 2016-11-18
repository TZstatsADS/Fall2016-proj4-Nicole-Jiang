
# source("https://bioconductor.org/biocLite.R")
# biocLite("rhdf5")
# install.packages("topicmodels",dependencies = T)
# install.packages("slam")
library(rhdf5)
library(dplyr)
library(topicmodels)

setwd("C:/Users/Administrator/Desktop/proj4/Project4_data")
load("lyr.RData")
setwd("C:/Users/Administrator/Desktop/proj4/Project4_data/data")

FileList = list.files(recursive = T)
nSongs = length(FileList)

# vlist=c(6,10,21,26,78,82,86,88)
lyr_words=as.matrix(lyr[,-1])
m1=LDA(lyr_words,k=100)
m1$Dim
sm1=topics(m1,k=1)
tm1=terms(m1,k=5000)
# write.csv(sm1,"sm1.csv")
category=data.frame(num=c(1:2350),type=sm1)
ind1=category[category$type==3,1]
ind2=category[category$type==4,1]

Resolve=function(FileName){
  a=h5read(FileName,"analysis")
  Seg_n=length(a$segments_start)
  Seg_Duration=diff(a$segments_start)
  ans=data.frame(Duration=Seg_Duration,
                 Loud0=a$segments_loudness_start[-Seg_n],
                 Loud1=a$segments_loudness_max[-Seg_n],
                 t(a$segments_pitches[,-Seg_n]),
                 t(a$segments_timbre[,-Seg_n]))
  return(ans)
}

Collapse=function(x,k){
  n=nrow(x);m=ncol(x)
  ind=c(1:(n-k+1))
  ans=x[ind,]
  for (i in 2:k){
    ind=ind+1
    ans=cbind(ans,x[ind,])
  }
  return(ans)
}


aa=Resolve(FileList[ind1[1]])
aa=aa[,16:27]#Collapse(aa[,16:27],k=2)
for (i in 2:length(ind1)){
  a=Resolve(FileList[ind1[i]])
  aaa=a[,16:27]#Collapse(a[,16:27],k=2)
  aa=rbind(aa,aaa)
}

bb=Resolve(FileList[ind2[1]])
bb=bb[,16:27]#Collapse(bb[,16:27],k=2)
for (i in 1:length(ind2)){
  b=Resolve(FileList[ind2[i]])
  bbb=b[,16:27]#Collapse(b[,16:27],k=2)
  bb=rbind(bb,bbb)
}



# 
# bb=Resolve(FileList[697])
# cc=Resolve(FileList[500])
# aaaa=Collapse(aa[,4:15],k=30)
# bbbb=Collapse(bb[,4:15],k=30)
# cccc=Collapse(cc[,4:15],k=30)
# TwoSeg=rbind(cbind(aa[-639,4:15],aa[-1,4:15]),cbind(bb[-608,4:15],bb[-1,4:15]),cbind(cc[-1,4:15],cc[-382,4:15]))
# EightSeg=rbind(aaaa,bbbb,cccc)
# FourSeg=rbind(aaaa,bbbb,cccc)

cc=rbind(aa[sample(1:dim(aa)[1],size=1000,replace=F),],bb[sample(1:dim(bb)[1],size=1000,replace=F),])
DistMat=as.matrix(dist(cc))

m2=cmdscale(DistMat)
plot(m2,col=c(rep(1,1000),rep(2,1000)),pch="*")

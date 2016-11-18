rowVar <- function(x){
  rowVar= vector()
  for(i in 1: nrow(x)){
    rowVar[i]= var(x[i,])
  }
  return(rowVar)
}
rowMin<- function(x){
  rowMin= vector()
  for(i in 1: nrow(x)){
    rowMin[i]= min(x[i,])
  }
  return(rowMin)
}

mean.loud_max= vector()
var.loud_max=vector()
max.loud_max=vector()
min.loud_max= vector()
mean.loud_time= vector()
var.loud_time= vector()
max.loud_time= vector()
min.loud_time= vector()
mean.pitch= matrix(nrow=length(songs_info), ncol= 12)
var.pitch= matrix(nrow=length(songs_info),ncol= 12)
#max.pitch= matrix(nrow=length(x),ncol= 12)
min.pitch= matrix(nrow=length(songs_info),ncol= 12)
mean.bars_start= vector()
min.bars_start= vector()
max.bars_start= vector()
mean.beats_start= vector()
min.beats_start= vector()
max.beats_start= vector()
for(i in 1:length(songs_info)){
  mean.loud_max[i]= mean(songs_info[[i]]$segments_loudness_max)
  var.loud_max[i]= var(songs_info[[i]]$segments_loudness_max)
  max.loud_max[i]= max(songs_info[[i]]$segments_loudness_max)
  min.loud_max[i]= min(songs_info[[i]]$segments_loudness_max)
  mean.loud_time[i]= mean(songs_info[[i]]$segments_loudness_max_time)
  var.loud_time[i]= var(songs_info[[i]]$segments_loudness_max_time)
  max.loud_time[i]= max(songs_info[[i]]$segments_loudness_max_time)
  min.loud_time[i]= min(songs_info[[i]]$segments_loudness_max_time)
  mean.pitch[i,]= rowMeans(songs_info[[i]]$segments_pitches)
  var.pitch[i,]= rowVar(songs_info[[i]]$segments_pitches)
  #max.pitch[i,]= rowMax(x[[i]]$segments_pitches)
  min.pitch[i,]= rowMin(songs_info[[i]]$segments_pitches)
  mean.bars_start[i]=mean(diff(songs_info[[i]]$bars_start))
  min.bars_start[i]=min(diff(songs_info[[i]]$bars_start))
  max.bars_start[i]=max(diff(songs_info[[i]]$bars_start))
  mean.beats_start[i]= mean(diff(songs_info[[i]]$beats_start))
  min.beats_start[i]=min(diff(songs_info[[i]]$beats_start))
  max.beats_start[i]=max(diff(songs_info[[i]]$beats_start))
}
music_info_stats= cbind(mean.loud_max,var.loud_max,max.loud_max,min.loud_max,mean.loud_time,
                        var.loud_time,max.loud_time,min.loud_time,mean.pitch,var.pitch,min.pitch,
                        mean.bars_start,min.bars_start,max.bars_start,mean.beats_start,
                        min.beats_start, max.beats_start)
##############################################################################
feature= matrix(0,ncol=843)
n.songs= length(songs_info)
l=vector()
proportion= c(0.1, 0.3, 0.5, 0.7,0.9)
feature_temp= vector()
for(i in 1:n.songs){
  ####### segments_loudness_max part
  l= length(songs_info[[i]]$segments_loudness_max)
  Seg_pitch=apply(songs_info[[i]]$segments_pitches,2,which.max)
  Seg_Duration=diff(songs_info[[i]]$segments_start) #added
  
  if(l >60){
    n1 = round(0.1*l)-5
    n2= round(0.3*l)-5
    n3= round(0.5*l)-5
    n4= round(0.7*l)-5
    n5= round(0.9*l)-5
    temp1= songs_info[[i]]$segments_loudness_max[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp2= songs_info[[i]]$segments_loudness_max_time[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    #temp3= songs_info[[i]]$segments_pitches[,c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp3= Seg_pitch[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp4= songs_info[[i]]$segments_timbre[,c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    feature_temp= c(temp1,temp2,temp3,temp4)
  }else if(l>10){ #2284
    n1= min(max(1,round(0.1*l)-5),l-9)
    n2= min(max(1,round(0.3*l)-5), l-9)
    n3= min(max(1,round(0.5*l)-5), l-9)
    n4= min(max(1,round(0.7*l)-5), l-9)
    n5= min(max(1,round(0.9*l)-5), l-9)
    temp1= songs_info[[i]]$segments_loudness_max[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp2= songs_info[[i]]$segments_loudness_max_time[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    #temp3= songs_info[[i]]$segments_pitches[,c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp3= Seg_pitch[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp4= songs_info[[i]]$segments_timbre[,c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    feature_temp= c(temp1,temp2,temp3,temp4)
  } else{ #1375
    temp1= rep(songs_info[[i]]$segments_loudness_max,13)[1:50]
    temp2= rep(songs_info[[i]]$segments_loudness_max_time,13)[1:50]
    #temp3= cbind(songs_info[[i]]$segments_pitches,songs_info[[i]]$segments_pitches,songs_info[[i]]$segments_pitches)[,1:10]
    temp3= rep(Seg_pitch,13)[1:10]
    temp4= do.call(cbind, replicate(13, songs_info[[i]]$segments_timbre, simplify=FALSE))[,1:50]
    feature_temp= c(temp1,temp2,temp3,temp4)
  }
  ACF_pitch=as.vector(acf(Seg_pitch,lag.max = 15,plot=F)$acf) #16
  ACF_loud=as.vector(acf(songs_info[[i]]$segments_loudness_max,lag.max=15,plot=F)$acf) #16
  ACF_len=as.vector(acf(Seg_Duration,lag.max=15,plot=F)$acf) #16
  PACF_pitch=as.vector(pacf(Seg_pitch,lag.max = 15,plot=F)$acf) #15
  PACF_loud=as.vector(pacf(songs_info[[i]]$segments_loudness_max,lag.max=15,plot=F)$acf) #15
  PACF_len=as.vector(pacf(Seg_Duration,lag.max=15,plot=F)$acf) #15
  feature_temp= c(feature_temp, ACF_pitch,ACF_loud,ACF_len, PACF_pitch, PACF_loud,PACF_len) #15
  feature= rbind(feature,feature_temp)
}
feature= feature[-1,]
feature2= cbind(feature, music_info_stats)


Resolve=function(FileName){
  a=h5read(FileName,"analysis") 
  Seg_n=length(a$segments_start) #no need
  Seg_Duration=diff(a$segments_start) #added
  Seg_pitch=apply(a$segments_pitches,2,which.max) #added 1375
  Speed=mean(Seg_Duration)
  Rythm=sd(Seg_Duration)
  Tempo=length(a$beats_start)/length(a$bars_start)
  ACF_pitch=acf(Seg_pitch,lag.max = 15,plot=F)$acf %>% MakeMatx(Seg_n-20)
  ACF_loud=acf(a$segments_loudness_max,lag.max=15,plot=F)$acf %>% MakeMatx(Seg_n-20)
  ACF_len=acf(Seg_Duration,lag.max=15,plot=F)$acf %>% MakeMatx(Seg_n-20)
  PACF_pitch=pacf(Seg_pitch,lag.max = 15,plot=F)$acf %>% MakeMatx(Seg_n-20)
  PACF_loud=pacf(a$segments_loudness_max,lag.max=15,plot=F)$acf %>% MakeMatx(Seg_n-20)
  PACF_len=pacf(Seg_Duration,lag.max=15,plot=F)$acf %>% MakeMatx(Seg_n-20)
  FREQ_pitch=stft(Seg_pitch,inc=1,win = 20,coef=10)$values[-(Seg_n-19),]
  FREQ_loud=stft(a$segments_loudness_max,inc=1,win = 20,coef=10)$values[-(Seg_n-19),]
  FREQ_len=stft(Seg_Duration,inc=1,win = 20,coef=10)$values[-(Seg_n-19),]
  indx=c(1:10,(Seg_n-9):(Seg_n-1))
  ans=data.frame(Duration=Seg_Duration[-indx],
                 Loud0=a$segments_loudness_start[-c(indx,Seg_n)],
                 Loud1=a$segments_loudness_max[-c(indx,Seg_n)],
                 Pitch=Seg_pitch[-c(indx,Seg_n)],
                 t(a$segments_pitches[,-c(indx,Seg_n)]),
                 t(a$segments_timbre[,-c(indx,Seg_n)]),
                 ACF_pitch,   ACF_loud,  ACF_len,
                 PACF_pitch, PACF_loud, PACF_len,
                 FREQ_pitch, FREQ_loud, FREQ_len,
                 Speed=Speed,
                 Tempo=Tempo,
                 Rythm=Rythm)
  return(ans) #ncol= 154
}

n=vector()
for(i in 1361:1400){
  n[i-1360]= length(songs_info[[i]]$segments_start)
}

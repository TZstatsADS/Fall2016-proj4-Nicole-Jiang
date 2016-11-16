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
feature= matrix(0,ncol=1300)
n.songs= length(songs_info)
l=vector()
proportion= c(0.1, 0.3, 0.5, 0.7,0.9)
for(i in 1:n.songs){
  ####### segments_loudness_max part
  feature_temp= 
  l= length(songs_info[[i]]$segments_loudness_max)
  if(l >60){
    n1 = round(0.1*l)-5
    n2= round(0.3*l)-5
    n3= round(0.5*l)-5
    n4= round(0.7*l)-5
    n5= round(0.9*l)-5
    temp1= songs_info[[i]]$segments_loudness_max[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp2= songs_info[[i]]$segments_loudness_max_time[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp3= songs_info[[i]]$segments_pitches[,c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp4= songs_info[[i]]$segments_timbre[,c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
  }else if(l>10){
    n1= min(max(1,round(0.1*l)-5),l-9)
    n2= min(max(1,round(0.3*l)-5), l-9)
    n3= min(max(1,round(0.5*l)-5), l-9)
    n4= min(max(1,round(0.7*l)-5), l-9)
    n5= min(max(1,round(0.9*l)-5), l-9)
    temp1= songs_info[[i]]$segments_loudness_max[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp2= songs_info[[i]]$segments_loudness_max_time[c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp3= songs_info[[i]]$segments_pitches[,c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    temp4= songs_info[[i]]$segments_timbre[,c(n1:(n1+9),n2:(n2+9),n3:(n3+9),n4:(n4+9),n5:(n5+9))]
    feature_temp= c(temp1,temp2,temp3,temp4)
  } else{
    temp1= rep(songs_info[[i]]$segments_loudness_max,3)[1:10]
    temp2= rep(songs_info[[i]]$segments_loudness_max_time,3)[1:10]
    temp3= cbind(songs_info[[i]]$segments_pitches,songs_info[[i]]$segments_pitches,songs_info[[i]]$segments_pitches)[,1:10]
    temp4= cbind(songs_info[[i]]$segments_timbre,songs_info[[i]]$segments_timbre,songs_info[[i]]$segments_timbre)[,1:10]
  }
  feature_temp= c(temp1,temp2,temp3,temp4)
  feature= rbind(feature,feature_temp)
}
feature= feature[-1,]
feature2= cbind(feature, music_info_stats)


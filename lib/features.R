names(songs_info[[1]])
songs_info[[2]]$segments_loudness_max
songs_info[[2]]$segments_loudness_max_time
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


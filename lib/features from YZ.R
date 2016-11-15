sound= songs_info
features8=matrix(nrow=2350,ncol=30)
k=1
for (k in 1:length(sound)){
  length8_k=length(sound[[k]]$segments_loudness_max)
  if (length8_k>=30 && length8_k%%2==0){
    features8[k,]=c(sound[[k]]$segments_loudness_max[1:10],sound[[k]]$segments_loudness_max[(length8_k/2-4):(length8_k/2+5)],sound[[k]]$segments_loudness_max[(length8_k-9):length8_k])
  }
  else if (length8_k>=30 && length8_k%%2!=0) {
    features8[k,]=c(sound[[k]]$segments_loudness_max[1:10],sound[[k]]$segments_loudness_max[((length8_k-1)/2-4):((length8_k-1)/2+5)],sound[[k]]$segments_loudness_max[(length8_k-9):length8_k])
  }
  else if (length8_k<30 && 30%%length8_k!=0){
    features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(30/length8_k)),sound[[k]]$segments_loudness_max[1:(30-floor(30/length8_k)*length8_k)])
    
  }
  else if (length8_k<30 && 30%%length8_k==0){
    features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(30/length8_k)))
  }
  
  k=k+1
}

###9 segments_loudness_max_time
features9=matrix(nrow=2350,ncol=30)
p=1
for (p in 1:length(sound)){
  length9_p=length(sound[[p]]$segments_loudness_max_time)
  if (length9_p>=30 && length9_p%%2==0){
    features9[p,]=c(sound[[p]]$segments_loudness_max_time[1:10],sound[[p]]$segments_loudness_max_time[(length9_p/2-4):(length9_p/2+5)],sound[[p]]$segments_loudness_max_time[(length9_p-9):length9_p])
  }
  else if (length9_p>=30 && length9_p%%2!=0) {
    features9[p,]=c(sound[[p]]$segments_loudness_max_time[1:10],sound[[p]]$segments_loudness_max_time[((length9_p-1)/2-4):((length9_p-1)/2+5)],sound[[p]]$segments_loudness_max_time[(length9_p-9):length9_p])
  }
  else if (length9_p<30 && 30%%length9_p!=0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(30/length9_p)),sound[[p]]$segments_loudness_max_time[1:(30-floor(30/length9_p)*length9_p)])
    
  }
  else if (length9_p<30 && 30%%length9_p==0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(30/length9_p)))
  }
  
  p=p+1
}


###11 segments_pitches
features11=matrix(nrow=2350,ncol=360)
m=1
for (m in 1:length(sound)){
  length11_m=ncol(sound[[m]]$segments_pitches)
  if (length11_m>=30 && length11_m%%2==0){
    features11[m,]=c(as.vector(sound[[m]]$segments_pitches[,1:10]),as.vector(sound[[m]]$segments_pitches[,(length11_m/2-4):(length11_m/2+5)]),as.vector(sound[[m]]$segments_pitches[,(length11_m-9):length11_m]))
  }
  else if (length11_m>=30 && length11_m%%2!=0) {
    features11[m,]=c(as.vector(sound[[m]]$segments_pitches[,1:10]),as.vector(sound[[m]]$segments_pitches[,((length11_m-1)/2-4):((length11_m-1)/2+5)]),as.vector(sound[[m]]$segments_pitches[,(length11_m-9):length11_m]))
  }
  else if (length11_m<30 && 30%%length11_m!=0){
    features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(30/length11_m)),as.vector(sound[[m]]$segments_pitches[,1:(30-floor(30/length11_m)*length11_m)]))
    
  }
  else if (length11_m<30 && 30%%length11_m==0){
    features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(30/length11_m)))
  }
  
  m=m+1
}

###13 segments_timbre
features13=matrix(nrow=2350,ncol=360)
q=1
for (q in 1:length(sound)){
  length13_q=ncol(sound[[q]]$segments_timbre)
  if (length13_q>=30 && length13_q%%2==0){
    features13[q,]=c(as.vector(sound[[q]]$segments_timbre[,1:10]),as.vector(sound[[q]]$segments_timbre[,(length13_q/2-4):(length13_q/2+5)]),as.vector(sound[[q]]$segments_timbre[,(length13_q-9):length13_q]))
  }
  else if (length13_q>=30 && length13_q%%2!=0) {
    features13[q,]=c(as.vector(sound[[q]]$segments_timbre[,1:10]),as.vector(sound[[q]]$segments_timbre[,((length13_q-1)/2-4):((length13_q-1)/2+5)]),as.vector(sound[[q]]$segments_timbre[,(length13_q-9):length13_q]))
  }
  else if (length13_q<30 && 30%%length13_q!=0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(30/length13_q)),as.vector(sound[[q]]$segments_timbre[,1:(30-floor(30/length13_q)*length13_q)]))
    
  }
  else if (length13_q<30 && 30%%length13_q==0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(30/length13_q)))
  }
  
  q=q+1
}

features_num<-cbind(features8,features9,features11,features13)


sound= songs_info
features8=matrix(nrow=2350,ncol=90)
k=1
index8=array(dim=9)
for (k in 1:length(sound)){
  length8_k=length(sound[[k]]$segments_loudness_max)
  a=1
  b=1
  for (a in 2:10){index8[a-1]=floor(length8_k/a)
  a=a+1}
  if (length8_k>=90){
    for (b in 1:9) {features8[k,(10*(b-1)+1):(10*b)]=sound[[k]]$segments_loudness_max[(index8[b]-4):(index8[b]+5)]
    b=b+1}
  }
  
  else if (length8_k<90 && 90%%length8_k!=0){
    features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(90/length8_k)),sound[[k]]$segments_loudness_max[1:(90-floor(90/length8_k)*length8_k)])
    
  }
  else if (length8_k<90 && 90%%length8_k!=0){
    features8[k,]=c(rep(sound[[k]]$segments_loudness_max,floor(90/length8_k)))
  }
  
  k=k+1
}

###9 segments_loudness_max_time
features9=matrix(nrow=2350,ncol=90)
p=1
index9=array(dim=9)
for (k in 1:length(sound)){
  length9_p=length(sound[[p]]$segments_loudness_max_time)
  a=1
  b=1
  for (a in 2:10){index9[a-1]=floor(length9_p/a)
  a=a+1}
  if (length9_p>=90){
    for (b in 1:9) {features9[p,(10*(b-1)+1):(10*b)]=sound[[p]]$segments_loudness_max_time[(index9[b]-4):(index9[b]+5)]
    b=b+1}
  }
  
  else if (length9_p<90 && 90%%length9_p!=0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(90/length9_p)),sound[[p]]$segments_loudness_max_time[1:(90-floor(90/length9_p)*length9_p)])
    
  }
  else if (length9_p<90 && 90%%length9_p!=0){
    features9[p,]=c(rep(sound[[p]]$segments_loudness_max_time,floor(90/length9_p)))
  }
  
  p=p+1
}


###11 segments_pitches
features11=matrix(nrow=2350,ncol=1080)
m=1
index11=array(dim=9)
for (m in 1:length(sound)){
  length11_m=ncol(sound[[m]]$segments_pitches)
  a=1
  b=1
  for (a in 2:10){ index11[a-1]=floor(length11_m/a)
  a=a+1}
  if (length11_m>=90){
    for (b in 1:9) {features11[m,(120*(b-1)+1):(120*b)]=as.vector(sound[[m]]$segments_pitches[,(index11[b]-4):(index11[b]+5)])
    b=b+1
    }
  }
  
  else if (length11_m<90 && 90%%length11_m!=0){
    features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(90/length11_m)),as.vector(sound[[m]]$segments_pitches[,1:(90-floor(90/length11_m)*length11_m)]))
    
  }
  else if (length11_m<90 && 90%%length11_m==0){
    features11[m,]=c(rep(as.vector(sound[[m]]$segments_pitches),floor(90/length11_m)))
  }
  
  m=m+1
}

###13 segments_timbre
features13=matrix(nrow=2350,ncol=1080)
index13=array(dim=9)
q=1
for (m in 1:length(sound)){
  length13_q=ncol(sound[[q]]$segments_timbre)
  a=1
  b=1
  for (a in 2:10){index13[a-1]=floor(length13_q/a)
  a=a+1}
  if (length13_q>=90){
    for (b in 1:9) {features13[q,(120*(b-1)+1):(120*b)]=as.vector(sound[[q]]$segments_timbre[,(index13[b]-4):(index13[b]+5)])
    b=b+1
    }
  }
  
  else if (length13_q<90 && 90%%length13_q!=0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(90/length13_q)),as.vector(sound[[q]]$segments_timbre[,1:(90-floor(90/length13_q)*length13_q)]))
    
  }
  else if (length13_q<90 && 90%%length13_q==0){
    features13[q,]=c(rep(as.vector(sound[[q]]$segments_timbre),floor(90/length13_q)))
  }
  
  q=q+1
}

features_num2<-cbind(features8,features9,features11,features13)

index= sample(2350,2200)
y_train= y[index,]
y_test= y[-index,]
features_num= as.data.frame(features_num)
features_train= features_num[index,]
features_test= features_num[-index,]

param <- list(objective = "binary:logistic",
              max_depth = 7,
              eta = 0.13,
              gamma = 0.1
)
y_pre= vector()
for(i in 1:20){
  dtrain <- xgb.DMatrix(as.matrix(features_train),label = y_train[,i])
  xg.fit <- xgboost(data=dtrain, params=param, nrounds=20, nthread=6)
  pre= predict(xg.fit, as.matrix(features_test))
  #ratio= colMeans(y)[i]
  #y_pre= cbind(y_pre, as.numeric(pre > quantile(pre,prob=(1-ratio))))
  y_pre= cbind(y_pre, pre)
}

rank.words_test= matrix(0, ncol=5000)
for(i in 1:(2350-length(index))){
  rank.words_test= rbind(rank.words_test,colSums(y_pre[i,]*words.prob))
}
rank.words_test= rank.words_test[-1,]
#########################################################################
lyr_test= lyr_data[-index,]
lyr_test= lyr_test !=0
result= apply( lyr_test, 1, function(x) which(x !=0))
temp_sum=0
for(i in 1:(2350-length(index))){
  temp= 5001-rank(rank.words_test[i,])
  temp_sum= temp_sum+sum(temp[result[[i]]])/length(result[[i]])
}
temp_sum/(2350-length(index))

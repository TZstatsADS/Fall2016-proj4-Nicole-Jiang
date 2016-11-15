library(xgboost)
set.seed(122)
index= sample(2350,2200)
y_train= y[index,]
y_test= y[-index,]
music= music_info_stats
music[is.na(music)]=0
music_train= music[index,]
music_test=music[-index,]
param <- list(objective = "binary:logistic",
              max_depth = 6,
              eta =0.05,
              gamma = 0.1
)
model= xgboost(data=music_train, label=y_train[,1],params=param, nrounds=20, nthread=6)
pre= predict(model,music_test )
##########################################
features_num= as.data.frame(features_num)
features_train= features_num[index,]
features_test= features_num[-index,]
model2= xgboost(data=features_train, label=y_train[,1],params=param, nrounds=50, nthread=6)
pre= predict(model2,features_test )

dtrain <- xgb.DMatrix(as.matrix(features_train),label = y_train[,1])
best_param = list()
best_seednumber = 1234
best_logloss = Inf
best_logloss_index = 0

for (iter in 1:15) {
  param <- list(objective = "binary:logistic",
                max_depth = sample(6:10, 1),
                eta = runif(1, .01, .3),
                gamma = runif(1, 0.0, 0.2) 
  )
  cv.nround = 50
  cv.nfold = 5
  seed.number = sample.int(10000, 1)[[1]]
  set.seed(seed.number)
  mdcv <- xgb.cv(data=dtrain, params = param, nthread=6, 
                 nfold=cv.nfold, nrounds=cv.nround,
                 verbose = T, early.stop.round=8, maximize=FALSE)
  
  min_logloss = min(mdcv[, test.error.mean])
  min_logloss_index = which.min(mdcv[, test.error.mean])
  
  if (min_logloss < best_logloss) {
    best_logloss = min_logloss
    best_logloss_index = min_logloss_index
    best_seednumber = seed.number
    best_param = param
  }
}
###########################   get XG boost model best_param: depth=7, eta= 0.1280138, gamma= 0.1097622
nround = best_logloss_index
set.seed(best_seednumber)
xg.fit <- xgboost(data=dtrain, params=best_param, nrounds=nround, nthread=6)
pre= predict(xg.fit, features_test)
ratio= colMeans(y)[1]
y_pre= as.numeric(pre > quantile(pre,prob=(1-ratio)))
sum(abs(y_test[,1]-y_pre))
y_test[, 1]
##########################################
library(randomForest)
data.train= as.data.frame(cbind(music_train,y_train[,1]))
model3= randomForest(as.factor(V51)~., data.train)
as.factor(y_train[,1])
for(i in 1:50){
  print(class(data.train[,i]))
}
is.na(data.train)

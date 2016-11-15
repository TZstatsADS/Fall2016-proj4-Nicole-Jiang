library(xgboost)
set.seed(123)
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
features_train= features_num[index,]
features_test= features_num[-index,]
model2= xgboost(data=features_train, label=y_train[,1],params=param, nrounds=20, nthread=6)
pre= predict(model2,features_test )


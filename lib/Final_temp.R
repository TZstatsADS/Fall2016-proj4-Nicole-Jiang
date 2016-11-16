########################################################################
# Set directory
setwd("~/Documents/Courses/ADS/Project 4/Project4_data")
########################################################################
# Load libraries
library(rhdf5)
library(NLP)
library(lda)
library(LDAvis)
library(dplyr) 
library(magrittr)
library(lda)
load("lyr.RData")
########################################################################
# Read the music information
songs_info= list()
num= 1
for(i in 1:2){
  for(j in 1:26){
    for(k in 1:26){
      #h5read("data/A/A/A/TRAAABD128F429CF47.h5", "/analysis")
      temp_path= sprintf("data/%s/%s/%s/", LETTERS[i],LETTERS[j],LETTERS[k])
      if(length(dir(temp_path)) != 0){
        for(l in 1:length(dir(temp_path))){
          temp_name= paste(temp_path, dir(temp_path)[l], sep = "")
          songs_info[[num]]= h5read(temp_name, "/analysis")
          num=num+1        
        }
      }
    }
  }
}
set.seed(1000)
index= sample(2350,2100)
########################################################################
# LDA set up
lyr_data= lyr[index,-1]
vocab= colnames(lyr)[-1]
n.train=length(index)
word.list= list()
for(i in 1:n.train){
  word.list[[i]]= rep(colnames(lyr_data), lyr_data[i,])
}

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index), as.integer(rep(1, length(index))))
}

doc <- lapply(word.list, get.terms)
# LDA: assgin topic
topic_number <- 20
iteration_number <- 100
alpha <- 0.02
eta <- 0.02
set.seed(998)
vocab2= colnames(lyr)
words.lda <- lda.collapsed.gibbs.sampler(documents = doc, K = topic_number, vocab = vocab2, 
                                         num.iterations = iteration_number, alpha = alpha, 
                                         eta = eta, initial = NULL, burnin = 0,
                                         compute.log.likelihood = TRUE)
theta <- t(apply(words.lda$document_sums + alpha, 2, function(x) x/sum(x)))  
y_train= ifelse(theta<0.1,0,1)
words.prob <- t(apply(t(words.lda$topics) + eta, 2, function(x) x/sum(x)))[,-1]

########################################################################
# Fit XG boost model
features= as.data.frame(feature2)
features[is.na(features)]=0
features_train= features[index,]
features_test= features[-index,]
param <- list(objective = "binary:logistic", max_depth = 7,eta = 0.13, gamma = 0.1)

y_pre= vector()
for(i in 1:20){
  dtrain <- xgb.DMatrix(as.matrix(features_train),label = y_train[,i])
  xg.fit <- xgboost(data=dtrain, params=param, nrounds=20, nthread=6)
  pre= predict(xg.fit, as.matrix(features_test))
  y_pre= cbind(y_pre, pre)
}

########################################################################
# Fit predict probability of 5000 word in the test songs
rank.words_test= matrix(0, ncol=5000)
for(i in 1:(2350-length(index))){
  rank.words_test= rbind(rank.words_test,colSums(y_pre[i,]*words.prob))
}
rank.words_test= rank.words_test[-1,]
########################################################################
# Add prior probability
prior= colSums(lyr_data)/sum(lyr_data)
prior_mat= matrix(rep(prior,(2350-length(index))),nrow= (2350-length(index)),byrow=T)
dim(prior_mat)
rank.words= prior_mat* rank.words_test
########################################################################
# Check with original lyrics
lyr_test= lyr[-index,-1]
lyr_test= lyr_test !=0
result= apply(lyr_test, 1, function(x) which(x !=0))
temp_sum=0
for(i in 1:(2350-length(index))){
  temp= 5001-rank(rank.words_test[i,])
  temp_sum= temp_sum+sum(temp[result[[i]]])/length(result[[i]])
}
temp_sum/(2350-length(index))




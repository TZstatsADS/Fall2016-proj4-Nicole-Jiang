setwd("~/Documents/Courses/ADS/Project 4/Project4_data")

library(rhdf5)
library(NLP)
library(lda)
library(LDAvis)
library(dplyr) 
library(magrittr)
############# read the music information
songs_name= vector()
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
          songs_name[num]=  substr(dir(temp_path)[l],1,18)
          songs_info[[num]]= h5read(temp_name, "/analysis")
          num=num+1        
        }
      }
    }
  }
}

############### assign topics
load("lyr.RData")
vocab= colnames(lyr)[-1]
n.train=2350
word.list= list()
t1= Sys.time()
for(i in 1:n.train){
  word.list[[i]]= rep(colnames(lyr)[2:5000], lyr[i,2:5000])
}
t2= Sys.time()
t2-t1

get.terms <- function(x) {
  index <- match(x, vocab)
  index <- index[!is.na(index)]
  rbind(as.integer(index), as.integer(rep(1, length(index))))
}

doc <- lapply(word.list, get.terms)
################ fit the LDA
topic_number <- 20
iteration_number <- 50
alpha <- 0.02
eta <- 0.02
library(lda)
set.seed(327)
#t1 <- Sys.time()
words.lda <- lda.collapsed.gibbs.sampler(documents = doc, K = topic_number, vocab = vocab, 
                                   num.iterations = iteration_number, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
#t2 <- Sys.time()
theta <- t(apply(words.lda$document_sums + alpha, 2, function(x) x/sum(x))) 
#the possibility of falling into a certain topic
topic_indicator <- apply(theta,1,order) %>%t()# %>% is_greater_than(15) %>% as.numeric() %>% matrix(n.train,20)
y= matrix(0,n.train,20)
for(i in 1:n.train){
  y[i,topic_indicator[i,16:20]]= 1
}
words.prob <- t(apply(t(words.lda$topics) + eta, 2, function(x) x/sum(x)))
######################## test topic
rank.words_test= matrix(0, ncol=5000)
for(i in 1:20){
  rank.words_test= rbind(rank.words_test,colSums(y[i,]*words.prob))
}
rank.words_test= rank.words_test[-1,]
#rank.words_test_sort <- t(apply(rank.words_test,1,sort))
######
# true lyr
lyr_test= lyr[1:20,-1]
lyr_test= lyr_test !=0
result= apply( lyr_test, 1, function(x) which(x !=0))
temp_sum=0
for(i in 1:20){
  temp= 5001-rank(rank.words_test[i,])
  temp_sum= temp_sum+sum(temp[result[[i]]])/length(result[[i]])
}
temp_sum/21
#######################
prob_test= matrix(c(0.3,0.1,0.02,0.48,0.4,0.1,0.2,0.3,0.5,0.4,0.05,0.05),3,4)
y_test= c(1,0,1)
y_test*prob_test

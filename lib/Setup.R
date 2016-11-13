setwd("~/Documents/Courses/ADS/Project 4/Project4_data")

library(rhdf5)

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
t1 <- Sys.time()
words.lda <- lda.collapsed.gibbs.sampler(documents = doc, K = topic_number, vocab = vocab, 
                                   num.iterations = iteration_number, alpha = alpha, 
                                   eta = eta, initial = NULL, burnin = 0,
                                   compute.log.likelihood = TRUE)
t2 <- Sys.time()
theta <- t(apply(words.lda$document_sums + alpha, 2, function(x) x/sum(x))) 
#the possibility of falling into a certain topic



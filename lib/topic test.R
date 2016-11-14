
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
  rank.words_test= rbind(rank.words_test,colSums(topic_indicator[i,]*words.prob))
}
rank.words_test= rank.words_test[-1,]
#rank.words_test_sort <- t(apply(rank.words_test,1,sort))
######
# true lyr
lyr_test= lyr[1:20,-1]
#lyr_test= lyr_test !=0
result= apply( lyr_test, 1, function(x) which(x !=0))
temp_sum=0
for(i in 1:20){
  temp= 5001-rank(rank.words_test[i,])
  temp_sum= temp_sum+sum(temp[result[[i]]])/length(result[[i]])
}
temp_sum/21


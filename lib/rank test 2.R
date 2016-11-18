######## test from Dai Minghao
set.seed(402)
index = sample(2350, 2100)

jcc <- final
jcc <- jcc[,-1]

jcc.test <- lyr[-index,-1]


rs.jcc <- rep(NA, nrow(jcc))
r.bar= 2500.5
for (i in 1:nrow(jcc)){
  #rank.jcc <- rank(-jcc[i,])
  rank.jcc <- jcc[i,]
  ind <- which(jcc.test[i,] != 0)
  rs.jcc[i] <- sum(rank.jcc[ind])/r.bar/length(ind)
}
mean(rs.jcc)

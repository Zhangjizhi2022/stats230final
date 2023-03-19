set.seed(17)
mean<-c(1,1)
sigma<-matrix(c(1,0,0,1),nrow = 2,ncol = 2)
sim<-mvrnorm(500,mean,sigma)
y=rbinom(500,1,exp(-1+sim[,1])/(1+exp(-1+sim[,1])))
N = length(y)
iter <- 100
U <- matrix(NA, nrow=N, ncol=iter)
U[, 1] <- c(rep(0, 400), rep(1, 100))
for (i in 1:iter){
  
  set.seed(i)
  U.prop <-sample(1:500,100,replace = FALSE)
  y.prop <- y[U.prop]
  n <- length(U.prop)
  delta_n.prop <- sum(y) - N/n*sum(y.prop)
  delta_n <- sum(y)- N/n*sum(y.cur)
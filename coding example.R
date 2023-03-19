set.seed(17)
mean<-c(1,1)
sigma<-matrix(c(1,0,0,1),nrow = 2,ncol = 2)
sim<-mvrnorm(500,mean,sigma)
y=rbinom(500,1,exp(-1+sim[,1])/(1+exp(-1+sim[,1])))
N = length(y)
d = 3
n <- 100
epsilon <- 0.005
iter <- 100
U <- matrix(NA, nrow=N, ncol=iter+1)
theta <- matrix(NA, nrow=d, ncol=iter+1)
theta[, 1] <- c(rep(0.5, d))
U[, 1] <- c(rep(0, 400), rep(1, 100))
for (i in 1:iter){
  set.seed(i)
  U.prop <-sample(1:500, n, replace = FALSE)
  y.cur <- y[which(U[, 1] == 1)]
  y.prop <- y[U.prop]
  delta_n.prop <- sum(y) - N/n*sum(y.prop)
  delta_n <- sum(y)- N/n*sum(y.cur)
  b <- exp(epislon*(delta_n^2-delta_n.prop^2))
  beta <- min(1,b)
  U[,i+1] <- rep(0,500)
  if (runif(1)<beta){
    U[,i+1][U.prop] <- 1
  }
  else U[,i+1] <- U[,i]
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
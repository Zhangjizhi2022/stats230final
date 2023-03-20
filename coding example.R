set.seed(17)
mean<-c(1,1)
sigma<-matrix(c(1,0,0,1),nrow = 2,ncol = 2)
sim<-mvrnorm(500,mean,sigma)
y=rbinom(500,1,exp(-1+sim[,1])/(1+exp(-1+sim[,1])))
N = length(y)
d = 3
n <- 100
delta <- 0.1
epsilon <- 0.005
iter <- 10000
U <- matrix(NA, nrow=N, ncol=iter+1)
betas <- matrix(NA, nrow=d, ncol=iter+1)
betas[, 1] <- c(rep(0.5, d))
U[, 1] <- c(rep(0, 400), rep(1, 100))

loglike <- function(beta, x, y) {
  eta <- x %*% beta
  loglike <- sum(y * eta - log(1 + exp(eta)))
  return(loglike)
}

# Define the prior distribution for beta
prior <- function(beta) {
  prior <- dnorm(beta, 0, 10, log = TRUE)
  return(prior)
}


for (i in 1:iter){
  set.seed(i)
  U.prop <-sample(1:500, n, replace = FALSE)
  y.cur <- y[which(U[, i] == 1)]
  y.prop <- y[U.prop]
  delta_n.prop <- sum(y) - N/n*sum(y.prop)
  delta_n <- sum(y)- N/n*sum(y.cur)
  b <- exp(epsilon*(delta_n^2-delta_n.prop^2))
  U[,i+1] <- rep(0,500)
  if (runif(1) < b){
    U[,i+1][U.prop] <- 1
  }
  else U[,i+1] <- U[,i]
  betas.prop <- rep(0,3)
  betas.prop[1] <- betas[1,i]+runif(1,min = -delta, max = delta)
  betas.prop[2] <- betas[2,i]+runif(1,min = -delta, max = delta)
  betas.prop[3] <- betas[3,i]+runif(1,min = -delta, max = delta)
  y.cur <- y[which(U[, i+1] == 1)]
  x.cur <- cbind(1,sim[which(U[, i+1] == 1),1],sim[which(U[, i+1] == 1),2])
  log_ratio <- loglike(betas.prop, x.cur, y.cur) + prior(betas.prop) -
    loglike(betas[,i], x.cur, y.cur) - prior(betas[,i])
  ratio <- mean(exp(log_ratio))
  betas[,i+1] <- rep(0,3)
  if (runif(1) < ratio){
    betas[,i+1] <- betas.prop
  }
  else betas[,i+1] <- betas[,i]
}

  
 plot(density(betas[1,]),type = 'l') 
 result1<- betas 
  
  
 for (i in 1:iter){
   betas.prop <- rep(0,3)
   betas.prop[1] <- betas[1,i]+runif(1,min = -delta, max = delta)
   betas.prop[2] <- betas[2,i]+runif(1,min = -delta, max = delta)
   betas.prop[3] <- betas[3,i]+runif(1,min = -delta, max = delta)
   y.cur <- y
   x.cur <- cbind(1,sim[,1],sim[,2])
   log_ratio <- loglike(betas.prop, x.cur, y.cur) + prior(betas.prop) -
     loglike(betas[,i], x.cur, y.cur) - prior(betas[,i])
   ratio <- mean(exp(log_ratio))
   #p.cur <- mean(plogis(betas[1,i]+betas[2,i]*sim[which(U[, i+1] == 1),1]+betas[3,i]*sim[which(U[, i+1] == 1),2]))
   #p.prop <- mean(plogis(betas.prop[1]+betas.prop[2]*sim[which(U[, i+1] == 1),1]+betas.prop[3]*sim[which(U[, i+1] == 1),2]))
   #a <- p.prop^sum(y.cur)*(1-p.prop)^(n-sum(y.cur))/(p.cur^sum(y.cur)*(1-p.cur)^(n-sum(y.cur)))
   #alpha <- min(1,a)
   betas[,i+1] <- rep(0,3)
   if (runif(1) < ratio){
     betas[,i+1] <- betas.prop
   }
   else betas[,i+1] <- betas[,i]
 }  
  
  result2 <- betas
  
  n<-70
  for (i in 1:iter){
    set.seed(i)
    U.prop <-sample(1:500, n, replace = FALSE)
    y.cur <- y[which(U[, i] == 1)]
    y.prop <- y[U.prop]
    delta_n.prop <- sum(y) - N/n*sum(y.prop)
    delta_n <- sum(y)- N/n*sum(y.cur)
    b <- exp(epsilon*(delta_n^2-delta_n.prop^2))
    U[,i+1] <- rep(0,500)
    if (runif(1) < b){
      U[,i+1][U.prop] <- 1
    }
    else U[,i+1] <- U[,i]
    betas.prop <- rep(0,3)
    betas.prop[1] <- betas[1,i]+runif(1,min = -delta, max = delta)
    betas.prop[2] <- betas[2,i]+runif(1,min = -delta, max = delta)
    betas.prop[3] <- betas[3,i]+runif(1,min = -delta, max = delta)
    y.cur <- y[which(U[, i+1] == 1)]
    x.cur <- cbind(1,sim[which(U[, i+1] == 1),1],sim[which(U[, i+1] == 1),2])
    log_ratio <- loglike(betas.prop, x.cur, y.cur) + prior(betas.prop) -
      loglike(betas[,i], x.cur, y.cur) - prior(betas[,i])
    ratio <- mean(exp(log_ratio))
    betas[,i+1] <- rep(0,3)
    if (runif(1) < ratio){
      betas[,i+1] <- betas.prop
    }
    else betas[,i+1] <- betas[,i]
  }
  
  
  lines(density(betas[1,]),type = 'l',col='blue') 
  result3<- betas 
  
  plot(betas[1,],type = 'l')
  
  
  
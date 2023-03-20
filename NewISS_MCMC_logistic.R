#' Informed Sub-Sampling MCMC function
#'
#' @param N 
#' @param n 
#' @param d 
#' @param delta 
#' @param epsilon 
#' @param iter 
#'
#' @return
#' @export
#'
#' @examples 
#' ##Simulated Data
#' set.seed(17)
#' mean<-c(1,1)
#' sigma<-matrix(c(1,0,0,1),nrow = 2,ncol = 2)
#' N = 10000
#' sim<-mvrnorm(N,mean,sigma)
#' y=rbinom(N,1,exp(-1+sim[,1])/(1+exp(-1+sim[,1])))


ISS_MCMC_logistic <- function(sim,y,N, n, d, delta, epsilon, iter)
{
  #storing subsamples and output
  U <- matrix(NA, nrow=N, ncol=iter+1)
  betas <- matrix(NA, nrow=d, ncol=iter+1)
  
  #initializing
  betas[, 1] <- c(rep(0.5, d))
  U[, 1] <- c(rep(0, N-n), rep(1, n))
  
  #likelihood function
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
    U.prop <-sample(1:N, n, replace = FALSE)
    y.cur <- y[which(U[, i] == 1)]
    y.prop <- y[U.prop]
    delta_n.prop <- sum(y) - N/n*sum(y.prop)
    delta_n <- sum(y)- N/n*sum(y.cur)
    b <- exp(epsilon*(delta_n^2-delta_n.prop^2))
    U[,i+1] <- rep(0,N)
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
  return(betas, U)
}
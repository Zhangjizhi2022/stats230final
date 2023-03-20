#' Original MRH MCMC function
#'
#' @param d 
#' @param delta 
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
#' d = 3
#' delta = 0.2
#' sim<-mvrnorm(N,mean,sigma)
#' y=rbinom(N,1,exp(-1+sim[,1])/(1+exp(-1+sim[,1])))

MRH_MCMC_logistic <- function(sim,y, d, delta){
  for (i in 1:iter){
    #allocate space to store output
    betas <- matrix(NA, nrow=d, ncol=iter+1)
    
    #initializing
    betas[, 1] <- c(rep(0.5, d))
    
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
    
    #running the MRH algorithm
    betas.prop <- rep(0,3)
    betas.prop[1] <- betas[1,i]+runif(1,min = -delta, max = delta)
    betas.prop[2] <- betas[2,i]+runif(1,min = -delta, max = delta)
    betas.prop[3] <- betas[3,i]+runif(1,min = -delta, max = delta)
    y.cur <- y
    x.cur <- cbind(1,sim[,1],sim[,2])
    log_ratio <- loglike(betas.prop, x.cur, y.cur) + prior(betas.prop) -
      loglike(betas[,i], x.cur, y.cur) - prior(betas[,i])
    ratio <- mean(exp(log_ratio))
    betas[,i+1] <- rep(0,3)
    if (runif(1) < ratio){
      betas[,i+1] <- betas.prop
    }
    else betas[,i+1] <- betas[,i]
  }  
  return(betas)
}
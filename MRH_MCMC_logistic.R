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
#' sim<-mvrnorm(N,mean,sigma)
#' y=rbinom(N,1,exp(-1+sim[,1])/(1+exp(-1+sim[,1])))

MRH_MCMC_logistic <- function(d, delta){
  for (i in 1:iter){
    #allocate space to store output
    betas <- matrix(NA, nrow=d, ncol=iter+1)
    
    #initializing
    betas[, 1] <- c(rep(0.5, d))
    
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
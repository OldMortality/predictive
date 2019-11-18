# number of observations
getN <- function() return(100) 
getM <- function() return(5)
getErrorSD <- function()  return(1)
  
getAlphas <- function(M,seed) {
  set.seed(seed)
  return(runif(M + 1,0,1))
}

#genAlphas(M = getM(), seed = 10)

# returns (N*M+1) matrix
getX <- function(N,M) {
  return(matrix(data=c(rep(1,N),
                       rep(rnorm(N,0,1),M)),byrow=F,nrow=N))
}


getX(N=10,2)

getErr <- function(N,seed) {
  set.seed(seed)
  return(rnorm(N,mean=0,sd=getErrorSD()))
}

getErrs(N,seed) {
  return(matrix(data=getErr(N,)))
}

getObs <- function(N,M,seed,errSeed) {
  return(  getX(N,M) %*%  matrix(data=getAlphas(M,seed),byrow=F,nrow=M+1) + getErrs(N,errSeed))
}

getObs(10,3,seed=10,errSeed = c(31,13))


library(purrr)
squar3 <- function(x) {
  return(x^2)
}
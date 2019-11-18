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


getErrByNSeed <- function(N,seed) {
  
  function(seed) {
    set.seed(seed)
    return(rnorm(N,mean=0,sd=getErrorSD()))  
  }
  
}

#getErrbySeed <- getErrByNSeed(N)

#seeds <- c(1,2,3)


#t <- map(seeds,getErrByNSeed(200))
#class(t)


errsAsMatrix <- function(theList,N) {
  return(matrix(unlist(theList),byrow=F,nrow=N))
}

# get error vectors for given N and vector of seeds, eg.
#   getErrs(1000,c(1,2,3))
#   returns list with 1 element for each seed, each element
#      consists of N error values.
getErrs <- function(N,seeds) {
  return(map(seeds,getErrByNSeed(N)) %>% errsasMatrix(...,N))
}

getObs <- function(N,M,seed,errSeed) {
  return(  getX(N,M) %*%  matrix(data=getAlphas(M,seed),byrow=F,nrow=M+1) ) #+ getErrs(N,errSeed))
}


getObs(N=4,M=2,seed=10,errSeed=c(1,2,3,4))
# v1 is N-vector of the determinate part of the model
# l1 is list of errors, 1 list for each simulation, each list contains N values.
#    returns: vectors of v1 + l1, one for each simualation, making up column of matrix
addObsErr(v1,l1) {
  
}

getErrs(10,c(31,13))[[1]] 
getObs(10,3,seed=10,errSeed = c(31,13))


##
## Program to compare model averaging with ridge regression
##   and lasso
##
##

# number of observations
getN <- function() return(100) 
getM <- function() return(5)
getErrorSD <- function()  return(1)
  
getAlphas <- function(M,seed) {
  set.seed(seed)
  return(runif(M + 1,0,1))
}

#genAlphas(M = getM(), seed = 10)

setNames <- function(X) {
  colnames(X) <- LETTERS[1:(getM()+1)] # works up to 26
  return(X)
}


# returns (N*M+1) matrix
getX <- function(N,M) {
  return(
    matrix(data=c(rep(1,N),
         rep(rnorm(N,0,1),M)),byrow=F,nrow=N) %>%
    setNames())
}


# z <- getX(getN(),getM())
# dim(z)


getErrByNSeed <- function(N,seed) {
  
  function(seed) {
    set.seed(seed)
    return(rnorm(N,mean=0,sd=getErrorSD()))  
  }
  
}

#getErrbySeed <- getErrByNSeed(N)


errsAsMatrix <- function(theList,N) {
  return(matrix(unlist(theList),byrow=F,nrow=N))
}

# get error vectors for given N and vector of seeds, eg.
#   getErrs(1000,c(1,2,3))
#   returns list with 1 element for each seed, each element
#      consists of N error values.
getErrs <- function(N,seeds) {
  return(map(seeds,getErrByNSeed(N)) %>% errsAsMatrix(N))
}

# returns a matrix of N rows of observations. Each
#  column is one simulation, derived from 1 errSeed.
getObs <- function(N,M,seed,errSeed) {
   apply(getErrs(N,errSeed),2,function(x) x + getX(N,M) %*%  matrix(data=getAlphas(M,seed),byrow=F,nrow= M + 1))
}

# returns (2^M-1 x M+1 matrix). Each row represents a combination
#   of parameters that will be in a model. For example, only the
#   first parameter y~1 will be c(1,0,0,0,0), and all of 5 parameters
#   would be c(1,1,1,1,1)
#   If M==3, then there are 3 parameters, plus the intercept, so we
#   get 4 columns, and there are 8 combinations of models.
getModelSet <- function(M) {
   cbind(rep(1,2^M),
   t(sapply(seq.int(0:(2^M - 1)),function(x){ as.integer(intToBits(x))[1:getM()]} ))[,1:M] 
   )
    
}

# reads a model v, and returns a formula for glm
#   eg aModel = c(1,0,1,1) 
#   returns "y ~ A + C + D + E"
createFormula <- function(aModel,X) {
  paste( " y ~",
         paste(colnames(X)[which(aModel == 1)],
               collapse = "+",
               sep = ""  ))
}


createFormulaSet <- function(m,X) {
  apply(m,1,FUN=createFormula,X)
}


applyModel <- function(forms,X,y) {
  #print(forms)
  lm(formula = forms,data=data.frame(X,y=y))
}

# apply a set of models to the data.
applyModels <- function(modelSet,X,y) {
  apply(t(modelSet),2,applyModel,X,y)
}

applyModels(t1[1:4],getX(N=getN(),M=getM()),
            y=getObs(getN(),getM(),getSeed(),errSeed=10))



 

 

 
 
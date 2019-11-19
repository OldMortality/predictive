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


z <- getX(getN(),getM())


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
  return(map(seeds,getErrByNSeed(N)) %>% errsAsMatrix(N))
}

# returns a matrix of N rows of observations. Each
#  column is one simulation, derived from 1 errSeed.
getObs <- function(N,M,seed,errSeed) {
   apply(getErrs(N,errSeed),2,function(x) x + getX(N,M) %*%  matrix(data=getAlphas(M,seed),byrow=F,nrow= M + 1))
}
xnam <- paste0("x", 1:3)
(fmla <- as.formula(paste("y ~ ", paste(xnam, collapse= "+"))))


# takes a matrix, and returns another matrix, with only
#   the rows which did not have a zero in the first column
#removeRowsWithZeroFirstColumnn <- function(M) {
#  M <- M[-which(M[,1] == 0),]
#}

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
M <- 2
print(getModelSet(3))

m <- getModelSet(getM())

# reads a model v, and returns a formula for glm
#   eg aModel = c(1,0,1,1) 
#   returns "y ~ A + C + D + E"
createFormula <- function(aModel) {
  paste( " y ~",
         paste(colnames(X)[which(aModel == 1)],
               collapse = "+",
               sep = ""  ))
}

createFormula(m[1,])

createFormulaSet <- function(m) {
  apply(m,1,FUN=createFormula)
}


m <- getModelSet(M = 4)
t1 <- createFormulaSet(m)


X <- getX(N,M) #%*%  matrix(data=getAlphas(M,seed),byrow=F,nrow= M + 1)
y <- getObs(getN(),getM(),getSeed(),10)

applyModel <- function(forms,X,y) {
  #print(forms)
  lm(formula = forms,data=data.frame(X,y=y))
}

#apply(t1[1:3],1,applyModel,X,y)
t1 <- createFormulaSet(m)

# apply a set of models to the data.
# returns a list of models.
applyModels <- function(modelSet,X,y) {
  apply(t(modelSet),2,applyModel,X,y)
}

applyModels(t1[1:4],X,y)
forms <- t1[1]

# dim(X)
# colnames(X) <- LETTERS[1:(getM()+1)] # works up to 26
# aModel <- c(1,1,0,1,1)
# 
# createModels <- function(X,Amodel) {
# 
# form <-  as.formula(paste( " y ~",
#                    paste(colnames(X)[which(aModel == 1)],
#                          collapse = "+",
#                           sep = ""  )))
# }
# 
# 
# form <- paste("y ~ A + B + C + D", sep=",")
# 
#lm(formula = form,data=data.frame(y=y,X))
# 
# lm(y ~ A + B + C + D,data=data.frame(X))
# 
# getSeed <- function() return(10)
# getNSim <- function() return(3)
# set.seed(getSeed())
# getObs(N=4,M=2,seed=getSeed(),errSeed=rnorm(getNSim()))






# test only
N=4
M=2
seed=10
errSeed=c(1,2,3,4)

# v1 is N-vector of the determinate part of the model
# l1 is list of errors, 1 list for each simulation, each list contains N values.
#    returns: vectors of v1 + l1, one for each simualation, making up column of matrix
addObsErr(v1,l1) {
  
}

getErrs(10,c(31,13))[[1]] 
getObs(10,3,seed=10,errSeed = c(31,13))

measurevar <- "y"
groupvars  <- c("x1","x2","x3")

# This creates the appropriate string:
paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ ")
#> [1] "y ~ x1 + x2 + x3"

# This returns the formula:
as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))

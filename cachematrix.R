## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## (there are also alternatives to matrix inversion that we will not discuss here).
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
  inverse <- NULL
  set <- function(Y){
    X <<- Y
    inverse <<- NULL
  }
  get <- function() X
  setinverse <- function(Inverse) inverse <<- Inverse
  getinverse <- function() inverse
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
cacheSolve <- function(X, ...) 
{  
  inverse <- X$getinverse()
  if(!is.null(inverse)){
    message("Inverse found in memory")
    return(inverse)
  }
  message("Matrix has changed calculating inverse...")
  data <- X$get()
  ## solve() only works with M*M matrices
  ## Check to see if it is a square matrix to use solve()
  if(dim(data)[1]==dim(data)[2]){
    message("This is a square matrix...")
    inverse <- solve(data, ...)
    X$setinverse(inverse)
    inverse
  }
  ## Here I am only using ginv() for M*N matrix.
  ## However, ginv() works with both M*N and M*M matrices  
  ## Use ginv() for rectangle matrix
  else{    
    library(MASS) ## Library required to use ginv()
    message("This is a rectangle matrix...")
    inverse <- ginv(data, ...)
    X$setinverse(inverse)
    inverse
  }
  
}


#Experiment to try if it works
#square matrix
X <- matrix(rpois(25,3), nrow = 5)
cX <- makeCacheMatrix(X)
cX$get()
cacheSolve(cX)
cacheSolve(cX)
invX <- cacheSolve(cX)

#Experiment to try if it works
#rectangular matrix rows > cols
Y <- matrix(rpois(20,2), nrow = 5, ncol = 4)
cY <- makeCacheMatrix(Y)
cY$get()
cacheSolve(cY)
cacheSolve(cY)
invY <- cacheSolve(cY)

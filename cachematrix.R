## Put comments here that give an overall description of what your
## functions do

## Helpers functions that cache the inverse of a matrix
## makeCacheMatrix:creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## inverse property
  mtx<-NULL
  
  ## Matrix set
  set<-function(y){
    x<<-y
    mtx<<-NULL
  }
  ## get the matrix
  get <- function() {
    ## Return the matrix
    x
  }
  ## set inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  ## get inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    mtx
  }
  ## list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve: calculates the inverse of the chached "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## matrix that is the inverse of 'x' input
        m <- x$getInverse()
        ## Return the inverse if its already set
        if( !is.null(m) ) {
          message("getting cached data")
          return(m)
        }
        ## Get the matrix from our object
        data <- x$get()
        ## Calculate the inverse using matrix multiplication
        m <- solve(data) %*% data
        ## Set the inverse to the object
        x$setInverse(m)
        ## Return the matrix
        m      
}

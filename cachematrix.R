## This program contains a pair of functions that cache the inverse of a matrix 

## The makeCacheMatrix function creates a special object (matrix) that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize inverse property
  
  inv <- NULL
  ## Set the matrix
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  ## Get the matrix
  get <- function(){
    x
  }
  
  ## Set inverse of matrix
  setInverse <- function(solveMatrix) {
    inv <<- solveMatrix
  }
  ## Get inverse of matrix
  getInverse <- function(){
    inv
  }
  
  ## Return list of methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special object (matrix) returned by the function makeCacheMatrix from above

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  ## Return the inverse if it is set 
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  ## Get matrix from the object
  data <- x$get()
  
  ## Calculate inverse using multiplication
  inv <- solve(data) %% data
  
  ## Set inverse of object
  x$setInverse(inv)
  
  ## Return matrix
  inv
}

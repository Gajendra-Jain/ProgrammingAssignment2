## Put comments here that give an overall description of what your
## functions do

## The below function creates a matrix.

makeCacheMatrix <- function(X = matrix()) {
  
  i <- NULL
  
  set <- function(y) {
    X <<- y
    i <<- NULL
  }
  get <- function() X
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)

}


## The below function returns the inverse of matrix and cache it.

cacheSolve <- function(X, ...) {
  
        ## Return a matrix that is the inverse of 'x'
    i <- X$getinverse()
  
      if(!is.null(i)){
      message("getting cached inverse matrix")
      return(i)
    }
    
    data <- X$get()
    
    ## Checking wether matrix is a square matrix or not
    if(!(ncol(data) ==nrow(data))){
      print("Matrix is not a square matrix")
      return()
    }
    
    i <- solve(data, ...) ##solving for the inverse of matrix
    X$setinverse(i)
    i                     ## returning the inverse of matrix
}

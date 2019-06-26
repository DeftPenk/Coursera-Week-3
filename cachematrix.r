## Functions that cache the inverse of a matrix

## A special matrix object is created that can cache its inverse
makeCacheMatrix <- function( t = matrix() ) {
  
  ## Initialize the inverse property
  i <- NULL
  
  set <- function( matrix ) {
    t <<- matrix
    i <<- NULL
  }
  
  get <- function() {
    ## Return the matrix
    t
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Compute the inverse of the special matrix 
cacheSolve <- function(x, ...) {
  
  t <- x$getInverse()
  
  if( !is.null(t) ) {
    message("getting cached data")
    return(t)
  }
  
  data <- x$get()
  
  t <- solve(data) %*% data
  
  x$setInverse(t)
  
  t
}

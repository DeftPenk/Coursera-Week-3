## Pair of functions that cache the inverse of a matrix

## Creates a special matrix object that can cache its inverse
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


## Compute inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated,
## then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return a matrix that is the inverse of 'x'
    t <- x$getInverse()

    ## Return the inverse if its already set
    if( !is.null(t) ) {
            message("getting cached data")
            return(t)
    }

    ## Get matrix from our object
    data <- x$get()

    ## Calculate inverse using matrix multiplication
    t <- solve(data) %*% data

    ## Set inverse to object
    x$setInverse(t)

    ## Returns matrix
    t
}

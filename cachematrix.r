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
## If the inverse has already been calculated,
## then the "cachesolve" should retrieve the inverse from the cache.
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

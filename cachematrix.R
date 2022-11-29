## A pair of functions that cache the inverse of a matrix


## This function creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
    i <- NULL
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }
    get <- function() {
        m
    }

    ## Method to set the inverse of the matrix and return inverse property
    setInverse <- function(inverse) {
        i <<- inverse
    }
    getInverse <- function() {
        i
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function calculates the mean of the special "vector" created with the above function
cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }
    data <- x$get()
    m <- solve(data) %*% data
    ## Set the inverse to the object
    x$setInverse(m)
    m
}

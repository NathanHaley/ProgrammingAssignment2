## Functions to create a matrix object and calculate and cache it's inverse.

## Matrix object creation and caching of inverse.

makeCacheMatrix <- function(x = matrix()) {
    x_inverted <- NULL
    set <- function(y) {
        x <<- y
        x_inverted <<- NULL
    }
    get <- function() x
    setInverse <- function(y) x_inverted <<- y
    getInverse <- function() x_inverted
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse= getInverse)
}

## Function to retrieve a matrix inverse from cache or compute and cache
## if not already cached.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    x_inverted <- x$getInverse()
    if(!is.null(x_inverted)) {
        message("getting cached data")
        return(x_inverted)
    }
    data <- x$get()
    x_inverted <- solve(data, ...)
    x$setInverse(x_inverted)
    x_inverted
}

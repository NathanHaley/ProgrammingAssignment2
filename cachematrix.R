## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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

## Write a short comment describing this function

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

## The functions contained in this file create and use a special matrix
## that can store its cached inverse to prevent the cost of repeatedly
## calling solve on the same matrix data.

## Creates a special matrix that can cache the results of its inversing
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Wraps solve with a call to a cached matrix's getinverse to return the
## cache value of solve instead of calculating the inverse everytime. If
## the inverse is not set, then it calculates the inverse and stores the
## value in the cached matrix.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

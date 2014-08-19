## This set of functions allows cached computations of 
## the inverse matrix for a given (invertible) matrix.

## The makeCacheMatrix function creates an object from a matrix
## which can be passed to the cacheSolve function in order to
## perform a cached computation of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y)  {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inv <<- i
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse)
}


## The cacheSolve function operates on objects created by 
## the makeCacheMatrix function. The first time the function
## is applied to an object, it will compute the inverse of the
## matrix and cache it. Subsequent applications to the same 
## object will just retrieve the inverse from the cache
## without recomputing it.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        data <- x$get()
        dims <- dim(data)
        inv <- tryCatch(solve(data),
                        error=function(cond) 
                            {
                            message("An error occured while computing the inverse")
                            message(cond)
                            message("\n")
                            return(NA)
                             })
        x$setInverse(inv)
        inv
}

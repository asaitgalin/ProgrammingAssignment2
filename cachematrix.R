# Inverse caching matrix wrapper
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL  # cached inverse
    
    # update inner matrix and reset cache
    setter <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getter <- function() {
        return(x)
    }
    # update cached inverse
    inverse_setter <- function(inv) {
        inverse <<- inv
    }
    inverse_getter <- function() {
        return(inverse)
    }
    
    return(list(set = setter, 
                get = getter,
                setinv = inverse_setter,
                getinv = inverse_getter))
}

# Get inverse of matrix (works with caching matrix wrapper)
# Returns cached inverse if found, otherwise solves for it and stores to cache
cacheSolve <- function(x, ...) {
    inverse <- x$getinv()
    
    # Is inverse cached?
    if (!is.null(inverse)) {
        message("getting from cache")
        return(inverse)
    }
    
    # Solve and update cache
    m <- x$get()
    inverse <- solve(m, ...)
    x$setinv(inverse)
    return(inverse)
}

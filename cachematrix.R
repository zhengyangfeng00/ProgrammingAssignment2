# This file defines two functions for solving a matrix with cache support.

# Returns a cachable object with set, get, set_inverse, get_inverse methods
makeCacheMatrix <- function(m = matrix()) {
    inverse_m <- NULL
    set <- function(rhs) {
        m <<- rhs
        inverse_m <<- NULL
    }
    get <- function() m
    set_inverse <- function(rhs) inverse_m <<- rhs
    get_inverse <- function() inverse_m
    list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse)
}

# Returns the inverse of a matrix.
# m is a cacheable matrix constructed by calling makeCacheMatrix
cacheSolve <- function(m, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse_m <- m$get_inverse()
    if (!is.null(inverse_m))
    {
        message("getting cached data")
        return(inverse_m)
    }
    data <- m$get()
    m$set_inverse( solve(data, ...) )
    m$get_inverse()
}

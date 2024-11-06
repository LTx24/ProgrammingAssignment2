## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize inverse as NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset inverse when the matrix is updated
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# This function computes the inverse of the special "matrix" created by makeCacheMatrix.
# If the inverse is already cached, it retrieves the inverse from the cache.
# Returns: A matrix that is the inverse of 'x'.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)  # Return the cached inverse if it exists
    }
    mat <- x$get()  # Get the matrix
    inv <- solve(mat, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse
    return(inv)  # Return the newly computed inverse
}






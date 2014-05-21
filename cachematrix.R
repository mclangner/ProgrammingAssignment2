## General Description
## makeCacheMatrix creates a function list that stores a matrix and intializes a variable for its inverse.
## cacheSolve checks to see if a matrix in a makeCachematrix function list has a calculated inverse.  If the 
## inverse does not exist, it is calculated, otherwise the stored inverse is returned.

## makeCacheMatrix
## If called with an input argument, create a function list with the input stored in x.  Otherwise generate
## a function list with an empty matrix x.  $set re-writes the matrix x, $get returns the matrix value.  $setinv
## sets the matrix inverse, and $getinv returns the cached inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve
## Checks a function list created with makeCacheMatrix and returns the matrix inverse if already cached
## Otherwise calculate, cache, and return the inverse matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

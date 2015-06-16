## makeCacheMatrix and cacheSolve work to cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
##              The object is really a list containing functions to:
##              1. set the value of the matrix
##              2. get the value of the matrix
##              3. set the value of the inverse
##              4. get the value of the inverse
## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cacheSolve should retrieve the inverse from the
## cache.


## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix. If the inverse has already been calculated (and the matrix
## has not changed), then the cachesolve should retrieve the inverse from the
## cache. Assumes the matrix is invertable.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

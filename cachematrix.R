## Write a pair of functions to cache the inverse of a matrix.

## makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get=get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes inverse of special matrix returned by makeCacheMatrix.
## If inverse is already calculated and matrix is not changed, then it returns the 
## inverse from cache.

cacheSolve <- function(x,...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("Getting cached inverse")
                return(m)
        }
        data <- x$get()
        m <- solve(data) 
        x$setinverse(m)
        m
}

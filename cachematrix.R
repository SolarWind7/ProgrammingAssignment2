## Functions allow working with cacheable matrix inverses.  An inverse for
## a given matrix is computed once and subsequent calls return cached inverse
## until original matrix in memory is reset to another value.

## Create and return a named list of functions assitiated with two matrix
## variables to allow setting and getting a source matrix and its inverse.
## A total of 4 functions is returned: get, set, getinverse and setinverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Using the object type such as that returned by the makeCacheMatrix function,
## attempts to retrieve the assiciated matrix inverse, and if it is null,
## computes one instead, caches both the matrix and itse inverse,
## and returns the inverse.

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

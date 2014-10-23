## Caching the inverse of a matrix
## These functions are for R Programming Assignment 2

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        n <- NULL
        set <- function(y) {
                x <<- y
                n <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) n <<- inverse
        getinverse <- function() n
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        matrix <- x$get()
        n <- solve(matrix, ...)
        x$setinverse(n)
        n
}

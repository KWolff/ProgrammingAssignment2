## Computing the inverse of a matrix can be a costly computation
## This program computes and caches the inverse of a matrix.
## This will prevent unnecessary repeated calculations of an inverse.

## This function creates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinv <- function(solve) m <<- solve
       getinv <- function() m
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## This function computes the inverse of the matrix object created 
## by the makeCacheMatrix function.

## If the inverse has already been calculated, this function will instead
## retreive the inverse from the cache.

cacheSolve <- function(x, ...) {
       m <- x$getinv()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       data <- x$get()
       m <- solve(data, ...)
       x$setinv(m)
       m
}
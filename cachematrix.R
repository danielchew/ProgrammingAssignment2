
## makeCacheMatrix take a matrix as an input and stores it.  Also creates lists of options so that
## the matrix can be retrieve and re-set.

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(mean) m <<- mean
     getInverse <- function() m
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve calculates prints out the inverse of the matrix.  If it's been previously
## solved, it'll just prints out the cache version.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}

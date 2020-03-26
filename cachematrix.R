## These functions solve and hold the value of an inverse matrix to ensure
## availability of the inverse matrix without having to redo the calculation
## every time it is needed.

## This function creates a matrix object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}


## This function checks to see if the inverse has already been solved.
## If it has, it returns that value. If not, it solves for the inverse.

cacheSolve <- function(x, ...) {
     
     m <- x$getinverse()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}

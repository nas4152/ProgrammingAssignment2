## These functions computes the inverse of a matrix.  
## Every time a new inverse is calculated it is stored or cached.
## Every time cacheSolve is used to return an inverse, it first checks
## if the inverse has been cached and only calculates the inverse
## if it cannot retrieve it from the cache.

## This function creates a list object that is used to cache a matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
          i <- NULL
          set <- function(y) {
                  x <<- y
                  i <<- NULL
          }
          get <- function () x
          setinverse <- function(inversem) i <<- inversem
          getinverse <- function() i
          list(set = set, get = get, setinverse = setinverse, 
                getinverse = getinverse)
}


## This function returns the inverse of a matrix using the list created
## by makeCacheMatrix().
## First it checks if an inverse has been cached and returns that value
## if present.
## If there is no cached inverse, it computes the inverse and stores it in the
## list.

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

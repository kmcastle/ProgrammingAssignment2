## makeCacheMatrix and cacheSolve is a pair of functions that calculates and 
## caches the inverse of a special matrix object.

## makeCacheMatrix is a function that creates a matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <-function(y){
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  }

## cacheSolve computes the inverse of the matrix object returned
## by makeCacheMatrix. If the inverse has already been calculated
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

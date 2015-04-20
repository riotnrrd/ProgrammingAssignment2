## makeCacheMatrix and cacheSolve implement a caching system so that a matrix
## inversion is cached once computed, and then retrieved in later steps if it
## has aleady been calculated

## makeCacheMatrix takes a matrix as input. it sets the matrix and its inverse
## as environment variables, as well as functions for set, get, setinverse, 
## and getinverse
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes as input a matrix created by makeCacheMatrix, above. It
## checks to see if the Matrix inverse has already been computed and stored,
## and if so it returns the inverse. If not, it computes, stores, and returns
## the inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  v <- x$get()
  i <- solve(v, ...)
  x$setinverse(i)
  i
}

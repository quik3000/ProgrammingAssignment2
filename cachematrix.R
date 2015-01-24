## Reference is taken from the example provided in assignment 2
## that is use to cache the mean of a numeric vector.

## Matrix inversion is usually a costly computation, hence it may
## make more sense to cache the inverse of a matrix rather than 
## compute it repeatedly.

## This R function is able to cache the inverse of a matrix by using
## makeCacheMatrix and cacheSolve


## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The cacheSolve function computes the inverse of the special "matrix"
## returned by makeCacheMatrix. If the inverse is already calculated
## and the matrix has not changed, then the cacheSolve will retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

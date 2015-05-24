## PART OF COURSERA PROGRAMMING ASSIGNMENT 2 - MANISH BHATTARAI
## -------
## Overall Description : The functions are meant to cache the computation of inverse of an matrix
## by storing values in a local environment.
## -------

## 'makeCacheMatrix creates a cacheable matrix from a given matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## 'cacheSolve' function returns the inverse of a given matrix, either from the local cache (if available) or by computation.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

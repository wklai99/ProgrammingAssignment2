## Matrix inversion may often involve time-consuming and costly computation
## To help shorten the repetitiveness, it may be beneficial to cache the inverse of a matrix as opposed to computing it over and over

## This function creates a matrix objects that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() {
    x
  }
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  getInverse <- function() {
    inv
  }
  list(set = set , get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse that is returned by the function "makeCacheMatrix"
## Checks to see if the inverse had already been calculated; if so, cacheSolve retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## The functions in this file can be used to compute, cache and retrieve
## the inverse of a matrix

## Creates a matrix object, which can contain its cached inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <<- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getInverse <- function() inverse
  setInverse <- function(inverseExternal) inverse <<- inverseExternal
  
  list(get = get, set = set, 
       getInverse = getInverse, setInverse = setInverse)
}


## Returns the inverse of a matrix from a cacheMatrix object if it is in cache or
## computes it and populates the cache
cacheSolve <- function(x, ...) {
  inverse <<- x$getInverse()
  if (is.null(inverse)) {
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    message("computed inverse and populated cache")
    inverse
  } 
  else
  {
    message("retrieved inverse from cache")
    inverse  
  }
}

## These two functions work together to create a matrix that caches its inverse
## The first function is creating the caching matrix while the second computes the
## inverse

## This function accepts a matrix and creates a list of functions to set and get the matrix
## as well as to set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) {
    inverse <<- inv
  }
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a "makeCacheMatrix" object. It accepts one such object
## and returns the inverse if it exists in the cache. If the cache is empty the inverse is computed,
## stored in the cache and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

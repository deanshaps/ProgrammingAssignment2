## Put comments here that give an overall description of what your
## functions do

## this function is a special matrix object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invMatrix <<- inverse
  getInverse <- function() invMatrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function will compute the inverse from the previous function makeCacheMatrix. If the inverse has already been calculated
## and the matrix didn't change, then it retrieves the inverse from cache

cacheSolve <- function(x, ...) {
  invMatrix <- x$getInverse()
  if (!is.null(invMatrix)) {
    message("loading cache...")
    return(invMatrix)
  }
  mat <- x$get()
  invMatrix <- solve(mat, ...)
  x$setInverse(invMatrix)
  invMatrix
}

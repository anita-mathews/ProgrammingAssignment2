## These functions allow the inverse of a matrix to
## be calculated or retrieved from cache if previously
## calculated.

## makeCacheMatrix takes an invertible matrix as input
## and returns a list of functions that can get or set
## the matrix itself or its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
  
}


## cacheSolve takes a cacheMatrix object as input.
## It calculates the inverse of the matrix if not previously
## calculated, stores it in cache and returns the inverse.
## If the inverse has already been calculated, the cached
## inverse is returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("returning cached inverse matrix")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

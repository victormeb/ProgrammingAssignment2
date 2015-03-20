## Put comments here that give an overall description of what your
## functions do

## This function creates a 'vector' (actually a list)
## to save a matrix and retrieve it.
## It also saves and retrieves its inverse.

makeCacheMatrix <- function(x = numeric()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInvX <- function(invX) inv <<- invX
  getInvX <- function() inv
  list(set = set, get = get,
       setInvX = setInvX,
       getInvX = getInvX)
}


## Return a matrix that is the inverse of 'x'
## If it already exists, it gets the cached inverse,
## this saves the effort to recalculate it.
## Otherwise, it calculates it.

cacheSolve <- function(x, ...) {
  invX <- x$getInv()
  if(!is.null(invX)) {
    message("getting cached inverse")
    return(invX)
  }
  xMatrix <- x$get()
  invX <- solve(xMatrix, ...)
  x$setInvX(invX)
  invX
}

## Purpose of this assignment is to write a pair of functions that cache the inverse of a matrix
## rather than computing them repeatedly

##The following function "makeCacheMatrix" creates a special "matrix" object 
##that can cache its inverse
makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(matrix) m <<- matrix
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}

## This function "cacheSolve" computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated, 
## then the cachesolve will retrieve the inverse
## from the cache.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## Inverse of a square matrix has been computed here with the solve() function. 
  ## if "data" is a square invertible matrix, then solve(data) returns its inverse.
  m <- solve(data, ...)
  x$setmatrix(m)
  m ## inverse matrix is returned
}

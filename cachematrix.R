## Matrix inversion is usually a costly computation and there is some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. This assignment is to write a pair of functions 
## that cache the inverse of a matrix

## makeCacheMatrix: This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setoriginal = setoriginal,
       getoriginal = getoriginal,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## retrieves the inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## if the matrix has not changed
  
  m <- x$getsolve()
  original <- x$getoriginal()
  if(!is.null(m) && !is.null(original) && x == original) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  x$setoriginal(x)
  m
}

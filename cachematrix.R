##
## R Programming Week 3, Programming Assignment 2: Lexical Scoping
##
## Author: Patrick (Pat) Tomsula
##
## Included functions:
##   makeCacheMatrix
##   cacheSolve
##


##
## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse.
##
## Uses the R '<<-' operator to assign a value to an object in an environment
## that is different from the current environment. This provides the cache.
##
## Args:
##   x: an invertible matrix, defaults to NA
##
## Returns:
##   A list containing functions that do the following:
##     - set the matrix, set
##     - get the matrix, get
##     - set the matrix inverse, setInverse
##     - get the matrix inverse, getInverse
##
makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL
  
  # Function to set the matrix.
  set <- function(y) {
    # use '<<-' to cache the value for subsequent calls.
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix.
  get <- function() x

  # Function to set the matrix inverse.
  setinv <- function(inverse) inv <<- inverse
  
  # Function to get the matrix inverse.
  getinv <- function() inv
  
  # Return the list of functions.
  list(set = set, get = get,
       setinv = setinv, getinv = getinv)
}


##
## The cacheSolve function returns a matrix that is the inverse of
## the supplied objects matrix.
##
## Args:
##   x: Output of makeCacheMatrix().
##
## Returns:
##   The inverse of the original matrix input to makeCacheMatrix().
##
cacheSolve <- function(x, ...) {

  # Get the inverse from the supplied argument
  inv <- x$getinv()
  
  # If the inverse has been calculated the return it.
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # Otherwise, get the matrix from the supplied object,
  # calculate the inverse, and set the inverse in the object.
  mdata <- x$get()
  inv <- solve(mdata, ...)
  x$setinv(inv)
  
  # And return the inverse.
  inv
}

##
## R Programming Week 3, Unit Test for Programming Assignment 2: Lexical Scoping
##
## Author: Patrick Tomsula
##
## Included functions:
##   test_cachematrix
##


## Basic unit test for 'cachematrix.R'.
##
## One iteration test that akes a supplied matrix and calls 'makeCacheMatrix'.
## Then calls 'cacheSolve' obtaining the call duration. Then calls 'cacheSolve'
## again obtaining the function duration. The second call should be of shorter
## duration for a non-trivial matrix.
##
## Usage:
##   > set.seed(<seed value>)
##   > r = rnorm(<matrix elements>)  # a square value such as 10000, 64*1024, 1000000, etc.
##   > testmatrix = matrix(r, nrow=sqrt(<matrix elements>), ncol=sqrt(<matrix elements>))
##   > print(times <- test_cachematrix(testmatrix))
##
## Args:
##   a_matrix: an invertible matrix.
##
## Returns:
##   A list containing the first and second call durations.
##

test_cachematrix <- function(a_matrix = matrix()) {

  # Make sure we have access to the function(s) to test.
  source("cachematrix.R")

  # Get the matrix functions.
  cachetest = makeCacheMatrix(a_matrix)
  
  # Test the first time which populates the cache.
  inittime <- system.time(cacheSolve(cachetest))
  initinv <- cachetest$getinv()

  # Test the second time which should just return the inversefrom
  # the cache.
  cachetime <- system.time(cacheSolve(cachetest))
  cacheinv <- cachetest$getinv()
  
  if (identical(initinv, cacheinv)) {
    message("INFO:  test_cachematrix: init and cached matrices identical")
  }
  else {
    message("ERROR: test_cachematrix: init and cached matrices NOT identical")
  }
  
  list("initialization time:" = inittime, "cached access time:" = cachetime)
}
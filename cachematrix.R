## OVERALL DESCRIPTION
## R programming
## Assignment 2 - caching the inverse of a matrix
## This script defines a pair of functions that operate together
## to calculate and cache the inverse of a matrix and return the
## inverse for any given matrix.

## Description of the makeCacheMatrix function
## This function creates a special matrix object that can cache
## its inverse. The function defines other functions and creates
## a list of these other functions.

makeCacheMatrix <- function(x = matrix()) {
  inmx <- NULL
  set <- function(y) {
    x <<- y
    inmx <<- NULL
  }
  get <- function() x
  setinvmatrix <- function(invmatrix) inmx <<- invmatrix
  getinvmatrix <- function() inmx
  list(set = set, get = get,
       setinvmatrix = setinvmatrix,
       getinvmatrix = getinvmatrix)
}
## Description of the cacheSolve function
## This function computes the inverse of the special matrix
## returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inmx <- x$getinvmatrix()
  if(!is.null(inmx)) {
    message("getting cached inverse matrix")
    return(inmx)
  }
  data <- x$get()
  inmx <- solve(data, ...)
  x$setinvmatrix(inmx)
  inmx
}

## This script has been tested on a square matrix defined as
## matrix(c(4,2,7,6), nrow = 2, ncol = 2) and successfully calculated
## and cached its inverse of matrix(c(0.6,-0.2,-0.7,0.4), nrow = 2, ncol = 2)
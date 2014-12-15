## This file contains a pair of functions that when used 
## together save the inverse of a matrix in a cache, so that
## the inverse does not have to be computed more than once

## makeCacheMatrix creates a special "matrix" object that 
## can cache its inverse
## Takes a matrix and returns a list containing the
## set, get, setinverse, and getinverse functions
makeCacheMatrix <- function(specialMatrix = matrix()) {
  
     inverse <- NULL  ## initialize the inverse
     
     ## set can be use to initialize the value of the matrix
     ## Uses the variables from the outer environment
     set <- function(m) {
          specialMatrix <<- m
          inverse <<- NULL
     }
     
     ## get simply returns the matrix value
     get <- function() specialMatrix
     
     ## setInverse saves the value of the inverse,
     ## Uses the variable inverse from the outer environment
     setInverse <- function(inv) inverse <<- inv
     
     ## getInverse returns the value of the saved inverse
     getInverse <- function() inverse
     
     ## create the list that holds the set and get functions
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}


## cacheSolve checks to see if the inverse was already
## calculated and saved. If it was, the cached value is
## returned, otherwise it calculates the inverse and 
## stores the result
## Takes a matrix and returns the inverse of the matrix
cacheSolve <- function(specialMatrix, ...) {
  
  inverse <- specialMatrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  matrixData <- specialMatrix$get()  ## get the matrix data
  inverse <- solve(matrixData, ...)  ## calculate inverse
  specialMatrix$setInverse(inverse)  ## store result
  inverse
}

## Here creates a set of functions. One take a square matrix as 
## input and store its inverse matrix calculated by another  
## function if the inverse exists.
##
## This function takes matrix as input and return 4 methods as a 
## list to read or write the origin matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  setMatrix <- function(y = matrix()) {
    x <<- y
    inve <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(y) inve <<- y
  getInverse <- function() inve
  list(setMa=setMatrix, getMa=getMatrix, setIn=setInverse, getIn=getInverse)
}


## This function takes the above as input, reset its inverse or 
## reports error if no inverse exist. You can Use rref() in package 
## "pracma" to check the rank of matrix.

cacheSolve <- function(x, reset=FALSE) {
  if (!is.null(x$getIn()) && !reset) {
    message('Inverse already exists, set "reset=TRUE" to recalculate')
    return(x$getIn())
  }
    x$setIn(solve(x$getMa()))
    x$getIn()
}
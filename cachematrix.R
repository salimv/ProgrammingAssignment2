## Following functions are efficient matrix inverse function
## Inverse computation of a matrix is an expensive operation and could take time
## for large matrices. The following two functions make this process efficient
## by calculating the inverse function using standard  solve() function only the
## first time, and then cache the result. As long as the matrix remains the same
## the inverse function will return the cached result

## maCacheMatrix prepares the matrix by creating 4 special functions associated 
## with the matrix - get, set, getsolve and setsolve. 
## set and get functions set or return the matrix to be operated on
## getinverse and setinverse get and set the inverse matrix solution for the 
## given matrix. This is where the cached value is stored.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse to NULL
  i <- NULL
  
  ## Function to set a new matrix. When this happes, clear the inverse so that
  ## it is computed the next time the cacheSolve is called
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  ## Return the current matrix
  get <- function () x
  
  ## Sets a new computed inverse matrix
  setinverse <- function(inv) {
    i <<- inv
  }
  
  ## Return the current inverse matrix
  getinverse <- function() i
  
  ## Make a list of the get/set functions and return to be used by the 
  ## special matrix object
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function works with the special matrix cache matrix created by makeCacheMatrix
## function. It checks if the matrix already has in inverse. If so, it returns
## the cached inverse. Else, it computes the inverse, sets it with the cached
## matrix object and returns the inverse

cacheSolve <- function(x, ...) {
  ## Get the current value of inverse
  inv <- x$getinverse()
  
  ## If inverse matrix exists, then return that matrix
  if (!is.null(inv)) {
    message("Returning cached inverse matrix")
    return (inv)
  }
  
  ## inverse does not exist. So get the original matrix and compute inverse
  mat <- x$get()
  inv <- solve(mat)
  
  ## cache the result
  x$setinverse(inv)
  
  ## return the result
  inv
}
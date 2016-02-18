## The functions allow the calculation of inverses of matrices and storage of calculated inverses
## in order to avoid unnecessary calculation.

## Write a short comment describing this function
## This function creates a list of matrix objects that includes the "focus" matrix and 
## its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  getData <- function() x
  setInv <- function(val) inv <<- val
  getInv <- function() inv   
    
  cache <- list(getData = getData,
                setInv = setInv,
                getInv = getInv)
  return(cache)
}


## This function caclculates the inverse of the "focus" matrix, but first checks for 
## a cached inverse. If the cache exists, then that cached value is returned instead
## Input is not regular matrix, but output of previous function - list of Matrix etc
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if (!is.null(inv)) {
    return(inv)
  }
  
  inv <- solve(x$getData())
  x$setInv(inv)
  return(inv)
}

## The function processes a matrix 
## such that it can be input to the 2nd function to check or solve 

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) x_inv <<- solve
  getinv <- function() x_inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## This function calculates the inverse of a processed matrix processed by the 1st function. 
## If identical operation was done before, it "get"s the inverse and returns. 
## Else, it "solve"s for the inverse and caches it with "setinv"

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
  
}

## Week3 HW - Caching the Inverse of a Matrix cache.r
# Objective: Write a pair of functions that cache the inverse of a matrix.
# Date: 03/12/2021

## Define function makeCacheMatrix which creates a special "matrix" object that can 
# cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  
  # set value of i to null  
  i <- NULL
  
  # create a function 'set' that sets object x to undefined variable y. Set i = null. 
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  
  # define 'get' function, which shows the value of x 
  get <- function() x
  
  # define 'setinv' which calculates the inverse of x 
  setinv <- function(inverse) i <<- inverse
  
  # define 'getinv' function, which fetches the inverse matrix x 
  getinv <- function() i
  
  
  # assign to a list 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##  Define function which computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
  
  # Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

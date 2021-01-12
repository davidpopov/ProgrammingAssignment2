## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function makes a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL      #initializes invert and x
  set <- function(y) 
  {
    x <<- y           #assigns value of y to x
    invert <<- NULL   #assigns value NULL to invert
  }
  
  get <- function() x #retrieves x
  setinv <- function(inverse) invert <<- inverse #assigns value to invert
  getinv <- function() invert                    #retrieves invert from the parent
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  ##assigns functions that are above as an element within a list()
}


## Write a short comment describing this function
##This function makes the inverse of the matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invert <- x$getinv()
  ## if the inverse has been calculated
  if(!is.null(invert))
  {
    ## get it from the cache 
    return(invert)
  }
  
  # if the inverse has not been calculated, calculates the inverse 
  data <- x$get()
  invert <- solve(data, ...)
  ## sets the value of the inverse in the cache
  x$setinv(invert)
  invert          
}

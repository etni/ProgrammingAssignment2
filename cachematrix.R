## Assignment 2:  Caching the Inverse of a Matrix
## By Etni Alvis - Coursera - R Language - July 21, 2015
## 
## The following functions will get the inverse (solving) of a Matrix
## but we'll cache the results so that subsequent requests return faster

## create a cacheable matrix solver function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  
  # reset the cached data 
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  
  get <- function() x
  #saves the solution to the current env space
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  
  #returns object functions
  list(set =set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

# solve a matrix with caching
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## attempt to get cached data
  inv <- x$getInverse()
  ## display message if we found cached data
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## no cached data, let's actually do some work
  data <- x$get()  # get input matrix
  inv <- solve(data, ...) # solve
  x$setInverse(inv) ## save to cache
  inv  
}

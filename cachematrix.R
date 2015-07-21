## Assignment 2:  Caching the Inverse of a Matrix
## By Etni Alvis - Coursera - R Language - July 21, 2015
## 
## The following functions will get the inverse (solving) of a Matrix
## but we'll cache the results so that subsequent requests return faster

## create a cacheable matrix solver function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL 
  }
  
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set =set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

# solve a matrix with caching
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}

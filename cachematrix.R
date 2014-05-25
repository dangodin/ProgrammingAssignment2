## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  # Define new matrix values, wipe out inverse from cache
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  # Retrieve matrix values
  get <- function() x
  
  # Set inverse of matrix in cache
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  # Get inverse from cache
  getinverse <- function() {
    return(i)
  }
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above. If the inverse has already been calculated (and the
# matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
  
  # Check if matrix inverse is already cached
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return (i)
  }

  # Calculate inverse and store in cache
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  return (i)
}

#1 makeCacheMatrix: this function creates a special "matrix"object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  # sets x equal to an empty matrix
  In <- NULL
  # set the inverse to null
  set <- function(y) {
    x <<- y
    # set the value of x to a function
    In <<- NULL
    # re-set the inverse to null
  }
  get <- function() x
  # get the value of the matrix
  setInverse <- function(solve) In <<- solve
  # set the inverse of the matrix
  getInverse <- function() In
  # returns the value
  list(set = set, get = get, setInverse = setInverse      , getInverse = getInverse)
  # creates a list of all functions
}

# 2 cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  In <- x$getInverse()
  if(!is.null(In)) {
    message("getting cached data")
    return(In)
    # if the inverse is not null, then the inverse that is already calculated is obtained. And the value is returned.
  }
  data <- x$get()
  # If the inverse is null, then the inverse is calculated
  In <- solve(data, ...)
  x$setInverse(In)
  # set the inverse to the computed value
  In
}

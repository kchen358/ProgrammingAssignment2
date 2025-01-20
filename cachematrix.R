## The following are a pair of functions that cache the inverse of a matrix.
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## cacheSolve computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated and the matrix is unchanged,
## cacheSolve retrieves the inverse from the cache.


## Create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize inverse property
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset inverse when matrix is updated
  }

  get <- function() x

  # Set and get inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Retrieve the cached inverse if available

  # If the inverse is already cached, return it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }

  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute the inverse
  x$setInverse(inv)       # Cache the inverse

  inv ## Return a matrix that is the inverse of 'x'
}

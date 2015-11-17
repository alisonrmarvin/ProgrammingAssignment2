# This pair of functions cache the inverse of a matrix.

# This function creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # step 0 - initialize
  inv <- NULL
  # step 1 - set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # step 2 - get the value of the matrix
  get <- function() x   
  # step 3 - set the value of the matrix inverse
  setinverse <- function(inverse) inv <<- inverse
  # step 4 - get the value of the matrix inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)         
}

# This function returns the inverse of the matrix. It first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. 
# If not, it computes the inverse, sets the value in the cache via setinverse function.
# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}




# Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
# This pair of function allow the user to input a matrix and calculate its inverse then cache the result.
# Doing so make retrieving the inverse of the matrix fast.


# The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
# 
# set the value of the matix
# get the value of the matrix
# set the value of the inverse of the matrix
# get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setInverse <- function(userMatrix) inverseMatrix <<- userMatrix
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# The following function calculates the Inverse of the special "vector" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
  
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached data")
    return(inverseMatrix)
  }
  message("First time computing Inverse (now stored in cache)")
  data <- x$get()
  inverseMatrix <- solve(data, ...)
  x$setInverse(inverseMatrix)
  inverseMatrix
        ## Return a matrix that is the inverse of 'x'
}

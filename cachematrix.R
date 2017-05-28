## This is the 2nd programming assignment in Week 3 
##
## Matrix inversion is usually a costly computation
## there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 
##
## The following functions make a special matrix object
## which caches its inverse.
##
##
## The 1st function, makeCacheMatrix creates a special " Matrix " 
##  containing a function to
# set the value of the Matrix
# get the value of the Matrix
# set the value of the Inverse
# get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y			# set the value of the Matrix
    m <<- NULL		# clear the cache
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse	# set the value of Inverse
  getInverse <- function() m	# get the value of Inverse
  list(set = set, get = get,		# return a list with above four functions
       setInverse = setInverse,
       getInverse = getInverse)
}

## The 2nd function below, cacheSolve calculates the Inverse of 
## the special "matrix" created with the 1st function.
# It first checks to see if the Inverse has already been calculated
# If so, it gets the inverse from the cache and skips the computation
# Otherwise, it calculates the Inverse of the matrix and 
# sets the value of the Inverse in the cache via the setInverse function.

cacheSolve  <- function(x) {
  m <- x$getInverse()	# fetches cached value for inverse
  if(!is.null(m)) {		# checks if inverse was already calculated
    message("getting cached data")
    return(m)
  }
  data <- x$get()	 # Otherwise, it calculates Inverse of matrix and
  m <- solve(data) 	 # sets the value of Inverse in cache
  x$setInverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}


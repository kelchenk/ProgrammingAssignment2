## Put comments here that give an overall description of what your
## functions do
#These functions create and call a matrix and its inverse in/from cache

## Write a short comment describing this function
#This function creates a matrix and stores it in cache then calls the matrix
#This also does the same thing for the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
#This function determines if the inverse of the matrix created above has already been
#calculated.
#If the inverse has already been calculated, it pulls it from the cache.
#If it has not already been calculated, it calculates the inverse and stores it in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("Cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}

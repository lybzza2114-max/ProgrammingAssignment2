## Put comments here that give an overall description of what your
## functions do
## These functions create a special matrix object that can cache its inverse
## and compute the inverse efficiently. If the inverse has already been
## calculated, the cached value is returned instead of recomputing.

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse is already cached, it retrieves it to save computation time.
cacheSolve <- function(x, ...) {
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

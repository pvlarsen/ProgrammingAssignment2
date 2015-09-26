## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.  The following functions makeCacheMatrix and
## cacheSolve are provided to implement caching of the inverse of a matrix
## to solve the problem.
##
##  makeCacheMatrix - Creates a special "matrix" object that can
##  cache its inverse
##
makeCacheMatrix <- function(x = matrix()) {
  # Null the cached matrix until such time that someone 'set's it
  myCachedMatrix <- NULL
  
  # Set function
  set <- function(inValue) {
    x <<- inValue
    myCachedMatrix <<- NULL
  }
  # Get function (return the localValue)
  get <- function()
    x
  # Set myCachedMatrix to be the inverse
  setInverse <- function(inverse)
    myCachedMatrix <<- inverse
  getInverse <- function()
    myCachedMatrix
  list(
    set = set, get = get, setInverse = setInverse,getInverse = getInverse
  )
}

## cacheSolve - Computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.
##
cacheSolve <- function(x, ...) {
  myCachedMatrix <- x$getInverse()
  if (!is.null(myCachedMatrix)) {
    message("getting cached data")
    return(myCachedMatrix)
  }
  matdata <- x$get()
  myCachedMatrix <- solve(matdata, ...)
  x$setInverse(myCachedMatrix)
  message("created cached data")
  myCachedMatrix
}

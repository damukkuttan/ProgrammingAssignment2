#makeCacheMatrix creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) invX <<- solve
  getInverse <- function() invX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#cacheSolve calculates the inverse of the special "matrix" created with the 
#above function. First it first checks to see if the inverse has already been 
#calculated. If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data and sets the value of the
#inverse in the cache via the setInverse function
cacheSolve <- function(x, ...) {
        invX <- x$getInverse()
        if(!is.null(invX)) {
          message("getting cached data")
          return(invX)
        }
        data <- x$get()
        invX <- solve(x, ...)
        x$setInverse(invX)
        invX
}

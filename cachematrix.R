## Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse_Matrix <- NULL
  set <- function(y) {
    x <<- y
    inverse_Matrix <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse_Matrix <<- inv
  getInverse <- function() inverse_Matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix function 
## above. If the inverse has already been calculated (and the matrix hasn't changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse_Matrix <- x$getInverse()
  if(!is.null(inverse_Matrix)) {
    message("getting cached data")
    return(inverse_Matrix)
  }
  data <- x$get()
  inverse_Matrix <- solve(data, ...)
  x$setInverse(inverse_Matrix)
  inverse_Matrix
}

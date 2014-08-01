## Computing and caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setInv <- function(ax_inv) x_inv <<- ax_inv
  getInv <- function() x_inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    message("gettign cached data")
    return(inverse)
  }
  matrix <- x$get()
  inverse = solve(matrix)
  x$setInv(inverse)
  inverse
}
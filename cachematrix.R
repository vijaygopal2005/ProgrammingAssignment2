## This script calculates the inverse of a matrix from the cache. If the matrix 
## is not present in the cache it creates a matrix and caches it.

## This function creates the matrix and caches it.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  ## setting the value of the matrix
  set <- function(s) {
    m <<- s
    i <<- NULL
  }
  ## getting the value of the matrix
  get <- function()
    m
  ## setting the value of the inverse matrix
  setInverse <- function(inv)
    i <<- inv
  ## getting the value of the inverse matrix
  getInverse <- function()
    i
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## This function checks if the matrix is cached and if not 
##will calculate the inverse of the matrix

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

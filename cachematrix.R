## Justin Stayrook (stayrookjj@gmail.com)

## The following functions create the ability to cache the inverse of a matrix
## that is created using the solve() function

## makeCacheMatrix
## this function takes in a square matrix that is non-singular and creates
## cache access methods for setting and getting the inverse matrix
## return: list with functions to get and set input matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve
## this function takes in the list returned from the 'makeCacheMatrix' method
## and then solves the matrix
## return: if the matrix has not been previously solved, it is solved and sent 
## to cache.  if the matrix was previously solved, it is returned from cache with
## a notification of 'getting cached data'

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

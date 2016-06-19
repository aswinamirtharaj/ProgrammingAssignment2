## We are caching the result of a matrix inverse to avoid recomputing everytime and 
## resue the value by retrieving from cache.
## makeCacheMatrix -- take a matrix as input and returns the get and set in a list
## cacheSolve -- take the value returned by makeCacheMatrix and calculates the inverse matrix 
## for first time and stores in cache when called. From second time it uses the value stored in cache.

## Storing the matrix and setter, getter: retuning a list

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculates the inverse of the matrix first time and stores in cache and later retrieves

cacheSolve <- function(x, ...) {
       m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m ## Return a matrix that is the inverse of 'x'
}

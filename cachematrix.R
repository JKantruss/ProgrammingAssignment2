## This function stores a list of four functions
## including set, get, setinverse, and getinverse
## set and get simply set and retrieve the value of a matrix, respectively
## where setinverse and getinverse do the same for the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}
  
##  The following function calculates the inverse of the matrix
##  created with the above function. However, if the inverse has already 
##  been calculated and stored it get`s the inverse from the
##  cache and skips the computation. Otherwise, it calculates the inverse of
##  the matrix and sets the value of the inverse in the cache via the `setinverse`
##  function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}



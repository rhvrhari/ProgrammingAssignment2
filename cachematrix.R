## R program to cache the results of a matrix inverse operation

## makeCacheMatrix function sets up the matrix, gets the value of the matrix, inverses the matrix
## and gets the value of an inverted matrix (using the solve function)

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

## cacheSolve function checks whether the result of a matrix inverse operation is already cached and
## returns it without performing the matrix inverse operation again. Else, it performs the inverse 
## operation afresh and caches it

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

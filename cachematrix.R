## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function just check if the matrix is square, the rest is the same as the example function

makeCacheMatrix <- function(x = matrix()) {
  if (nrow(x)!=ncol(x)) {
    print("! the matrix is not square")
    return()
  }
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


## Write a short comment describing this function
## the only difference with cacheMean is that m solve data

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


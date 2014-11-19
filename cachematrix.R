
## this function is a tool to cache the inverse of a matrix, it creates four methods to set,get a matrix
## or to set,get the inverse of this matrix. It stores m which will be the inverse matrix

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


## this function return the inverse of a matrix. It checks first
##if it has been already calculate and store within makeCacheMAtrix or it calculates it

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


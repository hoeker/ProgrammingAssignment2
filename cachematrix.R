## Put comments here that give an overall description of what your
## functions do

## Returns a CacheMatrix object--a list containing:
##   set(x)       -- a function to set the matrix
##   get()        -- a function to get the matrix
##   setinverse() -- a function to set the inverse of the matrix
##   getinverse() -- a function to get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setinverse <- function(inverse) {
    i <<- inverse
  }
  
  getinverse <- function() {
    i
  }
  
  list(set=set, get=get,
       setinverse=setinverse, getinverse=getinverse)
}


## Solves for the inverse of a CacheMatrix object.
## This function caches the result and return the cached
## result if called multiple times.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}

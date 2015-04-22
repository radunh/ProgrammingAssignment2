## The following functions (makeCacheMatrix and cachesolve) 
## will work together to allow for fast retrieval of an
## inverse matrix

## makeCacheMatrix will build a matrix that is capable of creating
## it's own inverse and cache it

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize in scope vars 
  inv <- NULL
  
  ## build your setters and getters for the matrix and it's invers
  set <- function(y) {
    ## init your out of scope (global) vars
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  ## instantiate setinverse and getinvers
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve will return the inverse of a matrix
## if the matrix is in cache already, it will return it
## otherwise it will build it, cache it, and return it

cacheSolve <- function(x, ...) {

  ## get the inverse from cache
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  ## if no cache, create the matrix, inverse it, and copy to cache
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv  
}

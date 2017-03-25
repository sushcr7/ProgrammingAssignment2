## setMatrix      set the value of a matrix
## getMatrix      get the value of a matrix
## cacheInverse   create an inverse of the matrix
## getInverse     get the cached value (inverse of the matrix)

## creates a matrix and calculates its inverse and outputs a list

makeCacheMatrix <- function(x = matrix()) {
  c<-NULL
  setMatrix <- function(y) {              
    x <<- y
    c <<- NULL
  }
  getMatrix <- function(){
    x
  }
  CacheInverse <- function(solve) c <<- solve
  getInverse <- function() c
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, CacheInverse = CacheInverse, 
       getInverse = getInverse)
}


## creates an inverse of the special matrix

cacheSolve <- function(x, ...) {
  c <- x$getInverse()
  if(!is.null(c)) {
    message("getting cached data")
    return(c)
  }
  data <- x$getMatrix()
  c <- solve(data, ...)
  x$CacheInverse(x)
  c
}

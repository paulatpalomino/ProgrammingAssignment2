## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a matrix object that can cache its own inverse.

rm(list = ls())

makeCacheMatrix <- function(ma = matrix()) {
  im <- NULL
  setMatrix <- function(y) {
    ma <<- y
    im <<- NULL
  }
  getMatrix <- function() ma
  setinverse <- function(inv) im <<- inv
  getinverse <- function() im
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the matrix returned by the first function. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  im <- x$getinverse()
  if (!is.null(im)) {
    message("get cached inverse matrix")
    return(im)
  }
  data <- x$getMatrix()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
        ## Return a matrix that is the inverse of 'M'
}
M <- matrix(c(1,2,3,4),2,2)

M1 <- makeCacheMatrix(M)
cacheSolve(M1) #inverse returned after computation with no message

cacheSolve(M1) #inverse returned from cache with message printed here

M2 <- makeCacheMatrix(-M)
cacheSolve(M1)
cacheSolve(M2)

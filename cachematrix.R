
## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather 
## than computing it repeatedly.The below two functions are used to 
## create a special object that stores a matrix and caches its inverse.

##makeCacheMatrix creates a "special" matrix object that can cache its inverse by creating 
## a list of functions for interacting with the matrix.
## The complete set of functions are:

## Set: Creates a cache of the matrix upon the call of makeCacheMatrix 
## and clears any previous cached inverse matrix.
## Get: Returns the cached matrix.
## Setinv : Creates a cache of the solved inverse of the orignal cached matrix
## created by set.
## Getinv Returns the cached inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve function solves the inverse of the cached smatrix 
## created by makeCacheMatrix. If the inverse of the cached matrix 
## has already been solved, returns the cached version of the 
## inverse matrix. Otherwise, solves the inverse matrix and stores it 
## in the cache and returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
s
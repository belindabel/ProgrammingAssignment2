## Functions to create a "matrix" object that can cache its inverse,
## and then retrieve the inverse from the cache

## makeCacheMatrix is a function that takes a matrix A and defines 
## four further functions: 
##        -set, which initialises the values in a 
##        nested environment; 
##        -get, which retrieves the matrix A; 
##        -setInv, which describes how to find the inverse
##        -getInv, which retrieves the inverse
## It returns a list of the four functions and their details, 
## including where they would be found.

makeCacheMatrix<-function(A){
  inv <- NULL
  set <- function(y) {
    A <<- y
    inv <<- NULL
  }
  get <- function() A
  setInv <- function(solve)  inv <<- solve(A,diag(nrow(A)))
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## cacheSolve uses the nested functions defined in the above 
## makeCacheMatrix to return an inverse from the nested environment
## where it is cached. If it does not exist in the cache, then it
## is computed from the matrix A and the functions get and setInv.

cacheSolve<-function(A, ...) {
  inv <- A$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- A$get()
  inv <- solve(data,diag(nrow(data)))
  A$setInv(inv)
  inv
}

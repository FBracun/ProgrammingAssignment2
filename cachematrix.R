## R programming - Course Programming Assignment 2: Lexical Scoping
## Franc Braèun
library(Matrix) # Needed for calculating matrix rank.


## Below are two functions (i.e., "makeCacheMatrix" and "cacheSolve") that are used 
## to create a special object that stores a numeric matrix and caches the 
## inverse of a matrix. 
## 
## Use example:
##                invM <- cacheSolve(makeCacheMatrix(M))
##
##                Note that a matrix M has to be a square non-singular matrix.
##



## The function "makeCacheMatrix" creates and returns a special "matrix".
## This "matrix" is really a list containing a function to:
## 
##   1. set the value of the matrix
##   2. get the value of the matrix
##   3. set the value of the inverse
##   4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  #If the matrix is not square the function is terminated.
  if (nrow(x)!=ncol(x)) stop("The matrix is not square and it is not invertible!")
  
  # If the rank of matrix is < number of columns the function is terminated. 
  # Note that in this case the matrix is singular and consequently 
  # it can not be calculated with "solve"  function.
  if (rankMatrix(x) < ncol(x)) stop("Singular matrix! - the matrix rank < number of columns.")
  
  # initialize the stored inverse value to NULL
  inverse <- NULL
  
  # Used to set the value of a matrix.
  set <- function(y) {
    x <<- y # the assignment is done in the parent environment
    inverse <<- NULL # since the matrix has changed
  }
  
  # Used to get the value of a matrix.
  get <- function() x
  
  # Used to set the inverse of a matrix.
  setinv <- function(inv) inverse <<- inv
 
  # Used to get the inverse of a matrix.
  getinv <- function() inverse
  
  # Return a list of all the above functions.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## The function "cacheSolve" calculates the inverse of 
## the special"matrix" created with the "makeCacheMatrix" function.
## First, by using the "getinv" function from the "makeCacheMatrix" function 
## it checks if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the
## computation. Otherwise, it calculates the inverse of the
## matrix and sets the value of the inverse in the cache via
## the "setinv" function defined in the "makeCacheMatrix".

cacheSolve <- function(x, ...) {
  
  # Check if the inverse is already cached
  inverse <- x$getinv()
  if(!is.null(inverse)) {
    message("Getting cached data.")
    return(inverse)
  }
  
  # Not cached, so we get the matrix into data
  data <- x$get()
  # and compute the inverse,
  inverse <- solve(data)
  
  # Then we cache the inverse
  x$setinv(inverse) # for later use if needed (e.g. in loop)
  
  # and return the inverse. 
  inverse
}
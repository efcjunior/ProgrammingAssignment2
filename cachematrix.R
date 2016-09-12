## These functions calculate the inverse of a matrix and cache the inverse.
## Once the matrix inverse is computed (by calling the "cacheSolve" function),
## the value of the inverse is cached until the values in the matrix are changed.

## makeCacheMatrix creates a special "matrix", which is really a list containing 
## the following functions:
## 1. set(): set the value of the matrix
## 2. get(): get the value of the matrix
## 3. setinv(): set the value of the matrix inverse
## 4. getinv(): get the value of the matrix inverse.
## 
## While calling makeCacheMatrix, always make sure to assign it a variable, 
## and input a square matrix, e.g.,
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of matrix in x. If the matrix inverse
## of x already exists, the function returns the cached value.
## Always make sure to run this function using the variable to which the 
## makeCacheMatrix was assigned, e.g.,
## > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## > cacheSolve(my_matrix)
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  m <- x$get()
  i <- solve(m, ...)
  x$setinv(i)
  i
}       

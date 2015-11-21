## Functions : makeCacheMatrix(), cacheSolve()
##
## Purpose   : Store a matrix in object form; store (cache) its inverse; 
##                offer get() and set() functions for the matrix object and its inverse
##             Return the inverse of matrix 'x'; return cached value of inverse when available
##
## Usage     : source("cachematrix.R")
##             mat <- makeCacheMatrix(matrix(c(2,0,0,0,2,0,0,0,2), nrow = 3))
##             mat$get()
##             matInv <- cacheSolve(mat)
##             matInv
##             mat$get() %*% matInv
##             matInv1 <- cacheSolve(mat)
##             matInv1
##
## Author    : Rahul N. Pupala
##
## Date      : 20 November 2015


## Store a matrix in object form; store (cache) its inverse; offer get() and set() functions
## for the matrix object and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x   <<- y                         # set (possibly reset) the object
    inv <<- NULL                      # reset the cached inverse each time the object changes
  }
  get    <- function() x
  setInv <- function(invArg) inv <<- invArg
  getInv <- function() inv

  list(set = set,
       get = get,
       setInv = setInv,
       getInv = getInv)
}


## Return the inverse of matrix 'x'; return cached value of inverse when available

cacheSolve <- function(x, ...) {
  inv <- x$getInv()                   # get the cached inverse
  
  if(!is.null(inv)) {                 # if it exists, we are done
    message("getting cached Inv")
    return(inv)
  }

  # else, compute the inverse and cache it for future use
  data <- x$get()                     # get the data (matrix) object
  inv  <- solve(data, ...)            # find its inverse
  x$setInv(inv)                       # cache inverse for future use

  inv                                 # last statement defines the value we wish to return
}
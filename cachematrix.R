## These are two functions that take advantage of lexical scoping to cache 
## matrix data to improve performance by not computing the matrix inverse
## if it has already been done and verifies the related matrix has not changed.
##
##
## This function creates a list of functions to be used by a different
## function in a separate environment allowing for the following:
##    1. set the value of the matrix if it has not changed.
##    2. get the value of the current matrix
##    3. set the cached value of the inverse of the matrix
##    4. get the value of the cached inverse of the matrix
##   
makeCacheMatrix <- function(x = matrix()) { 
  matrix_inverse = NULL
  set = function(y) {
    ##
    ##  Matrix check - set if different than previous updates else skips so 
    ##  cached value if inverse is used and not recalculated
    ##
      if (!(identical(x,y)) {
      x <<- y
      matrix_inverse <<- NULL
        }
  }
  get = function() x
  setinverse = function(inverse) matrix_inverse <<- inverse 
  getinverse = function() matrix_inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
 
## The following function sees if the previous function has cached the matrix
## inverse.  If so it diplays an appropriate message and returns the cahced value.
## If there is no cached value then the function computes the inverse matric and 
## caches the result before returning it.

cacheSolve <- function(x, ...) {
    matrix_inverse =  x$getinverse()
    if (!is.null(matrix_inverse)){
      message ("Getting cached data...")
      return(matrix_inverse)
    }
    matrix_data = x$get()
    matrix_inverse = solve(matrix_data, ...)
    x$setinverse(matrix_inverse)
    return(matrix_inverse)
  }
   

## -------------------------------------------------------------
## makeCacheMatrix creates a list that includes the matrix
## 
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL

  
## Four functions to set and get matrix and inverse matrix values
  # and to cache matrix and inverse matrix  
  
  setMatrix <- function(y) {
        x <<- y
        inv <<- NULL
      }
  getMatrix <- function() x
  setInverse <- function(solve) inv <<- solve(x)
  getInverse <- function() inv
 
## List Output  
  
  list(getMatrix = getMatrix, setMatrix = setMatrix, 
       setInverse = setInverse, getInverse = getInverse)
}

### CacheSolve outputs the inverse of a matrix.  If argument matrix x was created by makeCacheMatrix,
###  cachesolve will output the cached matrix, oherwise cachesolve will perform solve(x)
###    Cachesolve also evalutes the matrix against the cached matrix if it exists, if they are not identical,
###        cachesolve will DON"T FORGET TO WRITE SOMETHING!!!


cacheSolve <- function(x, ...) { 
  ## 
#   cacheMatrix <- x$getMatrix
#   message("cachematrix value is")
#   print(cachematrix)
  
#   if (identical(x,cacheMatrix)) {
#     message("The matrix entered does not match the cached matrix - recomputing inverse matrix")
#   }
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix)) { 
    message("getting cached data invMatrix") 
    return(invMatrix) 
  }
  cacheMatrix <- x$getMatrix()
  invMatrix <- solve(cacheMatrix, ...) 
  x$setInverse(cacheMatrix) 
  invMatrix
} 
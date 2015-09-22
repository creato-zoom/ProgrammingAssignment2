## The following two functions allow for caching the inverse of a matrix (using the solve function)
## The benefit to caching the inverse of the matrix is that matrix inversion is usually a costly computation
##  and therefore computing the inverse repeatedly would not be optimized.
## Example Usage:
##    > sq_matrix <- matrix(1:4,2,2)
##    > sq_matrix_cached <- makeCacheMatrix(sq_matrix)
##    > cacheSolve(sq_matrix_cached)
##
## the function cacheSolve will return the inverse of sq_matrix every time it is called.
## however, only the first time that it is called, does it actually call the solve function
## to compute the inverse.  Each subsequent time, a cached version of the inverse is returned
  
##makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse of the matrix
##   get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The following function calculates the inverse (using the solve function) of the special "matrix" 
## created with the above function. However, it first checks to see if the inverse has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m  
}

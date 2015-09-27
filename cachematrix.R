##This assignment shows a pair of functions that cache the inverse of a matrix.
##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

##this function calculates the inverse of the special "matrix"
##created with the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inv <<- solve
  getsolve <- function() inv
  list(set = set, get = set,
       setsolve = setsolve,
       getsolve = getsolve)   

}


##this function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheInverse <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getsolve()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setsolve(inv)
  inv
}

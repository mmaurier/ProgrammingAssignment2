## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
x <- rbind(c(1, 1/4), c(1/4, 1)) 

makeCacheMatrix <- function(x = matrix()) {
  s <- x$getsolve()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setsolve(m)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- NULL
  set <- function(y) {
    x <<- y 
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) i <<- solve
  getsolve <- function() i
  list(set = set, get = set,
       setsolve = setsolve,
       getsolve = getsolve)    
}

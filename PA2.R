
##This function caches the inverse of a matrix  
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function() inv <<- solve(x) #calculate the inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

cacheSolve <- makeCacheMatrix() 
cacheSolve$set(matrix(1:4, 2))
cacheSolve$get()

## This function returns the inverse of 'x'
cacheSolve <- function(x, ...) {
  
    +  inv <- x$getInverse()
    +  if (!is.null(inv)) {
      +    message("getting cached data")
      +    return(inv)
    }
    
    data <- x$get()
    invx <- solve(data, ...)
    
    # cache inverse
    x$setinv(invx)
    
    invx
}
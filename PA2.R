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
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
cacheSolve$setInverse()
cacheSolve$getInverse()
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

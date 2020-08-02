## These functions are for finding the inverse of the matrix. If the inverse has been previously calculated, then answer will be fetched from cached data, otherwise inverse will be calculated and stored in cache


## This function creates a matrix having list of commands like setting a matrix, getting the value of matrix,setting the value of inverse and getting the value of inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set <- function(y) {
    x <<- y
    inv<<- NULL
  }

    get <- function() {x}
    setInverse<- function(inverse) {inv <<- inverse}
    getInverse <- function() {inv}
    list(set=set,get=get,setInverse = setInverse,
     getInverse = getInverse)
}


## This function will calculate the inverser of above matrix. If the inverse is already calculated , it will fetch from cache and the calculation is skipped.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv<- solve(mat, ...)
  x$setInverse(inv)
  inv
}

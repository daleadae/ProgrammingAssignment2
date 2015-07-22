## makeCacheMatrix() will cache a matrix to allow for faster computation
## cacheSolve() will calculate the inverse of a given matrix by first checking if we already have this data in cache


## makeCacheMatrix will create a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cachesolve returns the inverse of the matrix given by makeCacheMatrix.
## It first checks if the inverse was already computed and returned it if that is the case.
## If the inverse has not been computed yet, it will calculate the inverse
## and will cache the newly calculated value through setinverse()

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  
  ## Return a matrix that is the inverse of 'x'
  inv
}
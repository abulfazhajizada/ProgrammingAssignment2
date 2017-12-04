## Overall aim of this assignment is to create two functions, "makeCachemMatrix"
## and "cahceSolve". By creating these functions we save time on complex
## computations.

## This function uses matrix to cache the inverse of the input

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  +  set <- function(y) {
    +    x <<- y
    +    inv <<- NULL
    +  }
  +  get <- function() x
  +  setinv <- function(inverse) inv <<- inverse
  +  getinv <- function() inv
  +  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function simply takes above function and computes its inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  +  if(!is.null(inv)) {
    +    message("getting cached result")
    +    return(inv)
    +  }
  +  data <- x$get()
  +  inv <- solve(data, ...)
  +  x$setinv(inv)
  +  inv
}

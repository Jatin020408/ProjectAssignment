## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # This will hold the cached inverse matrix
  print(x)
  # Setter function to store a new matrix and clear the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Clear cached inverse since matrix has changed
  }
  
  # Getter function to retrieve the matrix
  get <- function() x
  
  # Setter function for the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter function for the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  
  # If the inverse is already cached, return it
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  data <- x$get()
  inv <- solve(data, ...)  # Compute the inverse
  x$setInverse(inv)        # Cache the inverse
  inv   
}


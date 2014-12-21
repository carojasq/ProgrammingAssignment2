# This function build a special matrix to help with inverse matrix caching
makeCacheMatrix <- function(x=matrix()) {
  inverse_matrix <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <-function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function returns the inverse of a existing matrix, if the inverse is not cached then calculates and return
cacheSolve<- function(x, ...) {
  inverse <- x$getinverse()
  if (!is.null(inverse)){
    message("Getting cached data")
    return (inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

# Example code, uncomment to test
#x <- matrix(c(4,2,7,6), 2, 2)
#new_matrix <- makeCacheMatrix(x)
#print (cacheSolve(new_matrix))



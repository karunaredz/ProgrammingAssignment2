library(MASS)

mat <- matrix(1:9,ncol=3,nrow=3,byrow=TRUE)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv) & identical(x$get(),mat)) {             ### check if the matrix is equal 
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- ginv(data, ...)
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

#Test
matrix<-makeCacheMatrix(mat)
matrix$get()
cacheSolve(matrix)
#Now if the mat we passed is equal to which is present in Cache then inverse is taken from cache

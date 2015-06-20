## The following functions create a "matrix" object and cache the inverse of this matrix.
## cacheSolve then computes and returns the inverse of the matrix, 
## which is retrieved from the cache if it has already been computed.


## makeCacheMatrix creates a list containing four functions:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix's inverse
## 4. get the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
      get <- function() x
      setInverse <- function(inverse)  inv <<- inverse
      getInverse <- function() inv
      list(set = set, get = get, 
           setInverse = setInverse, 
           getInverse = getInverse)
}


## cacheSolve computes the inverse of the matrix from makeCacheMatrix
## and retrieves the inverse from the cache if the inverse has been 
## calculated before

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getInverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      matrix <- x$get()
      inv <- solve(matrix, ...) 
      x$setInverse(inv)
      inv
}

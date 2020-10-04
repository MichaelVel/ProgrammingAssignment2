## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(y) {
      x <<- y
      m <<- NULL
    }
    getmatrix <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
      message("getting cached matrix")
      return(inv)
    }
    matrix <- x$getmatrix()
    inv <- solve(matrix, ...)
    x$setinv(inv)
    inv
  }
        ## Return a matrix that is the inverse of 'x'
}

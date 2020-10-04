## The functions makeCacheMAtrix and cacheSolve work together to compute 
## and store in the cache the inverse of an invertible matrix  

## The function makeCacheMAtrix receives as argument an invertible matrix
## and return a special object that can cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    setmatrix <- function(y) {
      x <<- y
      m <<- NULL
    }
    getmatrix <- function() x
    setinv <- function(inv) inverse <<- inv
    getinv <- function() inverse
    list(setmatrix = setmatrix, getmatrix = getmatrix,
         setinv = setinv,
         getinv = getinv)
  }
}


## The cacheSolve function receives the special object as argument and computes
## the inverse of the matrix. If the inverse has been already calculated 
## (and its the same) the function returns the cached matrix inverse


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

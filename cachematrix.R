## These two functions are used to create a special object 
## that stores a matrix and cache's the inverse of the matrix.  

makeCacheMatrix <- function(x = matrix()) {
     ## this function creates an object that stores a matrix
     ## and it's inverse 
     
     s <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInverse <- function(solveInv) s <<- solveInv
     getInverse <- function() s
     list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     ## if the value has already been cached, then return the cached data
     
     s <- x$getInverse()
     if (!is.null(s)) {
          message("get cached data")
          return(s)
     }
     matx <- x$get()
     s <- solve(matx)
     x$setInverse(s)
     s
}

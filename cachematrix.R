## This pair of functions, makeCacheMatrix and cacheSolve, utilize a cache to optimize access to the inverse of a matrix 

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
          ## Return matrix that can cache a previously computed inverse
          i <- NULL
          set <- function(y) {
                    x <<- y
                    i <<- NULL
          }
          get <- function() x
          setinv <- function(inverse) i <<- inverse
          getinv <- function() i
          list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
          ## Return matrix 'i' that is the inverse of 'x'
          i <- x$getinv()
          if(!is.null(i)) {
                    message("cached data")
                    return(i)
          }
          data <- x$get()
          i <- solve(data, ...)
          x$setinv(i)
          i
}

## this function will set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
 xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) xinv <<- inv
  getinv <- function() xinv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


##  this function Solves, stores, and returns the inverse of the cached matrix,
## or only returns it if it already exists.

cacheSolve <- function(x, ...) {
     xinv <- x$getinv()
  if(!is.null(xinv)) {
    message("getting cached data")
    return(xinv)
  }
  data <- x$get()
  xinv <- solve(data, ...)
  x$setinv(xinv)
  xinv
}

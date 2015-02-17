## These 2 functions create a vector which can be stored, calculate its inverse and cache it

## makeCacheMatrix sets the value of a matrix and stores it 

makeCacheMatrix <- function(x = matrix())  {
  xINV <- NULL
  set <- function(y) {
    x <<- y
    xINV <<- NULL
  }
  get <- function() x
  setINV <- function(INV) xINV <<- INV
  getINV <- function() XINV
  list(set=set, get=get, setINV=setINV, getINV=getINV)
}


## cacheSolve calculates the Inverse of the stored matrix and caches it

cacheSolve <- function(x, ...) {
  xINV <- x$getINV
  if(!is.null(xINV)) {
    message("getting cached data")
    return(xINV)
  }
  data <- x$get
  xINV <- solve(data)
  x$setINV(xINV)
  xINV
}

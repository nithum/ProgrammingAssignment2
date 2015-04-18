## Put comments here that give an overall description of what your
## functions do

## This function allows us to construct a matrix which caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(INV) inv <<- INV
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## This function returns the inverse of a CacheMatrix object x by first checking
## if the inverse is already cached. If so, it returns the cached result. If 
## not, it computes the inverse and caches that.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}

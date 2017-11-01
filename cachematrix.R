## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object please note that the object can be initialized
## to an empty state.

makeCacheMatrix <- function(x = matrix()) {
  #Create special matrix that will then be passed tp
  #cacheSolve 
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inv) m <<- inv
  getInv <- function() m
  list(get = get, set = set, 
       setInv = setInv, getInv = getInv)
}


## The function below returns the inverse of the matrix object x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)){
    message('returning inverse matrix')
    return(x$getInv())
  }
  dat <- x$get()
  m <- solve(dat, ...)
  x$setInv(m)
  m
}

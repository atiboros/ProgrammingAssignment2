## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(m_) {
    m <<- m_
    inverseMatrix <<- NULL
  }
  get <- function() m
  setInverse <- function(inv) inverseMatrix <<- inv
  getInverse <- function() inverseMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) { 
          return(inv)
        }
        
        cachedM <- x$get()
        inv <- solve(cachedM, ...)
        x$setInverse(inv)
        inv
        
}

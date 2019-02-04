

## the makeCacheMatrix function builds an
## object that represents a cached computation
## of a inverse matrix
## it returns a list with four functions that
## allow to get the original matrix m, to set the matrix to a new value
## to get the inverse matrix, which might be null if not  yet computed,
## end finaly a function that sets the inverse matrix value matrix
makeCacheMatrix <- function(m = matrix()) {
  ## setting the cachhed inverse matrix to NULL
  ## since it is not yet computed
  inverseMatrix <- NULL
  
  ## the set function set a new value
  ## for the matrix
  ## end sets the cached inverse matrix to null
  ## indicating that the inverse of the new matrix
  ## wasn't calculated yet
  set <- function(m_) {
    
    ## attention store values 
    ## in the environment one level up
    ## in the environment of the
    ## makeCacheMatrix function
    m <<- m_
    inverseMatrix <<- NULL
  }
  
  ## this functionn gets the cahced value of the matrix
  get <- function() m
  
  ## this function sets the cached value of the inverse matrix
  setInverse <- function(inv) inverseMatrix <<- inv
  
  ## function get the cached inverse matrix
  getInverse <- function() inverseMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
  

}


## the cacheSolve takes a cached
## matrix and either retyurn the already
## calculated inverse matrix or
## calculates and caches the inverse matrix
cacheSolve <- function(x, ...) {
  
        ## get the cahed inverse matrix
        inv <- x$getInverse()
        
        ## if not null, e.g. already calculated
        ## simply return it
        if(!is.null(inv)) { 
          return(inv)
        }
        
        ## get the cached matrix
        cachedM <- x$get()
        
        ## calculate its inverse
        inv <- solve(cachedM, ...)
        
        #store the inverse in the cache
        x$setInverse(inv)
        
        ## return the inverse matrix
        inv
        
}

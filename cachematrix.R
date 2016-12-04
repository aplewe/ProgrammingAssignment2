## the function makeCacheMatrix caches a matrix and its inverse
## cacheSolve will either a.) compute the inverse and cache it,
## or b.) return the inverse if it's already cached.

## create a "matrix" object that also caches its own inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y)
    {
         x <<- y
         m <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    
    list(
      set = set, 
      get = get, 
      setinverse = setinverse,
      getinverse = getinverse)
}


## Return cached inverse or compute it:

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinverse()
      if(!is.null(inv))
      {
        return(inv)
      }
      res <- x$get()
      inv <- solve(res)
      x$setinverse(inv)
      inv
}

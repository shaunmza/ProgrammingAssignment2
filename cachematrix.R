## The first of these two functions create a cached matrix 
## with get and set functionality
## the second is called to fetch the inverse from the cache, 
## and the result will be returned

## Creates a cacheable matrix
## Expects a inversible matrix
## Returns a list containing functions

makeCacheMatrix <- function(x = matrix()) {
    ## clean up the environment
    m <- NULL
    
    ## anonymous function to set the value of the matrix 
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## anonymous function to get the value of the matrix 
    get <- function() x
    
    ## anonymous function to set the inverse value of the matrix 
    setinverse <- function(inverse) m <<- inverse
    
    ## anonymous function to get the inverse value of the matrix 
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Fetch inverse of matrix from the cache
## If not cached, inverse then cache and return inverse
## Expects list of functions outputted from makeCacheMatrix
## Returns inverse of matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## Lets try get it from the cache
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    ## return the cached inverse, breaks out of the function,
    ## so no further work done in here
    return(m)
  }
  ## Not cached, lets do some heavy lifting...
  ## Use the anonumous get function defined in makeCacheMatrix
  ## to fetch the actual matrix
  data <- x$get()
  
  ## Perform the inverse operation
  m <- solve(data, ...)
  
  ## Cache it for pssible future use
  x$setinverse(m)
  
  ## Last line returned, this is the inverse we were looking for
  m
}




## This function makeCacheMatrix recive a matrix: x and creates a list that contains functions to 
## set: set the value of the matrix
## get: get the value of the matrix
## setinvmat set the inverse of the matrix
## getinvmat get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  m<-NULL
  set <-function(y) 
  {
    x <<-y
    m <<- NULL
  }
  get <- function()x
  setinvmat <- function(solve) m <<- solve
  getinvmat <- function() m
  list(set=set, get=get, setinvmat=setinvmat, getinvmat=getinvmat)
  
  
}


## This function calculates the inverse of the matrix it gets. 
## Checks if the inverse has been calculated  and then gets it from the cache and return the value. 
## If it isn't on the cache then get the inverse of the matrix and return the value. 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinvmat()
  if(!is.null(m))
  {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinvmat(m)
  m
}
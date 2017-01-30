## Matrix invesion can be done using a cached function so that it is easier to be computed.

## The following function is used to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  set<- function(y)
  {
    x<<- y
    inverse<<- NULL  
  }
  get<-function()x
  setInv<- function(Inv) inverse<<-Inv 
  getInv <- function() inverse
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Function to inverse a matrix assuming all the functions are invertible

cacheSolve <- function(x, ...) {
  inverse <- x$getInv()
  if(!is.null(inverse)) {
    message("getting cached data.")
    return(inverse)
  }
  input<- x$get()
  inverse <- solve(input)
  x$setInv(inverse)
  inverse
}

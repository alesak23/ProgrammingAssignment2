## This is pretty much what was given in assignment text, but with matrix inverses instead of vector means.
## But I made some changes to these functions to make behaviour more logical, see below.

## Creates list of functions, where parent environment of these functions contains matrix 'x' and upon request also its inverse.
## Difference from assignment example is that getInverse test if inverse is null and if it, it computes inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y){
    x<<-y
    inverse<<-null
  }
  
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() {
    if(is.null(inverse))
      inverse <<- solve(x)
    
    inverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Because of changes to getInverse function, logic in this function was made redudant, so its almost empty.
## Now cached inverse is computed only upon request (calling getInverse and inverse is null),
##  so there is no need for this function.

cacheSolve <- function(x, ...) {
        x$getInverse()
}

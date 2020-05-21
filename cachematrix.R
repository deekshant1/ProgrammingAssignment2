## Pair of functions to cache and compute inverse of a matrix

## This function will create a special "matrix" object that will cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<-NULL
  set <- function(mtrx)
  {
    x <<- mtrx;
    inverse<<-NULL;
  }
  get<-function() return(x);
  setinverse <- function(inv) inverse<<-inv;
  getinverse <- function() inverse
  return(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve function will retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse))
  {
    message("Getting cached data...")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
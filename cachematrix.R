## Cache matrix inversion and allows look up in cache if
## matrix was previously inverted

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the special "matrix" returned in makeCacheMatrix;
## if the inverse has already been calculated, this function retrieves from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data,...)  ## returns inverse of matrix data <- x
  x$setinverse(m)
  m
}

## cacheMatrix.R implements Programming Assignment 2: Lexical Scoping for
## Coursera the R course

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse. Creates a special "matrix", which is really a list
## containing a function to: 1. set the matrix 2. get the matrix 3. set the
## matrix inverse 4. get the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cachesolve retrieves the inverse from
## the cache else it computes the inverse by calling the solve() method for the
## matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of the matrix inside the cachedMatrix 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

## Put comments here that give an overall description of what your
## functions do
# theses functions enables to cache the inverse of a matrix
# saving the time to re-calculate it

## Write a short comment describing this function
# makeCacheMatrix returns a list of function to access a matrix & its inverse.

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


## Write a short comment describing this function
# cacheSolve returns the inverse of matrix x
# if it can find it in the cache, it returns it from the cache
# otherwise, it computes & put it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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

# test code
x = matrix( c(1,2,1,1), nrow=2, ncol=2)
x
y <- solve(x)
y
xx <- makeCacheMatrix(x)
z <- cacheSolve(xx)
z
z <- cacheSolve(xx)
z
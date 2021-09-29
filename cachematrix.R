# R function is able to cache potentially time-consuming computations.
# While taking the mean of long vectors  If the contents of a vector are not changing,
#it may make sense to cache the value of the mean so that when we need it again, 
#it can be looked up in the cache rather than recomputed


# special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix) {
  m <- NULL 
  set <- function(y) {
    x <<- y
    m <<- NULL 
  }
  get <- function() x 
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m 
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}

#computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x, ...) {
  m <- x$getInv() 
  if(!is.null(m)) {
    message('getting cached data')
    return(m)
  }
  data <- x$get() 
  m <- solve(data, ...) 
  x$setInv(m) 
  m 
}
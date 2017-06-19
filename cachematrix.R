## makeCacheMatrix caches the matrix x and its inverse.
## cacheSolve retrieves the cached inverse, or
## calculates it if it has not been cached.

## create a list of functions to:
## 1. set the value of the matrix,
## 2. return the value of the matrix,
## 3. calculate the inverse of the matrix,
## 4. return the calculated inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## calculates the inverse matrix,
## or, if already calculated, retrieves it from cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  return(m)        ## Return a matrix that is the inverse of 'x'
}






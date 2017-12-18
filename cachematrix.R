## The cache matrix function is a set of nested functions which computes the inverse
## of a matrix and then caches its result of later use. 

## The makeMatrix function takes a matrix, computes and sets the inverse 

makeMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## the cachsolve function first checks in the input matrix has already been solved
## if so it returns the cached inverse. if not it computes and caches the inverse. 

cachesolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("returning cached inverse")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}



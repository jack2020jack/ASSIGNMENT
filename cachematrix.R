makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcacheSolve <- function(solve) m <<- solve
  getcacheSolve <- function() m
  list(set=set, get=get, setcacheSolve=setcacheSolve, getcacheSolve=getcacheSolve)
}

cacheSolve <- function(x, ...) {
  m <- x$getcacheSolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmean(m)
  m
}
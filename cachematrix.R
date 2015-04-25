## Overall these two functions compute the inverse of a matrix in the least processor-intensive way.  The first function enables the use
# of a cached matrix rather than requiring the re-computation of one which has already been processed.  The second draws on that to
# solve a matrix inversion task, returning the inverse of it's argument.

## makeCacheMatrix is a function designed to take an argument which is a matrix, and store the inverse of that matrix for future use as
#part of the cacheSolve function detailed in the commentry below.

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  set <- function(y) {
    get<- function() x
    setinverse <- function()solve(x)
    m <<- solve(x)
    getinverse <- function(m)
    matrix(set=set, get=get)
      setinverse = setinverse
      getinverse = getinverse
  }
}


## cacheSolve is designed to return the inverse of a given matrix in the most efficient way possible.  If that is by drawing on the cached
# result of makeCacheMatrix, then it does so.

cacheSolve <- function(x, ...) {
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

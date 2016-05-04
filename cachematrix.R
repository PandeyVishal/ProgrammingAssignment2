## Two Functions have been created that stores the matrix in a special form so that it can be 
## so that we can perform cache the result and avoid unwanted calculations.

## makeCacheMatrix function helps in making the special matrix that helps in performing the caching operations
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set_mat <- function(y) {
    x <<- y
    m <<- NULL
  }
  get_mat <- function() x
  set_inv <- function(inv) m <<- inv
  get_inv <- function() m
  list(set_mat = set_mat, get_mat = get_mat,
       set_inv = set_inv,
       get_inv = get_inv)
}

## Cachesolve function helps in calculating the inverse, but only after checking that we have not calculated the same result hitherto
cacheSolve <- function(x, ...) {
  m <- x$get_inv()
  if(!is.null(m)) {
    message("This inverse is already calculated, Getting you the chached data")
    return(m)
  }
  data <- x$get_mat()
  m <- solve(data, ...)
  x$set_inv(m)
  m
}
x=rbind(c(1, -1/4), c(-1/4, 1))
s=makeCacheMatrix(x)
cacheSolve(s)

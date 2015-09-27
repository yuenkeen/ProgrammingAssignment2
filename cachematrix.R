## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly (there 
## are also alternatives to matrix inversion that we 
## will not discuss here). Your assignment is to write 
## a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix() function creates a special 
## "matrix" object that can cache its inverse.

## To make a cache matrix, do this: 
## amatrix <- makeCacheMatrix(c(1, 2, 3, 4), nrow = 2, ncol = 2).

## Note the ellipsis in makeCacheMatrix() will pass 
## additional matrix arguments nrow and ncol .

makeCacheMatrix <- function(x = matrix(), ...) {
  m <- NULL
  set <- function(y) {  # set() function sets vector x to a new 
    x <<- y             # vector y, and resets m to NULL,  
    m <<- NULL          # where m would be made a matrix.
  }
  get <- function() matrix(x, ...)  # get() function returns x as matrix, note the ellipsis.
  setmatrix <- function(matrix) m <<- matrix  # setmatrix() assigns matrix to m? But what's the purpose??
  getmatrix <- function() m  # getmatrix() function returns m.
  list(set = set, get = get,  # Returns a list of functions.
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## cacheSolve() function first check cache whether there's
## an existing inverse matrix x stored.

## Then solve for inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  if(!is.null(m)) {  # Checking if m is available in cache, if so return m
    message("getting cached data")
    return(m)
  }
  data <- x$get()  # If no existing matrix m, assigns x$get() to data
  m <- solve(data, ...)  # Solve for inverse matrix
  x$setmatrix(m)  # Returns x$matrix(m)
  m
}

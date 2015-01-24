## To create a special object that stores a numeric matrix and cache's its mean

## makeCacheMatrix creates a speical list containing a function of 
## set the value of the matrix
## get the value of the matrix
## set the value of the inversion of matrix
## get the value of the inversion of matrix

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) m <<- inversion
  getinversion <- function() m
  list(set = set, get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## Checks to see if the inversion of matrix has already been calculated. 
## If so, it gets the inversion of matrix from the cache and skips the computation. 
## Otherwise, it calculates the inversion of matrix of the data and 
## sets the value of the inversion of matrix in the cache via the setinversion function.

cacheSolve <- function(x, ...) {
  m <- x$getinversion()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } 
  ## check if inversion of matrix is cached
  ## if it is cached, then return the message and m
  data <- x$get()
  m <- solve(data, ...)
  x$setinversion(m)
  m
}
## if it is not   
## Return a matrix that is the inverse of 'x'

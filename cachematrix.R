## These 2 functions help to create a list for storing a matrix and caching its inserversion.

# Examples
# > cm <- makeCacheMatrix( matrix(1:4,2,2) )
# > cacheSolve(cm)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(cm) %*% cm$get()
# getting cached data
# [,1] [,2]
# [1,]    1    0
# [2,]    0    1

## Creates a special "Matrix", which is a list containing 4 functions 
## that include set, get, setinversion and getinversion
## x must be a square matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) i <<- inversion
  geinversion <- function() i
  list(set = set, get = get,
       setinversion = setinversion,
       geinversion = geinversion)    
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

  i <- x$geinversion()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinversion(i)
  i  
    
}

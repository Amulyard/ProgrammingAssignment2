## Below two functions will create a caching facility for a time consuming calculation (Inverse of a matrix)
##  creates a special object that stores a matrix and cache's its inverse.

## Below function creates definition for get/ set and getinv / setinv methods
makeCacheMatrix <- function(x = matrix()) {
  #inv stores the Matrix Inverse
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <<-NULL
  }
  get <- function() x
  setinv <- function(MatInv) inv <<- MatInv
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Below function takes a matrix caching object and return the inverse of that matrix

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

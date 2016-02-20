## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is an oblect that stores the inverse  of a given matrix in memory, 
# but not necessarily in the current environment. So that if once in the past the
# inverse of a specific matrix has been calculated, the object cashes it. 

# cacheSolve returns the value of the inverse matrix. First, it searches whether the
# inverse has been already calculated and stored. If not, it computes it and writes it in
# the memory. 

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(stored) inv <<- stored
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

# cacheSolve does the following:
# first, it searches in the object makeCacheMatrix() for the inv value
# if this value is stored already, it returns inv value
# if it is not stored (NULL), it computes the inverse of a matrix which is an argument of
# makeCacheMatrix() and stores it in inv. 
# So that next time the same matrix is called, its inverse is retrieved from the cash.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}

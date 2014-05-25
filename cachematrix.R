## Put comments here that give an overall description of what your
## functions do

## This function allows to make a list containing functions for: 
## 1. Setting the value of the matrix x
## 2. Getting the value of the matrix x
## 3. Setting the inverse of matrix x
## 4. Getting the inverse of matrix x

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


## This function allows to obtain the inverse of a matrix x; 
## since this function first determines whether the inverse has already been obtained,
## before making the calculation, it is useful for calculations involving large matrices or a high number of them.
## If the inverse of the matrix has not yet been calculated, it obtains its inverse and
## update the value of the inverse in the cache, with the setsolve function. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

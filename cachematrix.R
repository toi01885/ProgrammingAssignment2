## This function creates a special "matrix" object that can cache its inverse

## 1. Set the value of the matrix
## 2. Get the value of the matrix
## 3. Set the value of the inverse (inv)
## 4. Get the value of the inverse


makeCacheMatrix <- function(a = matrix()) {
  inv <- NULL
  set <- function(b) {
    a <<- b
    inv <<- NULL
  }
  get <- function() a
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function calculates the inverse of the special matrix. 
##It checks if the inverse have been calculated; if it has it returns it from 
##the cache, else it calculates it.

cacheSolve <- function(a, ...) {
    inv <- a$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- a$get(a)
  inv <- solve(data, ...)
  a$setinv(inv)
  inv
}



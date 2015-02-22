# These functions cache the inverse of a matrix. When the value of the inverse of the matrix
# is needed again, it is looked up in the cache rather than recomputed.

# Creates a special matrix object that can cache its inverse.
# This function returns a list of functions to manipulate the matrix and its inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  # assigns a value to an object in an environment different from the current environment
  set <- function(y){
    x <<- y
    s <<- NULL
  }
  
  # get the matrix passed as parameter to this function makeCacheMatrix.
  get <-function() x
  
  # setsolve receives the inverse of the matrix and assigns it to s
  # outside the current environment so it can be accessed from cache.
  setsolve <- function(solve) s <<-solve
  
  # getsolve returns the s, which has the inverse of the matrix, if it has already been calculated.
  getsolve <- function() s
  
  # return a list of functions defined within the scope of the makeCacheMatrix function. 
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}

# computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  # s will retrieve the inverse from the cache, if it has already been calculated.
  s <-x$getsolve()
  
  # If the inverse has already been calculated (and the matrix has not changed), 
  # then the cachesolve prints the message and the inverse from the cache.
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  
  # If the inverse has not yet been calculated, 
  # data gets the matrix passed as argument when makeCacheMatrix is called.
  data <- x$get()
  
  # s gets the inverse of the matrix.
  s <- solve(data,...)
  
  # the inverse of the matrix is saved in cache.
  x$setsolve(s)
  x
}

# This part prints the result of calling the two functions:
# It first calculates the inverse, and shows it,
# then it shows the inverse by getting it from cache.
# The variable "cached" gets the return of cacheSolve function (the list of four functions)
# the first time it calculates the inverse
cached <- cacheSolve(makeCacheMatrix(matrix(1:4,2,2))) 

# Returns the calculated inverse of the matrix
cached$getsolve()

# When we call cacheSolve and pass the list of four functions again
# it returns the inverse from cache (with the message).
cacheSolve(cached)
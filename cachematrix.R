## overall description
# * makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# * cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. 
# We assume that the matrix supplied is always invertible.


# This function creates a special "matrix" object.
# Returns a list of functions to set/get 
# * a matrix 
# * a cached value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  
  # store a matrix
  set <- function(y) {
    x <<- y
    s <<- NULL
  }

  # returns the stored matrix
  get <- function() x
  
  # cache the given argument 
  setinverse <- function(solve) s<<-solve
  
  # get cached value 
  getinverse <- function() s
  
  # return a list of named element (element is a function) to s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# This function computes the inverse of the special "matrix" returned by
# makeCacheMatrix above.
cacheSolve <- function(x, ...) {

  # If the inverse has already been calculated (and the matrix has not changed)
  # retrieve the inverse from the cache.
  s <-x$getinverse()
  if(!is.null(s)){
    message("getting cached data")
    return(s)
  }
  #if not, set it, cache it and return result
  data<-x$get()
  s<-solve(data,...) 
  x$setinverse(s)
  s
  
}

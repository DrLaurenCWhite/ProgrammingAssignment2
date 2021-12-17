## makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse of a matrix.


#makeCacheMatrix creates an R object that builds and returns to the parent environment
#a set of functions within a list and stores a matrix and its inverse (if the inverse is already calculated)
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y #assign the input argument to the x object in the parent environment
    inverse <<- NULL #clears any value of m that had been cached by a prior execution of cacheSolve()
  }
  get <- function() x #retrieve x from the parent environment (makeCacheMatrix)
  setinverse <- function(solve) inverse <<- solve #assign the input argument to the value of 'inverse' in the parent environment.
  getinverse<- function() inverse #retrieve 'inverse' from the parent environment (makeCacheMatrix)
  list(set = set, get = get, #named list allows use of $ operator
       setinverse = setinverse,
       getinverse = getinverse)
}


#cacheSolve will calculate and populate and/or retrieve the inverse from an object 
#of type makeCacheMatrix()
cacheSolve <- function(x, ...) {
  inverse <- x$getinverse() #attempt to retrieve the inverse
  if(!is.null(inverse)) { #check if the inverse has already been cached and return if it has
    message("getting cached data")
    return(inverse)
  }
  data <- x$get() #otherwise get the matrix, calculate the inverse and set the inverse in the input object
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

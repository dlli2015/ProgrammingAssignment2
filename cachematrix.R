# Assignment #2
# Function MakeCacheMatrix creates a matrix and also creates four 
# other functions associated with the matrix object:
# 1. function set: initializes and changes the value of a matrix
# 2. function get: gets the value of a matrix
# 3. function setinverse: sets or calculates the inverse of a matrix
# 4. function getinverse: gets the inverse of a matrix

makeCacheMatrix <- function(x=matrix())  {
  m <- NULL
  
  #sets matrix to y, and lets parent environment know that the matrix
  #has been changed by setting m to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #gets the value of a matrix object
  get <- function() x
  
  #sets or calculates the inverse of a matrix
  setinverse <- function(inverse) m <<- inverse
  
  #gets the inverse of a matrix
  getinverse <- function() m
  
  #list of functions
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}



# Function cacheSolve finds the inverse of an invertible matrix
# by first checking if there is a calculated value already cached
# and then, if there is no previous value cached, calculates 
# the inverse and sends it to be cached

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #sets data to equal matrix x
  data <- x$get()
  
  #calculates the inverse of a matrix
  m <- solve(data, ...)
  
  
  x$setinverse(m)
  m
  
}





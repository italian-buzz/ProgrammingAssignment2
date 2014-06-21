## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  #initializes m to null
  m <- NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the solved matrix
  setsolve <- function(solve) m <<- solve
  
  #get the value of the solved matrix
  getsolve <- function() m
  
  #combines set, get, setsolve, and getsolve to a list
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  #tries to get the m matrix from the getsolve function of x
  m <- x$getsolve()
  
  #if the m matrix is not empty, get the cached data
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #gets the matrix
  matrix <- x$get()
  
  #solves the matrix inverse
  m <- solve(matrix, ...)
  
  #sets the value of the solved matrix
  #this also sets the solved matrix globally to m
  x$setsolve(m)
  
  #returns the m value
  m
}

test<-makeCacheMatrix()
test$set(matrix(1:4,2,2))
cacheSolve(test)

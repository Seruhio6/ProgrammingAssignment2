## Put comments here that give an overall description of what your
## functions do


## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL # sets an empty variable in order to store the value of inverse matrix
  set <- function(y) {
    
    x <<- y # sets the value of the matrix in the enclosing environment
    m <<- NULL # Since x has changed sets m to NULL in the enclosing environment
  }
  get <- function() x # gets the value of the matrix
  setinv <- function(inverse) m <<- inverse # sets the value of the inverse in 
                                            # the enclosing environment
  getinv <- function() m #gets the value of the inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv) #return a list of the functions
  
}


##  This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cache should retrieve the inverse from
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinv() # gets the value of the inverse matrix
  
  # if m already exists retrieve data from cache
  if(!is.null(m)) {
    message("getting cached data")
    return(m) # return the cached value of the inverse matrix
  }
  
  # if m doesn't exist, calculates the inverse of the matrix
  data <- x$get() # get the value of the matrix
  m <- solve(data, ...) # computes the inverse matrix
  x$setinv(m) # sets the inverse matrix
  m # return the value of the inverse matrix
}

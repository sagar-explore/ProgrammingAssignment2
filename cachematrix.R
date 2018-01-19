## the make CacheMatrix function have the setter method to set the matrix
## and getter method to retrieve the Matrix

## Write a short comment describing this function

## This is a makeCacheMatrix which can cache its inverse
makeCacheMatrix <- function(x = matrix()) {  ## we are passing the matrix here
  z <- NULL  
  set <- function(y) {  ## we have the setter function which sets the matrix here
    x <<- y            
    z <<- NULL
  }
  get <- function() x  ## we have the getter function in order to retrieve what we set above
  setinverse <- function(inverse) z <<- inverse ## inverse is assigned in enclosing environment
  getinverse <- function() z
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve function does the inverse computation returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## 
  
  z <- x$getinverse() ## this function gets the stored value
  if(!is.null(z)) {                 ## IF will check whether the stored value is available or not
    message("getting cached data")
    return(z)
  }
  data <- x$get()           
  z <- solve(data, ...)     ## Solve function will inverse the matrix
  x$setinverse(z)           ## This will set the value of inversed matrix
  z
}

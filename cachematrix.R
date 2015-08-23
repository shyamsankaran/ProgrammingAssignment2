## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##The first function, makeCacheMatrix creates a special "matrix" object, 
##which is really a list containing a function to
##1. set the value of the Matrix
##2. get the value of the Matrix
##3. SetInverse: Sets the Inverse Matrix
##4. getInverse: Gets the stored value in the Inverse Matrix
makeCacheMatrix <- function(x = matrix()) {
  ##Initiate the object by assigning NULL
  ContainsValue <- NULL
  ## Set will store the matrix
  set <- function(y) {
    x <<- y
    ## Clear the object since new element is stored.
    ContainsValue <<- NULL
  }
  ## Retrieves the stored matrix
  get <- function() x
  
  ## Store the Inverse Matrix. 
  SetInverse <- function(Solve) ContainsValue <<- Solve
  
  ## Get the Inversed matrix cached
  getInverse <- function() ContainsValue
  
  ## Return the list by providing the parameter name. 
  ## Each parameter is a function
  list(set = set, get = get,
       SetInverse = SetInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed),
## then the cachesolve  retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the cacehed value of 'x'
        Inverse <- x$getInverse()
        ## If the cached value is found, return the object
        if(!is.null(Inverse)) {
          message("getting cached data")
          return(Inverse)
        }
        ## Get the Matrix if not cached
        data <- x$get()
        ## Inverse the matrix
        Inverse <- solve(data)
        ## Call the SetInverse function to cache the value
        x$SetInverse(Inverse)
        
        ## display the Inverse matrix
        print(Inverse)
}

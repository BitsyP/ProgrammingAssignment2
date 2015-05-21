### This set of functions, makeCacheMatrix() and cacheInverse(), allow
### the inverse of a matrix to be easily stored and retrieved after
### being computed a single time.  cacheInverse() requires the
### list returned from makeCacheMatrix() to be passed to it.


## makeCacheMatrix() takes a matrix for which one wants to 
## cache the inverse (rather than computing it multipliable times)
## and returns a list with four functions.  The four functions are:
## a) setMatrix() - sets the global matrix of which one wants to
##    store the inverse, and resets the inverse to missing
## b) getMatrix() - returns the matrix of interest
## c) setInverse() - sets the cached, global inverse
## d) getInverse() - returns the inverse
makeCacheMatrix <- function(working_matrix = matrix()) {
  inverse <- NULL
  
  # This function will be returned in the list. It sets a new matrix 
  # to be the one that will have a stored inverse, and sets the stored 
  # inverse to NULL so that the inverse of the old matrix isn't
  # accidentally returned.
  setMatrix <- function(new_matrix) {
    working_matrix <<- new_matrix
    inverse <<- NULL
  }
  
  # This function will be returned in the list. It returns the matrix for which the
  # inverse is or will be stored.
  getMatrix <- function() working_matrix
  
  # This function will be returned in the list. It passes a value to be stored
  # as the cached inverse.
  setInverse <- function(passed_inverse) inverse <<- passed_inverse
  
  # This function will be returned in the list. It returns the stored inverse.
  getInverse <- function() inverse
  
  # A list of the functions above is returned.
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheInverse() takes the list returned from makeCacheMatrix
## and ether computes or retrieves the inverse from the
## current working matrix as appropriate.
cacheInverse <- function(cache_matrix) {
  
  inverse <- cache_matrix$getInverse()
  
  # If the inverse has already been computed, return this
  # pre-computed inverse (this return command exits the function).
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # If the inverse has not yet been computed, the function continues
  # it get the matrix of interest, computes the inverse, stores the
  # inverse and then return the inverse that has just been computed.
  working_matrix <- cache_matrix$getMatrix()
  inverse <- solve(working_matrix) #Allowing other thing to be passed in allows solve to do things beyond the scope of this program.
  cache_matrix$setInverse(inverse)
  inverse
}


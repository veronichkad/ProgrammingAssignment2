# The file contains two functions - makeCacheMatrix and cacheSolve. The are used to create matrix,
# calculate the inverse and store it in the cache

# MakeCacheMatrix is a function that is destined to store a martix and a cached value of the inverse of the
# matrix.

makeCacheMatrix <- function(x = numeric()) {
  
  # theres is nothing to cache at the beginning so the variable cache is set to NULL
  cache <- NULL
  
  # set value of the matrix
  setMatrix <- function(value) {
    x <<- value
    cache <<- NULL
  }
  
  # get value of the matrix
  getMatrix <- function() {
    x
  }
  
  # cache the inverse
  cacheInverse <- function(solve) {
    cache <<- solve
  }
  
  # get the cached inverse value
  getInverse <- function() {
    cache
  }
  
  # return a list of all the elements of the matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The second function makeCacheMatrix - it  returns the inverse if it exists, otherwise - calculates it,
# stores it in the cache and returns it. The matrix should be created with the function above.
cacheSolve <- function(y, ...) {
    
  # get the cached value
  inverse <- y$getInverse()
  
  # if there is already assigned value then return it
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # otherwise caclulate the inverse and store it in the cache
  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  # return the inverse
  inverse
}

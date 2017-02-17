# source("cachematrix.R")
# aSquareMatrix <- makeCacheMatrix(matrix(0:8, 8, 8))
# squareMatrix$getMatrix()
# aSquareMatrix$getCache() # will return NULL for the 1st time
# cacheSolve(aSquareMatrix)
# aSquareMatrix$getCache() # will return the solution

makeCacheMatrix <- function(x = matrix()) {
  
  # makes a cache matrix from a given matrix

  # initialize the cache Matrix 'cacheMatrix'
  # assign the value NULL for the first initialization
  
  cacheMatrix <- NULL
  
  # define the method named 'setMatrix'

  setMatrix <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  
  # define the method named 'getMatrix'
  # return the matrix 'x'
  
  getMatrix <- function() x
  
  # define the method named 'setCache'
  
  setCache <- function(inverse) cacheMatrix <<- inverse
  
  # define the method named 'getCache'
  # that will return the cached inverse of 'x'
  
  getCache <- function() cacheMatrix
  
  # list the names of all methods that will be known to the outside world
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setCache = setCache,
       getCache = getCache)
  
}


cacheSolve <- function(x, ...) {
  
  
  # 'cacheSolve'
  # return the inverse of a given matrix utilizing the cache
  
  # check the content of cache matrix

  cacheMatrix <- x$getCache()
  
  # if the content is not null then: return the result 
  
  if (!is.null(cacheMatrix)) {
    message("loading cache matrix...")
    return(cacheMatrix)
  }
  
  # if the content is empty: 
  # get the matrix, create, set, update and return the cache matrix
  else {
    dMatrix <- x$getMatrix()
    cacheMatrix <- solve(dMatrix, ...)
    x$setCache(cacheMatrix)
    return(cacheMatrix)
  }
  
}

####-we create two functions for inverting a matrix
####-the first one, makeCacheMatrix commit the inverse of the matrix into the cache
####- the second one cacheSolve return the inverse from the cache if the computation has
###-been already done, to avoid computing again and optimize the processing time.



####-the function is the same as the one in the example
###-except using the solve function instead of mean, in the 
###-SetInverse function

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL
   set <- function(y) {
      x <<- y
      inv <<- NULL
   }
   get <- function() x
   setInverse <- function(solve) inv <<- solve #calculate the inverted matrix
   getInverse <- function() inv
   list(set = set,
        get = get,
        setInverse = setInverse,
        getInverse = getInverse)
}

####-cachesolve checks the cached data to recover the result,
###- if it does not exist, it computes the result by calling the solve function again.

cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
   if (!is.null(inv)) {
      message("getting cached data") ###- retreive the result from the cache
      return(inv)
   }
   mat <- x$get()                
   inv <- solve(mat, ...)        ###-invert the matrix, if the result is not cached
   x$setInverse(inv)
   inv
}

####-testing the two functions
matrice = matrix(c(2:5), 2, 2)
mat_inv <- makeCacheMatrix(matrice)
cacheSolve(mat_inv)

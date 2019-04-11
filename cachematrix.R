## Put comments here that give an overall description of what your
## functions do

##The goal of these functions is to create a Matrix inversion and to cache the inverse of a matrix 
##rather than compute it repeatedly. 

## Write a short comment describing this function

## This fuction is to make a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <<- NULL
        set <- function(y) {
                x <<- function(y)
                inv <<- NULL
        }
        get <- function () x
        setInverse <- function(inverse) inv <<- inverse.
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned from makCacheMatrix.
## If the reverse matrix has been calculated (and the matrix has not changed) then the cachesolve should
## retrieve  the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       inv <- x$getInverse()
       if (!is.null(inv)) {
          message("getting cached date")
          return(inv)
       }
       mat <- x$get()
       inv <- solve(mat, ...)
       x$setInverse(inv)
       inv

}

## There are two functions in this .R program
## The first one, makeCacheMatrix, stores an input matrix 
## and caches its inverse (by the assignment rules, the matrix
## is 'guaranteed' to be invertible, so we don't check if it really is)
## The second one, cacheSolve, either gets the inverse from the cache,
## or calculates a new inverse
## Name:   cachematrix.R
## Author: ClydeDale 
## Date:   2015-05-23




##  makeCacheMatrix creates a list of functions, to
##  set the value of a matrix; 
##  get the value of a matrix;
##  set the value of a inverse of that matrix;
##  get the value of a inverseof that matrix

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      setmat <- function(y) {

## note that if the matrix changes, the cached value gets set to NULL

           x <<- y
         inv <<- NULL

      }
      getmat <- function() x
      setinv <- function (invrse) inv <<- invrse
      getinv <- function() inv
      list(setmat= setmat, getmat=getmat,
           setinv = setinv, 
           getinv = getinv)
}


## cacheSolve checks to see if the inverse has already been calculated,
## and that the matrix has not changed ( if !(is.null(inv)) )
## if not changed, the function then gets the inverse from the cache; 
## if changed, calculate the inverse using "solve" and store in the cache

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()

           if (!is.null(inv)) {
                 message("getting cached inverse")
                 return(inv)
        }
        data <-x$getmat()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

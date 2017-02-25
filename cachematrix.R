## cachematrix.R
## Jon Green - Feb 25, 2017
##
## Matrix inversion can be an expensive operation.  This set of functions
## creates and manages an object that stores an invertible matrix along with
## its inverse.  Assuming no change to the matrix itself, the inverse will
## always be cached and does not need to be recomputed for subsequent
## operations.
##
## Example usage:
##  > a <- matrix(c(4,3,3,2), 2, 2)
##  > b <- makeCacheMatrix(a)
##  > b$get()
##  > cacheSolve(b)
##
## Restrictions:  These functions do not ensure that a matrix is actually
## invertible.  cacheSolve() will crash with a fatal error if the matrix
## is not invertible.


## makeCacheMatrix is a "class" that is used to create an object.  It has
## four methods:
##  set() - stores a matrix
##  get() - returns the stored matrix
##  setInverse() - stores the inverse of the matrix
##  getInverse() - returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve is called to return the inverse of the matrix stored in an
## object created with makeCacheMatrix().  If the inverse has not been 
## previously computed, it will be computed and stored in the object.
## If the inverse has been previously cached, the cached value will be
## returned.

cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("using cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}

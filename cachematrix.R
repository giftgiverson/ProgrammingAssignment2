## Implementation of matrix objects which cach their own inverse

## makeCacheMatrix creates an object which represent a matrix which caches it's
##  own inverse.
## Parameters:
##  x - An R matrix to be represented by the returned object.
## Return value:
##  An object representing a matrix which cahces it's own object.

makeCacheMatrix <- function(x = matrix()) {
        # cache value placeholder
        inverse <- NULL
        
        # matrix value setter, resetting the cache
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        # matrix value getter
        get <- function() x
        
        # cache setter
        setsolve <- function(solution) inverse <<- solution
        
        # cache getter
        getsolve <- function() inverse
        
        #return the object as a list of the object functions
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve retrieves the inverse of a matrix, represented by an object
##  created by the makeCacheMatrix function, by retrieving the cached value
##  if it exists, or by calculating and caching the value otherwise.
## Parameters:
##  x - an object created by makeCacheMatrix
## Return value:
##  A matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        # get the cached value
        inverse <- x$getsolve()
        
        if(!is.null(inverse)) {
                # the cached value exists, return it
                message("getting cached data")
                return(inverse)
        }
        
        # the cache wasn't calculated yet, calculate it
        message("calculating data")
        #  get the matrix
        m <- x$get()
        #  calaculate the inverse
        inverse <- solve(m, ...)
        #  cache the value
        x$setsolve(inverse)
        #  return the caclulated value
        inverse
}
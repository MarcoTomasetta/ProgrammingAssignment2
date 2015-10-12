## This pair of function cache the inversion of an
## invertible matrix x

## makeCacheMatrix: This function creates a special 
## ”matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
		i <- NULL
        	set <- function(y) {
               		x <<- y
              		i <<- NULL
        	}
        	get <- function() x
        	setinversion <- function(solve) i <<- solve
        	getinversion <- function() i
        	list(set = set, get = get,
             	setinversion = setinversion,
             	getinversion = getinversion)
}


## cacheSolve: This function computes the inverse of 
## the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

cachemean <- function(x, ...) {
        i <- x$getinversion()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinversion(i)
        i
}

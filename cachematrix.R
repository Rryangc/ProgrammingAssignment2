## This pair of functions will cache the inverse of a matrix.
## 1. makeCacheMatrix: create a "matrix" object caching its inverse
## 2. cacheSolve: compute inverse of the "matrix" using makeCacheMatrix

## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(inverseValue) inverse <<- inverseValue
	getinverse <- function() inverse
	list(set = set, get = get, setinverse = setinverse,
		getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix function. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve should 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
            message("getting cached data")
            return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}

## TESTING OF THIS PAIR OF FUNCTIONS:
##
## > x<-matrix(1:4, 2,2)
## > y<-makeCacheMatrix(x)
## > cacheSolve(y)
## > cacheSolve(y)
##
## The result of the 3rd command will be inverse matrix of x
##				without the statement of "getting cached data"
## The result of the 4th command will be inverse matrix of x
##				with the statement of "getting cached data"


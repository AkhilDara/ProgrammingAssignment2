## This code can be used to cache the inverse of a matrix
## 1. makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## 2. cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix 
##    above. If the inverse has already been calculated (and the matrix has not changed), 
##    then the cacheSolve will retrieve the inverse from the cache.



## The first function, makeCacheMatrix creates a special "matrix", 
## which is a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. calculate the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL
	set <- function(y) {
		x <<- y
		xinv <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) xinv <<- solve
	getinverse <- function() xinv
	list(set = set, get = get,
	        setinverse = setinverse,
		   getinverse = getinverse)

}


## The following function calculates the inverse of the special "vector" 
## created with the above function. However, it first checks to see if the 
## inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of 
## the matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	xinv <- x$getinverse()
	if(!is.null(xinv)) {
	      message("getting cached data")
	      return(xinv)
	}
	data <- x$get()
	xinv <- solve(data)
	x$setinverse(xinv)
	xinv
}

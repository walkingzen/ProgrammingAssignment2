## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix: This function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	im <- NULL
	set <- function(mat) {
		x <<- mat
		im<<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) im<<- inverse
	getinverse <- function() im
	list ( set = set , get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	im <- x$getinverse()
	if(!is.null(im)){
		message("Getting cached inverse of matrix")
		return(im)
	}
	data <- x$get()
	im <- solve(data, ...)
	x$setinverse(im)
	im
}

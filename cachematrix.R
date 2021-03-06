## Programming Assignment 2 Coursera R Programming from John Hopkins
## The functions here are used to do the following:
## to make cached matrix inverse and
## to use cache matrix if it already there else compute it


## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(matrix.param = matrix()) {
	inverse.matrix <- NULL
	set <- function(matrix.in) {
	    matrix.param <<- matrix.in
		inverse.matrix <<- NULL
	}
	get <- function() matrix.param
	setinverse <- function(inverse) inverse.matrix <<- inverse
	getinverse <- function() inverse.matrix
	list ( set = set ,
	       get = get,
	       setinverse = setinverse,
	       getinverse = getinverse )
    }


## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(special.matrix , ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse.matrix <- special.matrix$getinverse()
	if(!is.null(inverse.matrix)){
		message("Getting cached inverse of matrix")
		return(inverse.matrix)
	}
	data <- special.matrix$get()
	inverse.matrix <- solve(data, ...)
	special.matrix$setinverse(inverse.matrix)
	inverse.matrix
}

## Testing the code
## matrix.test <- matrix(rnorm(100), 10 ,10 )
## matrix.in <- makeCacheMatrix(matrix.test)
## matrix.in$get()
## matrix.in$getinverse()
## cacheSolve(matrix.in)
## cacheSolve(matrix.in)
## matrix.in$getinverse()



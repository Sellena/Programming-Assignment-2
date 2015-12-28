## makeCacheMatrix function creates a special object for "matrix"
## and cacheSovle function calculates the inverse of the matrix

## this function will return a list containing four functions

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set = set, get = get, 
		setinverse = setinverse, getinverse = getinverse)
}


## This functon uses the return list from the first functoin as input, then 
## return the inverse matrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
        	message("getting cached inverse matrix")
        	return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
}

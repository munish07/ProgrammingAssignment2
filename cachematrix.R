# The function, makeCacheMatrixr, creates a special "vector", which is really a list containing a function to
#
# 1. set the value of the vector
# 2. get the value of the vector
# 3. set the value of the mean
# 4. get the value of the mean


makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
        set <- function(y) {
        	x <<- y
        	inv <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been 
# calculated (and the matrix has not changed), then the cachesolve 
# retrieves the inverse from the cache


cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
	
	orig_matrix <- x$get()
	inv <- solve(orig_matrix)
	x$setinverse(inv)
        ## Return a matrix that is the inverse of 'x'
	inv
}

## Allow a square invertible matrix's inverse to be cached in order to reduce
## computation time and cost

## Given an invertible matrix return a list of functions allowing access
## to the original matrix and a cached inverse of the matrix
    
makeCacheMatrix <- function(x = matrix()) {
	cached_inverse <- NULL

	get <- function() x
	setinverse <- function(inverse) cached_inverse <<- inverse
	getinverse <- function() cached_inverse
	list( get = get, setinverse = setinverse, getinverse = getinverse)

}


## Given a list created by makeCacheMatrix, return the inverse of the original
## matrix, using a cache if it exists. If no cache exists, compute the inverse
## and cache it

cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
	if (!is.null(inverse)) {
		message("retrieving inverse from cache")
		return(inverse)
	}

	original <- x$get()
	inverse <- solve(original)
	x$setinverse(inverse)
	inverse
}

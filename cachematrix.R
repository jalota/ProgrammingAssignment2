## Matrix inversion is usually a costly computation and there may be some
##  benefit to caching the inverse of a matrix rather than compute it repeatedly.

## Creates a special "matrix" object that can cache it's inverse.

makeCacheMatrix <- function(x = matrix()) {
	## Instantiate inverse matrix variable (cached matrix object)
	s <- NULL

	## Set matrix object
	set <- function(y) {
		x <<- y
	}

	## Return matrix object
	get <- function() x

	## Set inverse matrix object
	setsolve <- function(solve) s <<- solve

	## Return inverse matrix object
	getsolve <- function() s

	## Return special "matrix" object that can cache it's inverse
	list(set = set, 
		get = get, 
		setsolve = setsolve,
		getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed)
##  , then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
		## Define inverse matrix object to internal var
        s <- x$getsolve()

        ## Return message and cached inverse matrix if not null
        if (!is.null(s)) {
        	message("getting cached data")
        	return(s)
        }

        ## Get matrix object for internal computation
        data <- x$get()

        ## Compute matrix inversion
        s <- solve(data, ...)

        ## Cache inverse matrix object
        x$setsolve(s)

        ## Return inverse matrix object
        s
}

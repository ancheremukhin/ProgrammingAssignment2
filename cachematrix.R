
## Create cache matrix

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	getOriginal <- function() {
		x
	}
	setOriginal <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	getInverse <- function() {
		inverse
	}
	setInverse <- function(y) {
		inverse <<- y
	}

	list(
		getOriginal = getOriginal, setOriginal = setOriginal,
		getInverse = getInverse, setInverse = setInverse
	)
}


## Calculate inverse matrix of x and cache the result

cacheSolve <- function(x, ...) {
	inverse <- x$getInverse()
	if (is.null(inverse)) {
		inverse <- solve(x$getOriginal(), ...)
		x$setInverse(inverse)
	} else {
		message("getting cached data")
	}

	inverse
}

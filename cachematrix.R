## This file includes two functions to calculate the inverse of a matrix in
## a less demanding way than normal

## makeCacheMatrix stores a matrix given by the user in the cache

makeCacheMatrix <- function(x = matrix()) { #Matrix must be submitted
	m <- NULL					#Initializing the variable for cache
	set <- function(y) {			#Initializing the set function
		x <<- y				#Assigning the variable to the environment
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

## cacheSolve checks if there's already a matrix stored in cache. If there is,
## retrieves the solution.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setinverse(m)
	m
}

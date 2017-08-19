## The following 2 functions are to make a sqaure matrix, and then 
## calculate its inverse as well as cache the inverse of a matrix to
## avoid computing it repeatedly.

## The first function is to make a matirx that can cache its inverse, 
## what it returns is a list of functions that set the maxtrix and its 
## inverse as well as get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x   <<- y
		inv <<- NULL
	}
	get <- function() x
	setInv <- function(invMatrix) inv <<- invMatrix
	getInv <- function() inv
	list(set = set, get = get, 
	     setInv = setInv,
	     getInv = getInv) 
}


## The second function calculates the inverse of the matrix created above,
## it first checks to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache. Otherwise, it calculates the inverse 
## and sets the inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()
	if(!is.null(inv)) {
		message("getting cached inversed matrix")
		return(inv)
	}
	matrix <- x$get()
	inv <- solve(matrix, ...)
	x$setInv(inv)
	inv
}

## ---------------------------------------------------------------------------
## Followinig is a small test case. Expected output is the first cacheSolve(mM)
## will give the inverse calcluated using solve function and the following 2
## cacheSolve(mM) will return the inverse from cache.

mM <- makeCacheMatrix()
mM$set(matrix(1:4,2,2))
mM$get()
mM$getInv()
cacheSolve(mM)
cacheSolve(mM)
cacheSolve(mM)


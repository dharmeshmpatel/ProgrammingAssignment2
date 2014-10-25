## Matrix inversion is usually a constly computation hence there may be some benefit
## to caching the matrix inversion result instead of computing it each time.

## makeCacheMatrix is a function to create special 'matrix' object that can cache its inverse
## with the help of following functions:
## 1. set value of matrix
## 2. get value of matric
## 3. set value of matrix inverse
## 4. get value of matrix inverse 

makeCacheMatrix <- function(x = matrix()) {

	## inverse is defined as NULL
	inv <- NULL

	## set and get value of matrix
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	
 	## set and get matrix inverse
	setinverse <- function(inverse) inv <<- inverse
	getinverse <- function() inv

	## function names for the caller
	list(setMatrix=set, getMatrix=get, setInverse=setinverse, getInverse=getinverse)
}


## Function returning matrix inverse.  First checking if its value is in its cache, if not,
## it calculates it and sets it in cache for future use

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {

	## Get value from cache if its there
	inv <- x$getInverse()
	if(!is.null(inv)) {
		message("found in cache, returning cached data")
		return(inv)
	}
	
	## Not in cache, let's calcuate
	data <- x$getMatrix()
	inv <- solve(data)

	## Set inverse value in cache
	x$setInverse(inv)
	inv
}

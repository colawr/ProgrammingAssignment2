## Functions makeCacheMatrix() and cacheSolve() can be used to
## cache the inverse of a matrix. These functions make use of the scoping rules 
## of R and manipulate them to preserve state inside of an R object. 

## makeCacheMatrix() function takes a matrix as input and returns 
## a "special matrix" which actually is a list which has
## 1. a function set to set the value of the matrix
## 2. a function get to get the value of the matrix
## 3. a function setinverse() to set (or cache) the inverse of the matrix and
## 4. a function getinverse() to get inverse of the matrix which is cached
## setinverse() returns NULL if inverse of matrix is not cached yet. 


makeCacheMatrix <- function(x = matrix()) {
	ix <- NULL
	set <- function(y) {
		x <<- y
		ix <<- NULL
	}
	get <- function() x
	setinverse <- function(inv) ix <<- inv
	getinverse <- function() ix
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve() takes the "special matrix" created by makeCacheMatrix() as input 
## and returns the inverse of the matrix. If the inverse of the matrix is not  
## calculated yet, this function calculates it and puts it in cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		im <- x$getinverse()
		
		if(!is.null(im)) {
			print("Returning from Cache")
			im
		} else {
			m <- x$get()
			im <- solve(m)
			x$setinverse(im)
			im
		}
}

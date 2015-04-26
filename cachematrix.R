##Erik Babb
##Programming Assignment #2
##cachematrix.R - Two functions which can be used together to cache and return an inverse matrix

## Creates a list with 4 elements (which are functions):
## $set -- set input matrix
## $get -- return input matrix
## $setinverse -- assigns inverse matrix. Intended to be called by cacheSolve(), not called directly by user.
## $getinvese -- returns the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
	inv_output <- NULL ##whenever makeCacheMatrix is called the inverse matrix output is reset
    set <- function(y) {
            x <<- y
            inv_output <<- NULL ##whenever a new matrix is set the inverse matrix output is reset
    }
	get <- function() x ##simply return 
	setinverse <- function(solve) inv_output <<- solve
	getinverse <- function() inv_output
	
	
	list(set = set, get = get,
		 setinverse = setinverse,
		 getinverse = getinverse)
}

## Return a matrix that is the inverse of x$get().
## Calculate using solve() if inverse has not previously been computed.
cacheSolve <- function(x, ...) {
	inv_output <- x$getinverse()
       if(!is.null(inv_output)) {
                message("getting cached data")
                return(inv_output) ##return previously computed inverse matrix
        }
        data <- x$get()
        inv_output <- solve(data, ...) ##call solve() to compute inverse
        x$setinverse(inv_output) ##set the inverse for x
        
		inv_output			
}
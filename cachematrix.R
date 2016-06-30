
makeCacheMatrix <- function(x = matrix()) {
##creates a special matrix with functions to set and get value of matrix
##and set and get the value of its inverse
	i <- NULL
	set <- function(y) {	##sets the value of the matrix
             	x <<- y
             	i <<- NULL
      }
      get <- function() x	##gets the value of the matrix
      setinverse <- function(inverse) i <<- inverse		
		##sets the value of the inverse
      getinverse <- function() i					
		##gets the value of the inverse
      list(set = set, get = get,	 
            setinverse = setinverse,
            getinverse = getinverse)
		##creates a list		
}


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getinverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setinverse(i)
	i
}

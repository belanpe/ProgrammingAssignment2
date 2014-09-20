# Put comments here that give an overall description of what your
# functions do

# Purpose:
#   R-language script to create a matrix to be inverted
# Synopsis:
# 	amat <- makeCacheMatrix()
# 	amat$set(matrix(c(1,2,3,4),nrow=2,ncol=,byrow=TRUE))
# 	amat$get()
# 	cacheSolve(amat)
#


#  Date   ID		Description
#  091714 pbr		Inception
# 
#


#
#  Define matrix functions:
# 

makeCacheMatrix <- function(x,...) {
	#
	#  Ensure matrix is square:
	m <- NULL
    set <- function(matrix(y)) {
        x <<- y
        m <<- NULL
    }
	get <- function() x
	setinvert <- function(solve) m <<- solve
    getinvert <- function() m
	#  Broadcast results:
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}

#
# Resolve the cached square matrix
#
cacheSolve <- function(x, ...) {
	# Return a matrix that is the inverse of 'x'
	m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    #x$setmean(m)
    x$setinvert(m)
    m
}


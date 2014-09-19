# Put comments here that give an overall description of what your
# functions do

# Purpose:
#   R-language script to create a matrix to be inverted
# Synopsis:
#  makeCacheMatrix(
#
#  cacheSolve(x,...)
#
amat <- matrix(c(1,2,3,4),nrow=2,ncol=,byrow=TRUE)
amat
t(amat)
solve(amat)


#  Date   ID		Description
#  091714 pbr		Inception
# 
#


# Write a short comment describing this function
# 
makeCacheMatrix <- function(x = matrix()) {
	# 
	m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    #setmean <- function(mean) m <<- mean
    #getmean <- function() m
    #list(set = set, get = get,
    #     setmean = setmean,
    #     getmean = getmean)
	setinvert <- function(mean) m <<- mean
    getinvert <- function() m
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


#
# Write a short comment describing this function
#
cacheSolve <- function(x, ...) {
	# Return a matrix that is the inverse of 'x'
	#m <- x$getmean()
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


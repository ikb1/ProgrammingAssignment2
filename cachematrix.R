## Functions to create and cache the inverse of a matrix

## makeCacheMatrix function creates a matrix object to cache its inverse
## set the matrix [set]
## get the matrix [get]
## set the inverse of the matrix [setinv]
## get the inverse of the matrix [getinv]

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) i <<- inverse
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve function returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    # if the inverse has already been computed return the cached value
    if (!is.null(i)) {
        message("Getting cached data")
        return(i)
    }
    # else compute the matrix inverse
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    return(i)
}

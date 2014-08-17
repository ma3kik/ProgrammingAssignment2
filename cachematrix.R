## These functions implements a special data type which enables users
## to cache the inverse matrix of a given matrix.

## The makeCacheMatrix function outputs a matrix which can hold 
## its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setInverse <- function(invMat) iv <<- invMat
        getInverse <- function() iv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## The cacheSolve function sees if a given data produced by 
## the makeCacheMatrix function above has already the inverse matrix
## in its cache. If no, this function computes the inverse matrix
## using solve() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getInverse()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setInverse(iv)
        iv
}

## These are two funtions that compute and cache the inverse of a given matrix.

## This function creates a matrix 
## The inverse is saved (cached) to improve speed in future computations

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function returns the inverse of the matrix genrated by makeCacheMatrix.
## It also verifies if inverse matrix has been calculated
## cacheSolve retrieve the inverse from cache
## set the values in the cache via setinverse function (it issumed that has inversion possible)

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("obtaining cached...")
                return(inv)
        ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}

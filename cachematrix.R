## These are two funtions that compute and cache the inverse of a given matrix.

## This function creates a matrix 
## The inverse is saved (cached) to improve speed in future computations

makeCacheMatrix <- function(mtx = matrix()) {
        inv <- NULL
        set <- function(y) {
                mtx <<- y
                inv <<- NULL
        }
        get <- function() return(mtx)
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() return(inv)
        return(list(set=set, get=get, setinverse=setinverse, getinverse=getinverse))
}


## This function returns the inverse of the matrix genrated by makeCacheMatrix.
## It also verifies if inverse matrix has been calculated
## cacheSolve retrieve the inverse from cache
## set the values in the cache via setinverse function (it issumed that has inversion possible)

cacheSolve <- function(mtx, ...) {
        inv <- mtx$getinverse()
        if(!is.null(inv)) {
                message("Generating cached data ...")
                return(inv)
        ## Return a matrix that is the inverse of 'x'
        }
        data <- mtx$get()
        inv <- solve(data)
        mtx$setinverse(inv)
        return(inv)
}

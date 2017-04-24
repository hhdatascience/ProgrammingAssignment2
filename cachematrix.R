## Below are two functions that are used to create a special 
## object that stores a matrix and cache's its inverse.


## This function creates a special "matrix" object that can cache 
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    mtxinv <- NULL
    set <- function(y) {
        x <<- y
        mtxinv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mtxinv <<- inverse
    getinverse <- function() mtxinv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    mtxinv <- x$getinverse()
    if(!is.null(mtxinv)) {
        message("getting cached data")
        return(mtxinv)
    }
    data <- x$get()
    mtxinv <- solve(data, ...)
    x$setinverse(mtxinv)
    mtxinv
}

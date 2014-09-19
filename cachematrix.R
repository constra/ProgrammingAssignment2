## This function inverses a square invertable matrix and cache the result


## This function passes matrix and stores the inverse
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set <- function(y) {
        x <<- y
        mat <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) mat <<- solve
    getinverse <- function() mat
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates and cache the inverse
## If it is already cached, this function returns the result

cacheSolve <- function(x, ...) {
    mat <- x$getinverse()
    if(!is.null(mat)) {
        message("getting cached inverse")
        return(mat)
    }
    data <- x$get()
    mat <- solve(data, ...)
    x$setinverse(mat)
    mat
}

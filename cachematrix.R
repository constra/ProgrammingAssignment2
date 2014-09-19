## This function inverses a square invertable matrix and cache the result


## This function returns a list, which contain the functions of set the value of
## the matrix, get the value, set the inverse, and finally get the inverse
makeCacheMatrix <- function(x = matrix()) {
    mat <- NULL
    set<- function(y){
        x<<-y
        mat<-NULL
    }
    get <- function() x #get the value of the matrix
    setinverse <- function(solve) mat <<- solve #pass the calculated inverse
    getinverse <- function() mat #get the result
    
    # Return the list
    list(set=set,
        get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates and cache the inverse. If it is already cached, 
## this function returns the result

cacheSolve <- function(x, ...) {
    mat <- x$getinverse() #Call out the stored matrix inverse
    # If the result is already calculated, return the result
    if(!is.null(mat)) {
        message("getting cached inverse")
        return(mat)
    }
    mat <- solve(x$get(), ...) #calculate the inverse
    x$setinverse(mat) #store the result in mat
    mat #return the result
}

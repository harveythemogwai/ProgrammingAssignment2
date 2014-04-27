## Creates a matrix thats can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i  <- NULL
    set  <- function(y){
        x <<- y
        i <<- NULL 
    }
    get  <- function() x
    setInverse  <- function(inverse) i  <<- inverse
    getInverse  <- function() i
    list(set= set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}

## Computes the inverse of the matrix created by the makeCacheMatrix 
## function; if the inverse has been calculated this function will 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
    i  <- x$getInverse()
    if (!is.null(i)){
        return(i)
    }
    data  <- x$get()
    i  <- solve(data, ...)
    x$setInverse(i)
    i
}

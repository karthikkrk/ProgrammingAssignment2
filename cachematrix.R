## Put comments here that give an overall description of what your
## functions do

## This function caches the inverse and returns the cached inverse, if it exists. If the cache does not exist
## then it caches it so for subsequent calls, it can return from the cache
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) im <<- solve
        getinverse <- function() im
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function gets the inverse from the cache if it exists, otherwise it will calculate a new inverse and 
## cache it by calling the setinverse method on teh makecachematrix function. 
## This function takes makeCacheMatrix function as a argument 

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

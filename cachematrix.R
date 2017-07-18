## Put comments here that give an overall description of what your
## functions do

## This is basically a list that contains 4 functions
## set() method sets the matrix that needs to be inverted
## get() method returns the matrix that was set using the set() method
## setinverse() method stores the inverse in the cache
## getinverse() method returns the inverse from the cache

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the 
## cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the inverted matrix 
## in the cache via the setinverse function

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

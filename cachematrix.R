## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix sets and retrieves cached data. The cacheSolve function will 
## check if the inverse is stored, then call the appropriate part of this function.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {   ##This part will set the matrix and its inv as null
                x <<- y
                m <<- NULL
        }
        get <- function() x  ##This part retrieves the matrix
        setinv <- function(inv) m <<- inv  ##This part will set the inverse in the cache
        getinv <- function() m  ## This part retrieves the inverse of the matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function checks if the inverse of the matrix has already solved. If not, 
## the inverse is calculated. Then the inverse is returned and saved if it wasn't
## already. 

cacheSolve <- function(x, ...) {
        m <- x$getinv()  
        if(!is.null(m)) {  ##Here we check if the inverse is already stored.
                message("getting cached data")
                return(m)  ##Since the inverse was not returned as null, it reports the cached inverse
        }
        matrix <- x$get()  ##This only happens if the inverse was reported as null and now we solve for the inverse.
        m <- solve(matrix, ...)
        x$setinv(m)
        m
}
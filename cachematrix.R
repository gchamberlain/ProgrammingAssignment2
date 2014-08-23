## These functions serve to create and cache the inverse of a matrix.
## As inversing a matrix can be time consuming, where possible, it will retrieve it from the cache.
## Authored by G. Chamberlain 201408232110

## The makeCacheMatrix function allows for getting a matrix object and also saving it to cache.

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(solve) m <<- solve
        
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        

}


## The cacheSolve function checks to see if the inverse has been cached. If so it retrieves it.
## Else it calculates the inverse and calls the makeCacheMatrix function to save it to cache. 
## It then returns the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if (!is.null(m)) {
                message("Getting cached inverse matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

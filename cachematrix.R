## Two functions to inverse a matrix
## Provides optimization, caching an inverse matrix, avoid to
## repeatedly re-execute inversion that has a high computational
## cost. 

## Creates an object of matrix class, which can be cached, and methods 
## to get it

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
        x <<- y
        inv <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) inv <<- inverse
        
        getinverse <- function() inv
        
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
  
}


## Inverse a matrix, if it is not in cache or it changed from a previous version,
## cache it and return the brand new inverse version. 
## Otherwise, the cached version is returned.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinverse()
        
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        
        inv <- solve(data, ...)
        
        x$setinverse(inv)
        
        inv
}

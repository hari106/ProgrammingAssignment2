## Put comments here that give an overall description of what your
## functions do

## Creates a cache matrix and returns a list of functions as getters and setters for the matrix & inverse

makeCacheMatrix <- function(x = matrix()) {
    
    ## define the cache m
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL ## re-initialize m in the parent environment to null
    }
    get <- function() x 
    setInverse <- function(inverse) m <<- inverse 
    getInverse <- function() m 
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}
## Retrieves caches inverse else creates and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        return(inverse)
    }
    matrix <- x$get()
    inverse <- solve(matrix)
    x$setInverse(inverse)
    inverse
}


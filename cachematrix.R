## Functions create a list with functions to get and
## set matrix value and inverse, and to cache inverse value

## Creates a "matrix" list with accessors and mutators 
## to get and set matrix value and inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get, 
        setInverse = setInverse,
        getInverse = getInverse)
}


## Attempts to find inverse of "matrix" x
## using cached value from getInverse()
## otherwise calculates matrix inverse 
## and stores in cache using setInverse()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i        
}

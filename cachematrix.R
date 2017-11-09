## The makeCacheMatrix function creates a special "matrix" object 
## that can cache its inverse.
## The cacheSolve function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated, then the 
## cachesolve should retrieve the inverse from the cache.


## Below is the code for makeCacheMatrix function:

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Below is the code for cacheSolve function:

cacheSolve <- function(x, ...) {
	  ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

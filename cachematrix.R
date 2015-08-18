## There are two functions makeCacheMatrix and cacheSolve.
# Idea is to save time calculating same values for same 
# matrices by using already calculated results. 

# The first fuction creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        
        # now returns list with the 4 functions to communicate the cache
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


# The second function cacheSolve checks first if the inverse 
# of the matrix in question was already created. If yes, the 
# function just calls it without recalculating. If the inverse 
# was not yet calculated, the function does it and sets the 
# value into cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        
        # Return inversed matrix if it's available.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # Calculate inversed matrix if it's not available
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## function creates the appropriate data structure for the caching matrix

makeCacheMatrix <- function(x = matrix()) {
        reverse <- NULL
        set <- function(y) {
                x <<- y
                reverse <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) reverse <<- solve
        getsolve <- function() reverse
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)

}


## function inverse the input matrix using the cahcin mechanism

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        reverse <- x$getsolve()
        if(!is.null(reverse)) {
                message("getting cached data")
                return(reverse)
        }
        data <- x$get()
        reverse <- solve(data, ...)
        x$setsolve(reverse)
        reverse
}


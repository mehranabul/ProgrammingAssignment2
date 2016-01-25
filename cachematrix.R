##Coursera - R Programming | Assignment 2
## Caching of Inverse of a Matrix


######Function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        invrs <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) invrs <<- inverse
        getInverse <- function() invrs 
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

#########Function to compute the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invrs <- x$getInverse()
        if (!is.null(invrs)) {
                message("getting cached data")
                return(invrs)
        }
        mat <- x$get()
        invrs <- solve(mat, ...)
        x$setInverse(invrs)
        invrs 
}



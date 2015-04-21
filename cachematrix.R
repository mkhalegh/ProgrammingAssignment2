# Matrix inversion is usually a costly computation and there may be
# some benefit to caching the inverse of a matrix rather than compute it
# repeatedly.Following pair of functions cache the inverse of a matrix.

# The 'makeCacheMatrix' function sets the value of the matrix, gets the value 
# of the matrix, sets the value of inverse of the matrix, and gets the value 
# of inverse of the matrix. They'll be used as the input for cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                # use `<<-` to assign a value to an object in another environment
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   
}


# The 'cacheSolve' function returns the inverse of a given matrix, x. It first 
# checks if the inverse has already been computed. If so, it gets the result 
# and skips the computation. If not, it computes the inverse, sets the value 
# in the cache via the setinverse function. 
# Assumption: the matrix is always invertible.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        # if the inverse has already been calculated
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # otherwise, calculates the inverse
        data <- x$get()
        inv <- solve(data)
        # sets the value of the inverse in the cache via the setinverse function.
        x$setinverse(inv)
        return (inv)        
}

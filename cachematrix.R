## The first function below takes an invertible matrix and performs the
## necessary steps that allow the second function to calculate the inverse
## only if the solution is not already stored in the cache.

## makeCacheMatrix sets the stage for cacheSolve.  It ensures that the
## solution to cacheSolve gets stored in cache so it can be used if the 
## function is called repeatedly.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL  ## resets m to NULL to clear what is set in setinverse
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x  ## returns x when called by cacheSolve
        setinverse <- function(solve) m <<- solve  ## stores inverse in cache
        getinverse <- function() m  ## returns m stored in cache above
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}


## cacheSolve takes the output from makeCacheMatrix (the matrix that was
## originally input) and solves for the inverse only if the solution is
## not already stored in cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()  ## retrieves m from makeCacheMatrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)  ## if m already exists it is returned from cache
        }
        data <- x$get()
        m <- solve(data, diag(nrow(data)), ...)  ## if m is not in cache, it is calculated
        x$setinverse(m)
        m
}
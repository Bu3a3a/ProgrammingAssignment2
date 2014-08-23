##These programs cache matrix inversion 

## The first function, makeVector creates a special "matrix", 
##which is really a list containing a function to
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse of the matrix
##4. get the value of the inverse of the matrix

makeCacheMatrix <- function(m = matrix()) {
        im <- NULL
        set <- function(y) {
                m <<- y
                im <<- NULL
        }
        get <- function() m
        setim <- function(solve) im <<- solve
        getim <- function() im
        list(set = set, get = get,
             setim = setim,
             getim = getim)
}


##The following function calculates the inverse of the special "matrix" 
##created with the above function. However, it first checks to see if the
##inverse has already been calculated. If so, it gets the inverse from the 
##cache and skips the computation. Otherwise, it calculates the inverse of
##the matrix and sets the value of the inverse in the cache via the setim function.

cacheSolve <- function(m, ...) {
        im <- m$getim()
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
        }
        data <- m$get()
        im <- solve(data, ...)
        m$setim(im)
        im
}

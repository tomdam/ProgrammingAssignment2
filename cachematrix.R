## The constructor function for the "cacheMatrix" class
## It takes a single square matrix as a parameter, and caches it's inverse.
## The caller of this function must check if the parameter x is invertible before passing it to this function
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    
    myInv <- list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    class(myInv) <- "cacheMatrix"
    myInv
}


## Helper function, which takes an object that is returned by "makeCacheMatrix" function
## This function returns the inverse of the matrix that has previosly been passed to "makeCacheMatrix" function.
cacheSolve <- function(x, ...) {    
    m <- x$getinverse()
    #check if the inverse had already been calculated
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    
    #This should be enclosed in tryCatch block, 
    #but for this assignment we assume the matrix is invertible
    m <- solve(data, ...)
    
    x$setinverse(m)
    m
}

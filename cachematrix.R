
## this function takes a numeric matrix as input and  
## returns a special vector for use in cacheSolve function
## the special vector is basically a list of solve functions
## stored into variables that part of the parent environment

makeCacheMatrix <- function(x = numeric()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## this function takes makeCacheMatrix as input and
## first checks the parent environment for an existing
## cached result, if found return the cached value, 
## else computer the inverse (solve) function and then
## store the computed value into the parent environment
## for future calls 

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

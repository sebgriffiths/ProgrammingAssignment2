
## This function takes a numeric matrix as input and  
## returns a special vector for use in cacheSolve function
## The special vector is basically a list of solve functions
## stored into variables which get saved into the parent 
## environment (e.g. x <<- y)

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

## This function takes makeCacheMatrix as input and
## first checks the parent environment for an existing
## cached result, if found return the cached value, 
## else computer the inverse (solve) function 
## Then store the computed value into the parent environment
## for future calls 
##
## An example of how to use this:
## z <- matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3) 
## z
##      [,1] [,2] [,3]
## [1,]    1    2    3
## [2,]    0    1    4
## [3,]    5    6    0
## y <- makeCacheMatrix(z)
## cacheSolve(y)
##       [,1] [,2] [,3]
## [1,]  -24   18    5
## [2,]   20  -15   -4
## [3,]   -5    4    1
##
## NB: this example was taken for verification of functionality
## from http://www.purplemath.com/modules/mtrxinvr2.htm  

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

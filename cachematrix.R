## name:   cacheMatrix.R
## author: chouchenn
## date:   10/12/2014
## 
## The functions makeCacheMatrix and cacheSolve are almost
## identical to the makeCacheVector and cacheMean given in the
## assignment's introduction: cacheSolve calls the solve function
## instead of the mean, and I have changed variable names. 
## Call the testCacheMatrix function for a demo.



## inputs: a matrix *x*
##
## stores *x* and caches its inverse *inv*
##
## returns a list of getters/setters for the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## inputs: a makeCacheMatrix object *x*
##
## tests if the cached inverse *inv* is defined;
## if *inv* is defined: returns *inv*; 
## otherwise, retrieves the matrix *mat* contained in *x*,
## and computes its inverse with the solve function
##
## returns: the inverse of the matrix contained in *x*

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <-solve(mat, ...)
    x$setinverse(inv)
    inv
}


## test function for the cached matrix
testCacheMatrix <- function() {
    message("creating test matrix...")
    a <- matrix(1:4, 2, 2)
    print(a)
    message("")

    message("creating makeCacheMatrixObject...")
    c <- makeCacheMatrix(a)
    message("    c$get(): ")
    print(c$get())
    message("    c$getinverse(): ")
    print(c$getinverse())
    message("")

    message("first cacheSolve call...")
    print(cacheSolve(c))
    message("    c$get(): ")
    print(c$get())
    message("    c$getinverse(): ")
    print(c$getinverse())
    message("")

    message("second cacheSolve call...")
    print(cacheSolve(c))
}


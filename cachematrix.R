## A pair of functions that cache the inverse of a matrix
## Created by Ben Gaff for Programming Assignment 2 for R Programming on Coursera

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inversematrix <- NULL
    set <- function(y) {
        x <<- y
        inversematrix <- NULL
    }
    get <- function() x
    setinverse <- function(solve) inversematrix <<- solve
    getinverse <- function() inversematrix
    list(set = set, get = get, 
         setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cacheSolve function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inversematrix <- x$getinverse()
    if(!is.null(inversematrix)) {
        message("getting cached inverse matrix")
        return(inversematrix)    
    } else {
        data <- x$get()
        inversematrix <- solve(data)
        x$setinverse(inversematrix)
        inversematrix
    }
}
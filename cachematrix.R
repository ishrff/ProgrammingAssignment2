## Programming Assignment 2 - R programming

## -------------------------------
## Caching the inverse of a matrix
## -------------------------------

## The following two functions caches and computes the inverse of a matrix

## ------------------------
## Function makeCacheMatrix
## ------------------------
## This function creates a matrix that can be input to the cacheSolve() function

makeCacheMatrix <- function(mtx = matrix()) {
    inverse <- NULL
    set <- function(x) {
        mtx <<- x;
        inverse <<- NULL;
    }
    get <- function() return(mtx);
    setinv <- function(inv) inverse <<- inv;
    getinv <- function() return(inverse);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## --------------------
## Function cacheSolve
## --------------------
## This function computes the inverse of the matrix returned by the other function makeCacheMatrix()
## If the inverse has been previously calculated and there's no change in the matrix 
## then in this case the cacheSolve() returns the cached inverse

cacheSolve <- function(mtx, ...) {
    inverse <- mtx$getinv()
    if(!is.null(inverse)) {
        return(inverse)
    }
    data <- mtx$get()
    invserse <- solve(data, ...)
    mtx$setinv(inverse)
    return(inverse)
}

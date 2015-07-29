## Light Chen
## July.25
## Coursera R programming Assignment 2 Caching the Inverse of a Matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}
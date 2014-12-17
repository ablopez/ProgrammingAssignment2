
##  ASSIGNMENT 2: INVERSE OF A MATRIX

## The following two functions are used to cache the inverse of a matrix

## 'makeCacheMatrix'  creates a 'matrix' object that cache its inverse

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## 'cacheSolve' returns the inverse of the matrix returned by 
## makeCacheMatrix. If the inverse has already been computed then
## 'cashSolve' retrieves the inverse matrix from the cache

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}

## TEST CASE 1
##
## x = rbind(c(4,3), c(3, 2))
## m = makeCacheMatrix(x)
## m$get()

##       [,1]  [,2]
## [1,]  4       3
## [2,]  3       2

## No cache in the first run
## cacheSolve(m)
##           [,1]      [,2]
## [1,]      -2         3
## [2,]      3         -4


## TEST CASE 2 (Prerequisite is Test Case 1)
## Type cacheSolve(m) again and the message "getting cached data" is
## displayed while the calculation is performed:
## cacheSolve(m)
## Getting cached data.
##
## [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4


## TEST CASE 3
## This test proves that 'cacheSolved' is the Inverse Matrix, run TC1 and TC2 first:
## y = cacheSolve (m)
## x %*% y
##           [,1]      [,2]
## [1,]      1          0
## [2,]      0          1






 
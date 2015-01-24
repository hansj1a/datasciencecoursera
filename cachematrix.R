## The following function "makeCacheMAtrix" creates a matrix 
## It consists of a list of functions:
## a function "set" to set the values of the elements of a matrix
## a function "get" to get the values of the elements of a mytrix
## a function "setinv" to set the values of the elements of the inverse matrix
## a function "fetinv" to get the values of the elements of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL     
        set <- function(y) {
                x <<- y
                inv <<- NULL}
        get <- function() x
        setinv <- function(invers) inv <<- invers
        getinv <- function() inv
        list(set = set, get = get,setinv = setinv,getinv = getinv)
}

## The following function "cacheSolve" generates the inverse of the matrix
## which was created by the above function makeCacheMAtrix"
## The generation of the inverse matrix is either done by calculation 
## using the function solve() or -if the result already calculatd before- 
## by taking the existing result from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

## In order to run the calculation the following steps have to be done:
## 1. define a matrix using the function "makeCacheMatrix()", e.g.:
##              myMatrix<-makeCacheMatrix(matrix(c(1,2,3,4),2,2))
## 2. calculate the inverse matrix using the function "cacheSolve(), e.g.:
##              cacheSolve(myMatrix)

## An example of the output is given here:
## The example shows the calculation of a matrix:
##      > matrix(c(1,2,3,4),2,2)
##              [,1] [,2]
##      [1,]    1    3
##      [2,]    2    4
##  1. definition of the matrix using the function "makeCacheMatrix()
##      > myMatrix<-makeCacheMatrix(matrix(c(1,2,3,4),2,2))
##  2. calculation of the inverse matrix:
##      > cacheSolve(myMatrix)
##              [,1] [,2]
##      [1,]   -2  1.5
##      [2,]    1 -0.5
##  Repetition of the calculation leads to:
##     > cacheSolve(myMatrix)
##              getting cached data
##                      [,1] [,2]
##                [1,]   -2  1.5
##                [2,]    1 -0.5
## Put comments here that give an overall description of what your
## functions do

## x is the matrix in the makeCacheMatix funtion
## makeCacheMatrix return a list of four functions that cache the previously computed inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
## set function will pass the value to x, but do not compute its inverse
        x <<- y
## use <<- to assign x the value in an environment different from the current environment
        inv <<- NULL
    }
    
    get <- function() x
## get function return the value of the x matrix
 
    setInverse <- function(inverse) inv <<- inverse
## setInverse function will pass the inverse value to x

    getInverse <- function() inv
## getInverse function return the inverse value of the x matrix    
    list(set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
## make a list inside the function, thus x can retrieve each individual function inside the makeCacheMatrix.

}



## cacheSolve function will solve the inverse of matrix x, and return the value to makeCacheMatrix function

cacheSolve <- function(x, ...) {
 
    inv <- x$getInverse()
## call the getInverse function and pass the value to inv, if the inverse is already there, print the message and return the value.
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    mat <- x$get()
## pass the value to mat

    inv <- solve(mat, ...)
## compute the inverse of x(mat)

    x$setInverse(inv)
## pass inverse value to x's inverse

    inv
## return the inverse value of x
}

## example and solution
> my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(my_matrix)
getting cached data
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$getInverse()
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
> my_matrix$get()
     [,1] [,2]
[1,]    2    1
[2,]    2    4
> my_matrix$getInverse()
NULL
> cacheSolve(my_matrix)
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> cacheSolve(my_matrix)
getting cached data
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333
> my_matrix$getInverse()
           [,1]       [,2]
[1,]  0.6666667 -0.1666667
[2,] -0.3333333  0.3333333

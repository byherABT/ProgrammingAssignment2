##--------------------------------------------------------------------------------------- 
## Programming Assignment 2: Lexical Scoping
## This assignment is to write a pair of functions that cache the inverse of a matrix.
## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its 
## inverse.
## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.
##---------------------------------------------------------------------------------------
##---------------------------------------------------------------------------------------
## Function Name:makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse
## Arguments:
## x: a square numeric or complex matrix, which is invertible
## Return:
## a list containing four functions: set(), get(), setInvMatrix(), getInvMatrix()
##---------------------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    # Internal function, checkNonSquareMatrix: 
    # check if the input data is a matrix, and is a square matrix
    checkNonSquareMatrix <- function(z) {
        if (!is.matrix(z)) {
            stop("the input argument is not a matrix!")
        }
        if (dim(z)[1] != dim(z)[2]) {
            stop("the input argument is not a square matrix!")
        }
    }
    checkNonSquareMatrix(x) # check the input data, x
    invM <- NULL # initialize invM to NULL
    # Function set: set the value of the matrix, to be included in the output list
    set <- function(y) {                  
        checkNonSquareMatrix(y) # check the input data, y
        x <<- y # assign y to x in the makeCacheMatrix() environment
        invM <<- NULL # initialize invM to NULL, invM is in the makeCacheMatrix() environment
    }
    # Function get: get the value of the matrix, to be included in the output list
    get <- function() x
    # Function setInvMatrix: set the value of the inverse matrix, to be included in the output list
    setInvMatrix <- function(invMatrix) {
        checkNonSquareMatrix(invMatrix) # check the input matrix, invMatrix
        # check the dimension of the input matrix, invMatrix
        if ((dim(x)[1] != dim(invMatrix)[1]) | (dim(x)[2] != dim(invMatrix)[2])) { 
            stop("dimensions of the inverse matrix and the original matrix do not match!")
        }
        invM <<- invMatrix # assign invMatrix to invM, in the makeCacheMatrix() environment
    }
    # Function getInvMatrix: get the value of the inverse matrix, to be included in the output list
    getInvMatrix <- function() invM
    list(set = set, get = get,    # create an output list
         setInvMatrix = setInvMatrix,
         getInvMatrix = getInvMatrix)
}

##---------------------------------------------------------------------------------------
## Function Name:cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## Note: For this assignment, assume that the matrix supplied is always invertible
## Arguments:
## x: a special "matrix" object, created by makeCacheMatrix(), that can cache its inverse
## Return:
## the inverse matrix
##---------------------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    if(!is.list(x)) { # check if the input data is a list
        stop("the input data is not a list!")
    }
    invM <- x$getInvMatrix() # get the value of the inverse matrix
    if(!is.null(invM)) { # check if the inverse matrix is cached
        message("getting cached data") # display a message if getting the cached inverse matrix
        return(invM) # return the cached inverse matrix
    }
    # The inverse matrix is not cached, so we need to calculate it
    data <- x$get() # get the value of the input matrix
    # For this assignment, assume that the matrix supplied is always invertible
    invM <- solve(data, ...) # calculate the inverse matrix
    x$setInvMatrix(invM) # set the value of the inverse matrix
    invM # return the inverse matrix
}
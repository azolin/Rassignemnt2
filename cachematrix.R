## Ranko Gacesa, 16/04/2014

## - cachematrix.R documentation -------------------------
## functions:
## makeCacheMatrix:
## -> generates matrix pseudo object with cachable invert
## cacheSolve: 
## -> retrieves cachable invert (or generates it if not calculated yet)
## for matrix made by makeCacheMatrix function

## example use:
## M <- makeCacheMatrix(matrix(runif(25),5,5))
##    M$get()      # gets values of M
## cacheSolve(M)   # gets invert

## NOTES: 
# -> matrix should be invertible or code will crash!
# -> M$set() should never be used after calling cacheSolve; 
# create new matrix instead by using makeCacheMatrix!
# -> M$get() is safe to use
# -> M$getinvert should never be called directly, it is internal function
# called by cacheSolve(); call cacheSolve(M) instead!      
# -> M$setinvert should never be called directly

## --------------------------------------------------------
## makeCacheMatrix: 
## - creates cachable matrix "pseudo-object"
## containing getter/setter and getter/setter for cached inverse

## USE: 
## -> makeCacheMatrix(<matrix>) to create; 
## ex: M <- makeCacheMatrix(matrix(runif(25,0,10),5,5))
## -> call <matrix>$get to get; ex: M$get()
## -> call <matrix>$set to set values; ex: M$set(matrix(1:9,3,3))
## -> setinvert and getinvert should NOT be called directly
## use cacheSolve instead
## --------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # set value of matrix
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # get value of matrix
    get <- function() {x}
    # set invert of matrix
    setinvert <- function(solve) {m <<- solve}
    # get invert of matrix
    # note: this function should not be called directly - call cacheSolve!
    getinvert <- function() {m}
    # return construct
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)        
}

## --------------------------------------------------------
## cacheSolve:
## returns stored inverse of matrix if it exists
## otherwise calls setinvert to generate one
## --------------------------------------------------------
cacheSolve <- function(x, ...) {
    # Return a matrix that is the inverse of 'x'
    m <- x$getinvert()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}

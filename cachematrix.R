## Put comments here that give an overall description of what your
## functions do

## This functions returns a list of functions that:
##  set value of matrix
##  get the value of a matrix
##  set the value of the inverse of the matrix
##  get the value of the inverse of a matrix 
makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverseMatrix) Inv <<- inverseMatrix
    getInverse <- function() Inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}

## This function calculates the inverse of output of the makeCacheMatrix function
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
    Inv <-x$getInverse()
    if(!is.null(Inv)){
        message("getting cached data")
        return (Inv)
    }
    matData<-x$get()
    Inv<-solve(matData)
    x$setInverse(Inv)
    Inv

}

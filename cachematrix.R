## 2 Functions, one that creates a special matrix object that cache the inverse
# of the matrix, the last one calculates the inverse of matrix and checks if the
#Inverse has been already calculated and return the cached inverse.

## this function creates the special matrix object that can cache the inverse.

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y){
        x <<- y
        Inv <<- NULL
    }
    #Get the value of a matrix
    get <- function() x
    
    #set the value of inverse
    setInverse <- function(inverse) (Inv <<- inverse)
    
    #get the value of inverse
    getInverse <- function(){Inv}
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## This function compute the inverse of the returned matrix and retrieve the
# inverse from the cache(if it has been already calculated)

cacheSolve <- function(x, ...) {
    Inv <- x$getInverse()
    
    #Check if the inverse has been calculated
    if(!is.null(Inv)){
        message("Getting cached data")
        return(Inv)
    }
    matx <- x$get()
    
    #calculate the inverse of matrix using solve
    Inv <- solve(matx, ...)
    x$setInverse(Inv)
    Inv
        
}

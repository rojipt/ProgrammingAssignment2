## This file includes the implentation of the below functions
## 1. makeCacheMatrix
## 2. cacheSolve


## The below function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The function produces list containing a function each to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()){
  
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(i) inv <<- i
        
        getinv <- function() inv
                
        list(set = set, get = get,
                setinv = setinv,
                getinv = getinv)
}  


## This function computes the inverse of the special "matrix" returned by  makeCacheMatrix  above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then  cacheSolve  will  retrieve the inverse from the cache.
## If the inverse has never been calculated, this function will calculate
## it and place it in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' if it is already calculated.
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## Otherwise calculate the inverse using solve(), place the inverse in cache
        ## and return the inverse to the caller.
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}

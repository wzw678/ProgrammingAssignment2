## these functions serve as the answer to Coursera R Programing
## Project 2


## this function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set=set, get=get,
         setinv=setinv, 
         getinv=getinv)

}


## this function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (
## and matrix has not changed), then cacheSolve retrieves the
## inverse from the cache.
## note that the x here isn't a regular matrix but a list following 
## the format of the list returned by makeCaheMatrix. 

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <-solve(data, ...)
    x$setinv(inv)
    inv
}
    
        ## Return a matrix that is the inverse of 'x'

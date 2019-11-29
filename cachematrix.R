## Put comments here that give an overall description of what your
## functions do

## The following function is to set the inverse of the special "matrix".

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function is to calculate the inverse of matrix created by 
## the function makeCacheMatrix ()
cacheSolve <- function(x, ...) {
        inv<- x$getinverse(
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get(),
        inv <- mean(data, ...),
        x$setmean(inv),
        inv
        )
}


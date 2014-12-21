## Put comments here that give an overall description of what your
## functions do


## makeCacheMatrix creates special matrix object which can cache its inverse
##

makeCacheMatrix <- function(x = numeric()) {
    inv <- NULL
    set <- function(y){
        x <<- y          #x contains previously inputted matrices
        inv <<- NULL     #inv contains inverse matrices corresponds to x
    }
    get <- function() x  #returns matrix
    setInverse <- function(solve) inv <<- solve  #set inverse matrix
    getInverse <- function() inv                 #get inverse matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes the inverse of the special matrix returned by makeCacheMatrix
## If the inverse has previously calculated, then cache can retrieve the inverse from its cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()              #try to get inverse matrix from cache
        if(!is.null(inv)){                 #if the inverse has been calculated,
            message("getting cached data") #it outputs message and return cached value
            return(inv)
        }
        data <- x$get()                    #if not, input matrix and its inverse are
        inv <- solve(data, ...)            #cached after calculation
        x$setInverse(inv)
        inv
}

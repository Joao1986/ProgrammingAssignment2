## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The function makeCacheMatrix create a empty object to cache an inverse of a 
#matrix,in the end create a list of four different values, set and get the 
#matrix and the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## Write a short comment describing this function
#it's use the result of the computation of makeCacheMatrix, first check if 
#makeCacheMatrix was done and return the inverse matrix, if it's not computed
#the function compute the inverse matrix of x

cacheSolve <- function(x, ...) {
           ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}



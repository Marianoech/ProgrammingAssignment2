## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {   # set the value of the matrix
                x <<- y
                inver <<- NULL
        }
        get <- function() x  # get the value of the matrix
        setinverse <- function(inverse) inver <<- inverse   # set the value of the inverse
        getinverse <- function() inver   # get the value of the inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inver <- x$getInverse()
        if (!is.null(inver)) {
        message("getting cached data")
        return(inver)
        }
        matriz <- x$get()
        inver <- solve(matriz, ...)
        x$setInverse(inver)
        inver
}

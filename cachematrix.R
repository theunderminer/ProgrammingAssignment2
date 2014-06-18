## With these functions it is possible to create a matrix object that is able
## to calculate the inverse of the matrix and store it in cache

## This function creates matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # variable i stores the cached inverse of the matrix
    i <- NULL
    # set function stores a given matrix y as matrix and clears the cached inverse value i
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # get function returns the matrix
    get <- function() x
    # setinverse stores the given inverse as inverse of the matrix
    setinverse <- function(inverse) i <<- inverse
    # getinverse returns the cached inverse value
    getinverse <- function() i
    # makeCacheMatrix returns a list object with references to the functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of a matrix created with makeCacheMatrix
cacheSolve <- function(x, ...) {
    # try to obtain the inverse of the matrix from the cache
    i <- x$getinverse()
    # if the inverse is stored in the cache, return the cached inverse
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # if the inverse is not stored in cache, get the value of the matrix
    data <- x$get()
    # calculate the inverse of the matrix
    m <- solve(data, ...)
    # store the inverse of the matrix in the cache
    x$setinverse(i)
    # return the calculated inverse of the matrix 
    i
}

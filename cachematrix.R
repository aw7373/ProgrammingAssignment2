## The makeCacheMatrix function takes a matrix and provides a list of functions
## to manipulate it. THe cacheSolve function takes the result of the
## makeCacheMatrix and returns the inverse of the inital matrix.

## This function takes a matrix as the parameter and stores it in cache and
## returns a list of functions to set and get the value of the matrix as well as
## set and get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function takes the list of functions from the makeCacheMatrix function
## as a parameter and first checks if the inverse of the initial matrix is
## stored in cache and returns it if it was found otherwise it calculates the
## inverse of the initial matrix and returns the result

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}

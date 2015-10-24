## cacheSolve() calculates the inverse of a matrix.
## functions CacheSolve() and makeCacheMatrix() work as a combination as
## the results of function makeCacheMatrix() are used to feed into 
## funtion cacheSolve().
## If the inverse of a matrix is already calculated, cacheSolve() function
## gets the inverse from cache, otherwise it calculates the inverse of a matrix

## function makeCacheMatrix() accepts a matrix and returns a list of functions
## set(), get(), set_inverse() and get_inverse()

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
        
        x <<- y        # <<- is used to set the variable which is outside this function      
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}  


## function cacheSolve() is used to calculate inverse of a matrix
## it uses output of function makeCacheMatrix() and 
## returns an inverse of matrix passed to the function makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # otherwise get and calculate inverse of matrix
    myMatrix <- x$get()
    inv <- solve(myMatrix, ...)
    
    x$set_inverse(inv)
    return(inv)
}


## creates a pseudo 'matrix' with the input matrix, its inverse and get and set 
## methods to get and set the matrix and its inverse

## create 'matrix' and methods
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, 
         get = get,
         setinv = setinv,
         getinv = getinv
    )
    
}

## create and return inverse of matrix (if it doesn't already exist in cache) or return cached inverse
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

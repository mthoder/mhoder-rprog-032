## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix;
## 2. get the value of the matrix;
## 3. set the value of the inverse matrix;
## 4. get the value of the inverse matrix.
## This list is used as the input to cacheSolve().

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns the inverse of the matrix that is input into makeCacheMatrix().

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        mat.data <- x$get()
        inv <- solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}
